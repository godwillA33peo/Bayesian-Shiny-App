priorPredictiveServer <- function(id, library) {
  moduleServer(id, function(input, output, session) {

    fam_key <- reactiveVal(NULL)
    observe(fam_key(input$family))

    int_defaults <- list(bernoulli = pl_prior("normal", 0, 2.5), poisson = pl_prior("normal", 1, 1),
                         gaussian = pl_prior("normal", 60, 20))
    slope_defaults <- list(bernoulli = pl_prior("normal", 0, 2.5), poisson = pl_prior("normal", 0, 1),
                           gaussian = pl_prior("normal", 0, 10))
    reset_int <- reactive({ k <- fam_key(); req(k); list(supports = "real", prior = int_defaults[[k]]) })
    reset_slope <- reactive({ k <- fam_key(); req(k); list(supports = "real", prior = slope_defaults[[k]]) })

    p_int <- priorPickerServer("int", supports = "real", library = library, reset = reset_int)
    p_slope <- priorPickerServer("slope", supports = "real", library = library, reset = reset_slope)
    p_sigma <- priorPickerServer("sigma", init = pl_prior("exponential", 0.1), supports = "positive", library = library)

    sim <- reactive({
      fam <- input$family
      K <- input$K
      pi_ <- p_int$prior(); pb <- p_slope$prior()
      ps <- if (fam == "gaussian") p_sigma$prior() else NULL
      nd <- as.integer(input$ndraw)
      set.seed(4242 + input$redraw)
      pl_sim_regression(fam, K, pi_, pb, ps, ndraw = nd)
    })

    edge <- reactive(input$p_edge)

    outside <- reactive({
      s <- sim()
      switch(s$family,
        bernoulli = mean(s$mu < edge() | s$mu > 1 - edge()),
        poisson = mean(s$mu > input$rate_max),
        gaussian = mean(s$mu < input$y_min | s$mu > input$y_max)
      )
    })

    output$tiles <- renderUI({
      s <- sim()
      tiles <- list(pl_tile(pl_fmt(s$eta_sd, 3), "SD of the linear predictor \u{03b7}"))
      if (s$family == "bernoulli") {
        tiles <- c(tiles, list(
          pl_tile(pl_pct(outside()), sprintf("of individuals have p < %s or p > %s", pl_fmt(edge()), pl_fmt(1 - edge()))),
          pl_tile(pl_pct(mean(s$dataset_stat == 0 | s$dataset_stat == 1)), "of simulated data sets are all 0s or all 1s")
        ))
      } else if (s$family == "poisson") {
        tiles <- c(tiles, list(
          pl_tile(pl_pct(outside()), sprintf("of individuals have a mean count above %s", pl_fmt(input$rate_max))),
          pl_tile(pl_fmt(median(s$mu), 3), "median expected count for an individual")
        ))
      } else {
        tiles <- c(tiles, list(
          pl_tile(pl_pct(outside()), sprintf("of individual means fall outside [%s, %s]", pl_fmt(input$y_min), pl_fmt(input$y_max))),
          pl_tile(pl_pct(mean(s$y < input$y_min | s$y > input$y_max)), "of simulated outcomes y fall outside that range")
        ))
      }
      div(class = "pl-tiles", tiles)
    })

    clamp_q <- function(z, lo = 0.005, hi = 0.995) {
      z <- z[is.finite(z)]
      q <- quantile(z, c(lo, hi), names = FALSE)
      z[z >= q[1] & z <= q[2]]
    }

    output$mu_plot <- renderPlot({
      s <- sim()
      ttl <- "Implied mean outcome for a random individual"
      if (s$family == "bernoulli") {
        ggplot(data.frame(z = s$mu), aes(x = z)) +
          annotate("rect", xmin = c(0, 1 - edge()), xmax = c(edge(), 1), ymin = -Inf, ymax = Inf,
                   fill = "#e34948", alpha = 0.10) +
          geom_histogram(breaks = seq(0, 1, by = 0.02), fill = PL_COLS[["Prior"]], colour = "white", linewidth = 0.2) +
          labs(x = "Probability that y = 1", y = "Prior draws", title = ttl,
               subtitle = "Shaded: extreme probabilities") +
          pl_theme(13)
      } else if (s$family == "poisson") {
        z <- pmin(pmax(s$mu[is.finite(s$mu)], 1e-3), 1e6)
        ggplot(data.frame(z = z), aes(x = z)) +
          geom_histogram(bins = 50, fill = PL_COLS[["Prior"]], colour = "white", linewidth = 0.2) +
          geom_vline(xintercept = input$rate_max, colour = "#e34948", linetype = "dashed", linewidth = 0.8) +
          scale_x_log10(labels = function(b) pl_fmt(b, 3)) +
          labs(x = "Expected count (log axis)", y = "Prior draws", title = ttl,
               subtitle = "Dashed: highest plausible mean count") +
          pl_theme(13)
      } else {
        ggplot(data.frame(z = clamp_q(s$mu)), aes(x = z)) +
          annotate("rect", xmin = input$y_min, xmax = input$y_max, ymin = -Inf, ymax = Inf,
                   fill = PL_COLS[["Likelihood"]], alpha = 0.10) +
          geom_histogram(bins = 50, fill = PL_COLS[["Prior"]], colour = "white", linewidth = 0.2) +
          labs(x = "Expected y", y = "Prior draws", title = ttl,
               subtitle = "Shaded: plausible range (central 99% of draws shown)") +
          pl_theme(13)
      }
    })

    output$lines_plot <- renderPlot({
      s <- sim()
      validate(need(s$K > 0, "Add at least one predictor to see implied curves."))
      ln <- s$lines
      p <- ggplot(ln, aes(x = x, y = mu, group = draw)) +
        geom_line(colour = PL_COLS[["Prior"]], alpha = 0.25, linewidth = 0.5) +
        labs(x = "x\u{2081} (standardised; other predictors at 0)", title = "100 prior draws of the curve") +
        pl_theme(13)
      if (s$family == "bernoulli") {
        p + coord_cartesian(ylim = c(0, 1)) + labs(y = "P(y = 1)")
      } else if (s$family == "poisson") {
        ln$mu <- pmin(pmax(ln$mu, 1e-3), 1e6)
        p %+% ln + scale_y_log10(labels = function(b) pl_fmt(b, 3)) + labs(y = "Expected count (log axis)")
      } else {
        rng <- quantile(ln$mu, c(0.01, 0.99), names = FALSE)
        rng <- range(c(rng, input$y_min, input$y_max))
        p + annotate("rect", xmin = -Inf, xmax = Inf, ymin = input$y_min, ymax = input$y_max,
                     fill = PL_COLS[["Likelihood"]], alpha = 0.10) +
          coord_cartesian(ylim = rng) + labs(y = "Expected y")
      }
    })

    output$dataset_plot <- renderPlot({
      s <- sim()
      z <- s$dataset_stat
      p <- ggplot(data.frame(z = if (s$family == "gaussian") clamp_q(z) else z[is.finite(z)]), aes(x = z)) +
        labs(x = s$dataset_label, y = "Simulated data sets", title = "Whole simulated data sets") +
        pl_theme(13)
      if (s$family == "bernoulli") {
        p + geom_histogram(breaks = seq(0, 1, by = 0.02), fill = PL_COLS[["Prior"]], colour = "white", linewidth = 0.2)
      } else if (s$family == "poisson") {
        p %+% data.frame(z = pmin(pmax(z, 1e-3), 1e6)) +
          geom_histogram(bins = 50, fill = PL_COLS[["Prior"]], colour = "white", linewidth = 0.2) +
          scale_x_log10(labels = function(b) pl_fmt(b, 3))
      } else {
        p + annotate("rect", xmin = input$y_min, xmax = input$y_max, ymin = -Inf, ymax = Inf,
                     fill = PL_COLS[["Likelihood"]], alpha = 0.10) +
          geom_histogram(bins = 50, fill = PL_COLS[["Prior"]], colour = "white", linewidth = 0.2)
      }
    })

    output$coef_plot <- renderPlot({
      s <- sim()
      ggplot(data.frame(z = clamp_q(s$eta)), aes(x = z)) +
        geom_histogram(bins = 50, fill = PL_COLS[["Prior"]], colour = "white", linewidth = 0.2) +
        labs(x = switch(s$family, bernoulli = "\u{03b7} (log-odds)", poisson = "\u{03b7} (log expected count)", gaussian = "\u{03b7} (expected y)"),
             y = "Prior draws",
             title = "Linear predictor \u{03b7} = \u{03b2}\u{2080} + \u{03a3} \u{03b2}\u{2096}x\u{2096}",
             subtitle = sprintf("SD = %s with K = %d predictors (central 99%% shown)", pl_fmt(s$eta_sd, 3), s$K)) +
        pl_theme(13)
    })

    output$code <- renderText({
      fam <- input$family
      K <- input$K
      pi_ <- p_int$prior(); pb <- p_slope$prior()
      lines <- c(sprintf("  prior(%s, class = \"Intercept\")", pl_stan_string(pi_)))
      if (K > 0) lines <- c(lines, sprintf("  prior(%s, class = \"b\")", pl_stan_string(pb)))
      if (fam == "gaussian") lines <- c(lines, sprintf("  prior(%s, class = \"sigma\")", pl_stan_string(p_sigma$prior())))
      rhs <- if (K > 0) paste(paste0("x", seq_len(min(K, 5))), collapse = " + ") else "1"
      if (K > 5) rhs <- paste0(rhs, " + ... + x", K)
      fam_call <- switch(fam, bernoulli = "bernoulli()", poisson = "poisson()", gaussian = "gaussian()")
      paste0(
        "library(brms)\n",
        "priors <- c(\n", paste(lines, collapse = ",\n"), "\n)\n\n",
        "# brms centres predictors, so the Intercept prior is for an average individual.\n",
        "# Standardise predictors first (scale()) so a slope prior means 'per 1 SD'.\n",
        "fit_prior <- brm(y ~ ", rhs, ",\n",
        "                 data = d, family = ", fam_call, ",\n",
        "                 prior = priors, sample_prior = \"only\")\n",
        "pp_check(fit_prior, ndraws = 100)"
      )
    })
  })
}
