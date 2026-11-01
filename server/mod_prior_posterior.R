# Stan code for the single-parameter models in the Prior -> Posterior tab
pl_stan_model_code <- function(lik, mix) {
  par <- switch(paste(lik$type, lik$scale),
                "binomial natural" = "theta", "binomial link" = "eta",
                "poisson natural" = "lambda", "poisson link" = "eta", "normal natural" = "mu")
  prior_stmt <- if (length(mix$components) == 1) {
    paste0("  ", pl_stan_prior_stmt(mix$components[[1]], par))
  } else {
    sprintf("  target += log_mix(%s,\n                    %s,\n                    %s);",
            pl_fmt(mix$weights[1]), pl_stan_lpdf_expr(mix$components[[1]], par),
            pl_stan_lpdf_expr(mix$components[[2]], par))
  }
  switch(paste(lik$type, lik$scale),
    "binomial natural" = paste0(
      "data {\n  int<lower=0> n;               // ", lik$n, "\n  int<lower=0, upper=n> k;      // ", lik$obs, "\n}\n",
      "parameters {\n  real<lower=0, upper=1> theta;\n}\nmodel {\n", prior_stmt, "\n  k ~ binomial(n, theta);\n}"),
    "binomial link" = paste0(
      "data {\n  int<lower=0> n;               // ", lik$n, "\n  int<lower=0, upper=n> k;      // ", lik$obs, "\n}\n",
      "parameters {\n  real eta;                     // log-odds\n}\nmodel {\n", prior_stmt,
      "\n  k ~ binomial_logit(n, eta);\n}\ngenerated quantities {\n  real theta = inv_logit(eta);\n}"),
    "poisson natural" = paste0(
      "data {\n  int<lower=0> y;               // ", lik$obs, "\n  real<lower=0> E;              // ", pl_fmt(lik$E), "\n}\n",
      "parameters {\n  real<lower=0> lambda;\n}\nmodel {\n", prior_stmt, "\n  y ~ poisson(E * lambda);\n}"),
    "poisson link" = paste0(
      "data {\n  int<lower=0> y;               // ", lik$obs, "\n  real<lower=0> E;              // ", pl_fmt(lik$E), "\n}\n",
      "parameters {\n  real eta;                     // log-rate\n}\nmodel {\n", prior_stmt,
      "\n  y ~ poisson_log(log(E) + eta);\n}\ngenerated quantities {\n  real lambda = exp(eta);\n}"),
    "normal natural" = paste0(
      "data {\n  int<lower=1> n;               // ", lik$n, "\n  real ybar;                    // ", pl_fmt(lik$obs),
      "\n  real<lower=0> sigma;          // ", pl_fmt(lik$sigma), " (known)\n}\n",
      "parameters {\n  real mu;\n}\nmodel {\n", prior_stmt, "\n  ybar ~ normal(mu, sigma / sqrt(n));\n}")
  )
}

pl_brms_model_code <- function(lik, mix) {
  if (length(mix$components) > 1) {
    return("# brms has no built-in mixture priors. Use the Stan code above,\n# or the RBesT package, which is built for robust mixture priors.")
  }
  pr <- pl_stan_string(mix$components[[1]])
  switch(paste(lik$type, lik$scale),
    "binomial link" = sprintf(
      "fit <- brm(k | trials(n) ~ 1,\n           data = data.frame(k = %s, n = %s),\n           family = binomial(),\n           prior = prior(%s, class = \"Intercept\"))",
      lik$obs, lik$n, pr),
    "poisson link" = sprintf(
      "fit <- brm(y ~ 1 + offset(log(E)),\n           data = data.frame(y = %s, E = %s),\n           family = poisson(),\n           prior = prior(%s, class = \"Intercept\"))",
      lik$obs, pl_fmt(lik$E), pr),
    "normal natural" = sprintf(
      "# with your raw data in column y of data frame d\nfit <- brm(y ~ 1, data = d, family = gaussian(),\n           prior = c(prior(%s, class = \"Intercept\"),\n                     prior(constant(%s), class = \"sigma\")))  # known sigma",
      pr, pl_fmt(lik$sigma)),
    "# brms puts priors on the link scale (log-odds / log-rate).\n# Switch \u{2018}Put the prior on\u{2019} to the link scale for brms code, or use the Stan code above."
  )
}

priorPosteriorServer <- function(id, library) {
  moduleServer(id, function(input, output, session) {

    scale_now <- reactive({
      switch(input$lik, binomial = input$b_scale, poisson = input$p_scale, normal = "natural")
    })
    key <- reactiveVal(NULL)
    observe(key(paste(input$lik, scale_now())))

    supports <- c("binomial natural" = "unit", "binomial link" = "real",
                  "poisson natural" = "positive", "poisson link" = "real",
                  "normal natural" = "real")
    main_defaults <- list(
      "binomial natural" = pl_prior("beta", 2, 2),
      "binomial link" = pl_prior("normal", 0, 1.5),
      "poisson natural" = pl_prior("gamma", 2, 0.5),
      "poisson link" = pl_prior("normal", 1, 1),
      "normal natural" = pl_prior("normal", 40, 15)
    )
    vague_defaults <- list(
      "binomial natural" = pl_prior("beta", 1, 1),
      "binomial link" = pl_prior("normal", 0, 2.5),
      "poisson natural" = pl_prior("gamma", 1, 0.05),
      "poisson link" = pl_prior("normal", 1, 3),
      "normal natural" = pl_prior("normal", 40, 150)
    )
    reset_main <- reactive({
      k <- key(); req(k)
      list(supports = supports[[k]], prior = main_defaults[[k]])
    })
    reset_vague <- reactive({
      k <- key(); req(k)
      list(supports = supports[[k]], prior = vague_defaults[[k]])
    })
    main <- priorPickerServer("prior", supports = "unit", library = library, reset = reset_main)
    vague <- priorPickerServer("vague", supports = "unit", library = library, reset = reset_vague)

    observeEvent(input$lik, {
      updateNumericInput(session, "threshold",
                         value = switch(input$lik, binomial = 0.5, poisson = 3, normal = 50))
    }, ignoreInit = TRUE)

    is_whole <- function(x) is.finite(x) && abs(x - round(x)) < 1e-9

    lik <- reactive({
      switch(input$lik,
        binomial = {
          n <- input$b_n; k <- input$b_k
          validate(need(is_whole(n) && n >= 0, "n must be a whole number \u{2265} 0."),
                   need(is_whole(k) && k >= 0 && k <= n, "k must be a whole number between 0 and n."))
          pl_lik_spec("binomial", list(n = n, k = k), input$b_scale)
        },
        poisson = {
          y <- input$p_y; E <- input$p_E
          validate(need(is_whole(y) && y >= 0, "y must be a whole number \u{2265} 0."),
                   need(is.finite(E) && E > 0, "Exposure E must be positive."))
          pl_lik_spec("poisson", list(y = y, E = E), input$p_scale)
        },
        normal = {
          n <- input$n_n; yb <- input$n_ybar; s <- input$n_sigma
          validate(need(is_whole(n) && n >= 0, "n must be a whole number \u{2265} 0."),
                   need(is.finite(yb), "Enter the sample mean."),
                   need(is.finite(s) && s > 0, "\u{03c3} must be positive."))
          pl_lik_spec("normal", list(n = n, ybar = yb, sigma = s))
        }
      )
    })

    mix <- reactive({
      l <- lik()
      pr <- main$prior()
      comps <- list(pr)
      w <- 1
      if (isTRUE(input$robust)) {
        comps <- list(pr, vague$prior())
        w <- c(input$w_inf, 1 - input$w_inf)
      }
      # while the scale is switching, the pickers briefly hold the old prior
      req(all(vapply(comps, function(cp) is.null(pl_check_prior(cp, l$support)), logical(1))))
      pl_mixture(comps, w)
    })

    an <- reactive({
      l <- lik(); m <- mix()
      res <- tryCatch(pl_posterior_analysis(m, l), error = function(e) e)
      validate(need(!inherits(res, "error"), if (inherits(res, "error")) conditionMessage(res) else ""))
      res
    })

    ppc <- reactive({
      a <- an()
      set.seed(11)
      pl_prior_predictive(a$mix, a$lik)
    })

    nat_threshold <- reactive({
      c0 <- input$threshold
      if (is.finite(c0)) c0 else NULL
    })

    # ---- tiles --------------------------------------------------------------------
    output$tiles <- renderUI({
      a <- an()
      lvl <- input$level
      s <- pl_grid_summary(a$post$x, a$post$w, lvl, "equal", nat_threshold(), f = a$lik$to_nat)
      tiles <- list(
        pl_tile(pl_fmt(s$Mean, 3), paste("Posterior mean of", a$lik$nat_label)),
        pl_tile(pl_interval_str(s$Lower, s$Upper), sprintf("%d%% credible interval (equal-tailed)", round(100 * lvl)))
      )
      if (!is.null(nat_threshold())) {
        tiles[[length(tiles) + 1]] <- pl_tile(pl_pct(s$`P(> c)`), sprintf("P(%s > %s | data)", strsplit(a$lik$nat_label, " ")[[1]][1], pl_fmt(nat_threshold())))
      }
      if (a$lik$has_data) {
        pp <- ppc()
        tiles[[length(tiles) + 1]] <- pl_tile(formatC(pp$p_two, format = "f", digits = 3), "Prior predictive tail prob. (small = conflict)")
      }
      if (!is.null(a$comp_post)) {
        tiles[[length(tiles) + 1]] <- pl_tile(pl_pct(a$comp_post[1]),
                                              sprintf("Posterior weight on main component (prior %s)", pl_pct(a$mix$weights[1], 0)))
      }
      div(class = "pl-tiles", tiles)
    })

    # ---- main plot ------------------------------------------------------------------
    output$main_plot <- renderPlot({
      a <- an()
      cv <- pl_posterior_curves(a)
      keep <- intersect(c("Prior", "Likelihood", "Posterior", "Reference"), names(cv))
      if (!isTRUE(input$show_ref)) keep <- setdiff(keep, "Reference")
      long <- do.call(rbind, lapply(keep, function(k) data.frame(x = cv$x, y = cv[[k]], what = k)))
      labs_map <- c(Prior = if (length(a$mix$components) > 1) "Prior (mixture)" else "Prior",
                    Likelihood = "Likelihood (scaled)", Posterior = "Posterior",
                    Reference = paste0("Posterior with ", a$lik$ref_label, " prior"))
      long$what <- factor(labs_map[long$what], levels = labs_map[keep])
      cols <- setNames(PL_COLS[keep], labs_map[keep])
      ltys <- setNames(c(Prior = "solid", Likelihood = "dashed", Posterior = "solid", Reference = "dotted")[keep], labs_map[keep])
      lwds <- setNames(c(Prior = 1.1, Likelihood = 0.9, Posterior = 1.4, Reference = 0.9)[keep], labs_map[keep])

      lvl <- input$level
      ci <- if (input$interval == "hdi") pl_grid_hdi(a$post$x, a$post$w, lvl) else
        pl_grid_quantile(a$post$x, a$post$w, c((1 - lvl) / 2, 1 - (1 - lvl) / 2))
      shade <- cv[cv$x >= ci[1] & cv$x <= ci[2], ]
      cap <- pl_density_cap(list(cv$Posterior, cv$Prior, if (isTRUE(input$show_ref)) cv$Reference))

      ggplot(long, aes(x = x, y = y, colour = what, linetype = what, linewidth = what)) +
        geom_area(data = shade, aes(x = x, y = Posterior), inherit.aes = FALSE,
                  fill = PL_COLS[["Posterior"]], alpha = 0.18) +
        geom_line(na.rm = TRUE) +
        scale_colour_manual(values = cols) +
        scale_linetype_manual(values = ltys) +
        scale_linewidth_manual(values = lwds) +
        coord_cartesian(ylim = c(0, cap), expand = FALSE) +
        labs(x = a$lik$par_label, y = "Density",
             title = "Prior, likelihood and posterior",
             subtitle = sprintf("Shaded: %d%% %s interval of the posterior", round(100 * lvl),
                                if (input$interval == "hdi") "highest-density" else "equal-tailed")) +
        pl_theme()
    })

    output$summary_table <- renderTable({
      a <- an()
      lvl <- input$level
      c0 <- nat_threshold()
      link <- a$lik$scale == "link"
      rows <- list()
      add <- function(name, g) {
        if (is.null(g)) return()
        s1 <- pl_grid_summary(g$x, g$w, lvl, input$interval, if (!link) c0 else NULL)
        r <- data.frame(Distribution = name, Scale = a$lik$par_label,
                        Mean = pl_fmt(s1$Mean, 3), SD = pl_fmt(s1$SD, 3), Median = pl_fmt(s1$Median, 3),
                        Interval = pl_interval_str(s1$Lower, s1$Upper),
                        P = if (!link && !is.null(c0)) pl_pct(s1$`P(> c)`) else "",
                        check.names = FALSE)
        rows[[length(rows) + 1]] <<- r
        if (link) {
          s2 <- pl_grid_summary(g$x, g$w, lvl, "equal", c0, f = a$lik$to_nat)
          rows[[length(rows) + 1]] <<- data.frame(
            Distribution = name, Scale = paste(a$lik$nat_label, "(equal-tailed)"),
            Mean = pl_fmt(s2$Mean, 3), SD = pl_fmt(s2$SD, 3), Median = pl_fmt(s2$Median, 3),
            Interval = pl_interval_str(s2$Lower, s2$Upper),
            P = if (!is.null(c0)) pl_pct(s2$`P(> c)`) else "", check.names = FALSE)
        }
      }
      add("Prior", a$prior)
      add("Posterior", a$post)
      if (isTRUE(input$show_ref)) add(paste("Posterior,", a$lik$ref_label, "prior"), a$ref)
      out <- do.call(rbind, rows)
      names(out)[names(out) == "Interval"] <- sprintf("%d%% interval", round(100 * lvl))
      names(out)[names(out) == "P"] <- if (is.null(c0)) " " else sprintf("P(> %s)", pl_fmt(c0))
      out
    }, striped = TRUE, align = "l", class = "pl-table")

    output$ess_note <- renderUI({
      a <- an()
      ess <- pl_prior_ess(a)
      if (!is.finite(ess)) return(NULL)
      unit <- switch(a$lik$type, binomial = "trials", poisson = "units of exposure", normal = "observations")
      n_eff <- switch(a$lik$type, binomial = a$lik$n, poisson = a$lik$E, normal = a$lik$n)
      p(class = "pl-note",
        sprintf("Approximate prior effective sample size: %s %s (your data: %s). ", pl_fmt(ess, 3), unit, pl_fmt(n_eff)),
        "Computed as n \u{00d7} (Var_reference / Var_posterior \u{2212} 1). It is exact for the Normal model and approximate otherwise; negative values mean the prior and data disagree enough to widen the posterior.")
    })

    # ---- sensitivity ----------------------------------------------------------------
    sens <- reactive({
      a <- an()
      pp <- if (a$lik$has_data) ppc()$p_two else NULL
      list(path = pl_powerscale_path(a, level = input$level), tab = pl_sensitivity_table(a, pp))
    })

    output$sens_diag <- renderUI({
      s <- sens()$tab
      pth <- sens()$path
      pr <- pth[pth$component == "prior", ]
      shift <- pr$mean[which.max(pr$alpha)] - pr$mean[which.min(pr$alpha)]
      extra <- sprintf(" Between a prior half as strong (\u{03b1} = 0.5) and twice as strong (\u{03b1} = 2), the posterior mean moves by %s on the %s scale.",
                       pl_fmt(abs(shift), 2), an()$lik$par_label)
      tag <- switch(s$level, ok = "Diagnosis:", warn = "Check:", alert = "Warning:")
      pl_diag(s$level, tag, paste0(s$diagnosis, extra))
    })

    output$sens_plot <- renderPlot({
      a <- an()
      pth <- sens()$path
      pth$component <- factor(ifelse(pth$component == "prior", "Prior power-scaled", "Likelihood power-scaled"),
                              levels = c("Prior power-scaled", "Likelihood power-scaled"))
      ggplot(pth, aes(x = alpha)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = PL_COLS[["Posterior"]], alpha = 0.18) +
        geom_line(aes(y = mean), colour = PL_COLS[["Posterior"]], linewidth = 1.2) +
        geom_point(aes(y = mean), colour = PL_COLS[["Posterior"]], size = 2.4) +
        geom_vline(xintercept = 1, colour = PL_INK, linetype = "dotted") +
        facet_wrap(~component, nrow = 1) +
        scale_x_continuous(trans = "log2", breaks = c(0.5, 0.71, 1, 1.41, 2),
                           labels = c("0.5\nweaker", "0.71", "1\nas specified", "1.41", "2\nstronger")) +
        labs(x = "Power \u{03b1}", y = a$lik$par_label,
             title = "How the posterior moves when the prior or the data are re-weighted",
             subtitle = sprintf("Line: posterior mean. Band: %d%% equal-tailed interval.", round(100 * input$level))) +
        pl_theme()
    })

    output$sens_table <- renderTable({
      s <- sens()$tab
      lab <- function(v) if (is.na(v)) "\u{2013}" else if (v >= PL_SENS_THRESHOLD) "Flagged (\u{2265} 0.05)" else "Low"
      data.frame(
        Component = c("Prior", "Likelihood"),
        `Sensitivity index` = c(pl_fmt(s$prior, 2), if (is.na(s$likelihood)) "\u{2013}" else pl_fmt(s$likelihood, 2)),
        Level = c(lab(s$prior), lab(s$likelihood)),
        check.names = FALSE
      )
    }, striped = TRUE, align = "l", class = "pl-table")

    # ---- prior predictive ------------------------------------------------------------
    output$ppc_diag <- renderUI({
      a <- an()
      if (!a$lik$has_data) return(pl_diag("warn", "No data:", "Enter some data to compare against the prior predictive distribution."))
      pp <- ppc()
      lvl <- if (pp$p_two < 0.01) "alert" else if (pp$p_two < 0.05) "warn" else "ok"
      txt <- sprintf("Two-sided prior predictive tail probability = %s (P(Y \u{2264} observed) = %s, P(Y \u{2265} observed) = %s). %s",
                     formatC(pp$p_two, format = "f", digits = 3),
                     formatC(pp$p_lower, format = "f", digits = 3),
                     formatC(pp$p_upper, format = "f", digits = 3),
                     switch(lvl,
                            ok = "Data like yours are plausible under this prior.",
                            warn = "Your data sit in the tail of what the prior expects: possible prior\u{2013}data conflict.",
                            alert = "Your data are very surprising under this prior: strong prior\u{2013}data conflict. Consider a robust mixture prior."))
      pl_diag(lvl, switch(lvl, ok = "No conflict:", warn = "Check:", alert = "Conflict:"), txt)
    })

    output$ppc_plot <- renderPlot({
      a <- an()
      req(a$lik$has_data)
      pp <- ppc()
      l <- a$lik
      if (l$type == "binomial") {
        tab <- tabulate(pp$ysim + 1, nbins = l$n + 1) / length(pp$ysim)
        df <- data.frame(y = 0:l$n, p = tab, obs = (0:l$n) == pp$obs)
        ggplot(df, aes(x = y, y = p, fill = obs)) +
          geom_col(width = 0.85) +
          scale_fill_manual(values = c(`FALSE` = PL_COLS[["Prior"]], `TRUE` = PL_COLS[["Posterior"]]),
                            labels = c(`FALSE` = "Prior predictive", `TRUE` = "Your data")) +
          labs(x = paste("Number of", l$stat_label, "out of", l$n), y = "Probability",
               title = "What the prior expects you to see") +
          pl_theme()
      } else {
        ys <- pp$ysim[is.finite(pp$ysim)]
        hi <- quantile(ys, 0.995, names = FALSE)
        lo <- if (l$type == "normal") quantile(ys, 0.005, names = FALSE) else 0
        hi <- max(hi, pp$obs); lo <- min(lo, pp$obs)
        beyond <- mean(pp$ysim > hi | pp$ysim < lo)
        df <- data.frame(y = ys[ys >= lo & ys <= hi])
        ggplot(df, aes(x = y)) +
          geom_histogram(bins = 60, fill = PL_COLS[["Prior"]], colour = "white", linewidth = 0.2) +
          geom_vline(xintercept = pp$obs, colour = PL_COLS[["Posterior"]], linewidth = 1.3) +
          annotate("label", x = pp$obs, y = Inf, vjust = 1.3, label = "your data", size = 4,
                   colour = PL_INK, label.size = 0) +
          labs(x = if (l$type == "poisson") paste("Number of events in exposure", pl_fmt(l$E)) else "Sample mean \u{0233}",
               y = "Simulated data sets",
               title = "What the prior expects you to see",
               caption = if (beyond > 0) sprintf("%s of simulated values fall outside the plotted range.", pl_pct(beyond)) else NULL) +
          pl_theme()
      }
    })

    # ---- code -----------------------------------------------------------------------------
    output$code_stan <- renderText({
      a <- an()
      pl_stan_model_code(a$lik, a$mix)
    })
    output$code_brms <- renderText({
      a <- an()
      pl_brms_model_code(a$lik, a$mix)
    })
  })
}
