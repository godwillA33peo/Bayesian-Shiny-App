priorHierServer <- function(id, library) {
  moduleServer(id, function(input, output, session) {

    # =========================================================================
    # (a) What a tau prior implies about group differences
    # =========================================================================
    link_key <- reactiveVal(NULL)
    observe(link_key(input$gs_link))
    gs_defaults <- list(logit = pl_prior("half_normal", 1), log = pl_prior("half_normal", 0.5),
                        identity = pl_prior("half_normal", 10))
    gs_reset <- reactive({ k <- link_key(); req(k); list(supports = "positive", prior = gs_defaults[[k]]) })
    gs_tau <- priorPickerServer("gs_tau", supports = "positive", library = library, reset = gs_reset)

    observeEvent(input$gs_link, {
      updateNumericInput(session, "gs_baseline",
                         value = switch(input$gs_link, logit = 0.7, log = 5, identity = 60))
    }, ignoreInit = TRUE)

    gs <- reactive({
      link <- input$gs_link
      b <- input$gs_baseline
      validate(need(is.finite(b), "Enter the typical group's value."))
      if (link == "logit") validate(need(b > 0 && b < 1, "For a proportion the typical value must be between 0 and 1."))
      if (link == "log") validate(need(b > 0, "For a rate the typical value must be positive."))
      pr <- gs_tau$prior()
      set.seed(77)
      pl_sim_group_spread(pr, input$gs_on_var == "var", link, b, J = input$gs_J)
    })

    output$gs_tiles <- renderUI({
      g <- gs()
      q <- function(z) quantile(z[is.finite(z)], c(0.05, 0.5, 0.95), names = FALSE)
      qt <- q(g$tau); qr <- q(g$ratio)
      tiles <- list(
        pl_tile(sprintf("%s  %s", pl_fmt(qt[2], 3), pl_interval_str(qt[1], qt[3])), "\u{03c4}: median and 90% prior interval"),
        pl_tile(sprintf("%s  %s", pl_fmt(qr[2], 3), pl_interval_str(qr[1], qr[3])), paste0(g$ratio_label, " (median, 90%)"))
      )
      if (g$link == "logit") {
        qd <- q(g$diff_nat)
        tiles <- c(tiles, list(
          pl_tile(sprintf("%s pp  [%s, %s]", round(100 * qd[2]), round(100 * qd[1]), round(100 * qd[3])),
                  "Gap in percentage points, 90th vs 10th percentile group (median, 90%)"),
          pl_tile(pl_pct(mean(g$ratio > 10)), "Prior probability the odds ratio exceeds 10")
        ))
      } else if (g$link == "log") {
        tiles <- c(tiles, list(pl_tile(pl_pct(mean(g$ratio > 10)), "Prior probability the rate ratio exceeds 10")))
      }
      div(class = "pl-tiles", tiles)
    })

    output$gs_tau_plot <- renderPlot({
      g <- gs()
      pr <- gs_tau$prior()
      on_var <- input$gs_on_var == "var"
      hi <- quantile(g$tau, 0.99, names = FALSE)
      x <- seq(0, hi, length.out = 600)[-1]
      y <- pl_tau_density(pr, x, on_var)
      y[!is.finite(y)] <- NA
      ggplot(data.frame(x = x, y = y), aes(x, y)) +
        geom_area(fill = PL_COLS[["Prior"]], alpha = 0.15, na.rm = TRUE) +
        geom_line(colour = PL_COLS[["Prior"]], linewidth = 1.1, na.rm = TRUE) +
        coord_cartesian(ylim = c(0, pl_density_cap(list(y)))) +
        labs(x = "\u{03c4} (between-group SD, link scale)", y = "Density", title = "Prior on \u{03c4}",
             subtitle = if (on_var) "Placed on \u{03c4}\u{00b2}, shown on the \u{03c4} scale" else NULL) +
        pl_theme(13)
    })

    output$gs_ratio_plot <- renderPlot({
      g <- gs()
      z <- g$ratio[is.finite(g$ratio)]
      if (g$link == "identity") {
        z <- z[z <= quantile(z, 0.995)]
        p <- ggplot(data.frame(z = z), aes(z)) +
          geom_histogram(bins = 50, fill = PL_COLS[["Prior"]], colour = "white", linewidth = 0.2) +
          labs(x = "Difference between 90th and 10th percentile groups")
      } else {
        z <- pmin(z, 1e6)
        p <- ggplot(data.frame(z = z), aes(z)) +
          geom_histogram(bins = 50, fill = PL_COLS[["Prior"]], colour = "white", linewidth = 0.2) +
          geom_vline(xintercept = 10, colour = "#e34948", linetype = "dashed") +
          scale_x_log10(labels = function(b) pl_fmt(b, 3)) +
          labs(x = paste0(if (g$link == "logit") "Odds" else "Rate", " ratio, 90th vs 10th percentile group (log axis)"))
      }
      p + labs(y = "Prior draws", title = "Implied gap between groups") + pl_theme(13)
    })

    output$gs_strip_plot <- renderPlot({
      g <- gs()
      st <- g$strip
      ord <- order(unique(st[, c("draw", "tau")])$tau)
      lev <- unique(st$draw)[ord]
      st$row <- factor(st$draw, levels = lev)
      lab <- setNames(sprintf("\u{03c4} = %s", pl_fmt(unique(st[, c("draw", "tau")])$tau, 2)), unique(st$draw))
      xl <- switch(g$link, logit = "Group proportion", log = "Group rate (log axis)", identity = "Group mean")
      p <- ggplot(st, aes(x = value, y = row)) +
        geom_vline(xintercept = g$baseline, colour = PL_INK, linetype = "dotted") +
        geom_point(colour = PL_COLS[["Prior"]], alpha = 0.75, size = 2) +
        scale_y_discrete(labels = lab) +
        labs(x = xl, y = NULL, title = sprintf("30 prior draws, each showing %d groups", input$gs_J),
             subtitle = "Rows sorted by \u{03c4}. Dotted line: the typical group.") +
        pl_theme(12) + theme(panel.grid.major.y = element_blank())
      if (g$link == "logit") p <- p + scale_x_continuous(limits = c(0, 1), labels = function(b) paste0(100 * b, "%"))
      if (g$link == "log") p <- p + scale_x_log10(labels = function(b) pl_fmt(b, 3))
      p
    })

    # =========================================================================
    # (b) Partial pooling with exact marginalisation
    # =========================================================================
    pp_tau <- priorPickerServer("pp_tau", init = pl_prior("half_cauchy", 25), supports = "positive", library = library)

    observeEvent(input$pre_hc, { pp_tau$set(pl_prior("half_cauchy", 25)); updateRadioButtons(session, "pp_on_var", selected = "sd") })
    observeEvent(input$pre_unif, { pp_tau$set(pl_prior("uniform", 0, 100)); updateRadioButtons(session, "pp_on_var", selected = "sd") })
    observeEvent(input$pre_ig1, { pp_tau$set(pl_prior("inv_gamma", 1, 1)); updateRadioButtons(session, "pp_on_var", selected = "var") })
    observeEvent(input$pre_ig001, { pp_tau$set(pl_prior("inv_gamma", 0.001, 0.001)); updateRadioButtons(session, "pp_on_var", selected = "var") })

    pp_data <- reactive({
      txt <- input$pp_data
      df <- tryCatch(read.csv(text = txt, strip.white = TRUE, stringsAsFactors = FALSE), error = function(e) NULL)
      validate(need(!is.null(df) && nrow(df) >= 2, "Could not read the data: give a header row and at least two groups."))
      if (ncol(df) >= 3) {
        g <- as.character(df[[1]]); y <- suppressWarnings(as.numeric(df[[2]])); s <- suppressWarnings(as.numeric(df[[3]]))
      } else if (ncol(df) == 2) {
        g <- as.character(seq_len(nrow(df))); y <- suppressWarnings(as.numeric(df[[1]])); s <- suppressWarnings(as.numeric(df[[2]]))
      } else {
        validate(need(FALSE, "Need columns: group, estimate, se."))
      }
      ok <- is.finite(y) & is.finite(s)
      validate(need(all(ok), "Every estimate and se must be a number."),
               need(all(s > 0), "Every standard error must be positive."))
      g[!nzchar(g)] <- as.character(which(!nzchar(g)))
      data.frame(group = make.unique(g), y = y, s = s)
    })

    mu_prior <- reactive({
      if (input$pp_mu_prior == "flat") return(NULL)
      validate(need(is.finite(input$pp_m0) && is.finite(input$pp_s0) && input$pp_s0 > 0, "Prior on \u{03bc}: give a mean and a positive SD."))
      list(m0 = input$pp_m0, s0 = input$pp_s0)
    })

    pp <- reactive({
      d <- pp_data()
      pr <- pp_tau$prior()
      on_var <- input$pp_on_var == "var"
      set.seed(8)
      res <- tryCatch(pl_hier_analysis(d$y, d$s, pr, on_var, mu_prior(), level = input$pp_level),
                      error = function(e) e)
      validate(need(!inherits(res, "error"), if (inherits(res, "error")) conditionMessage(res) else ""))
      Bmat <- vapply(d$s, function(sj) sj^2 / (sj^2 + res$tau^2), numeric(length(res$tau)))
      res$pooling <- colSums(res$tau_weights * Bmat)
      res$groups <- d$group
      res
    })

    tau_upper <- reactive({
      r <- pp()
      cw <- cumsum(r$tau_weights)
      max(r$tau[which(cw >= 0.995)[1]] * 1.3, 1e-6)
    })

    output$pp_tiles <- renderUI({
      r <- pp()
      lvl <- round(100 * input$pp_level)
      div(class = "pl-tiles",
          pl_tile(sprintf("%s  %s", pl_fmt(r$tau_q[2], 3), pl_interval_str(r$tau_q[1], r$tau_q[3])),
                  sprintf("\u{03c4}: posterior median and %d%% interval", lvl)),
          pl_tile(sprintf("%s  %s", pl_fmt(r$mu[["mean"]], 3), pl_interval_str(r$mu[[2]], r$mu[[3]])),
                  sprintf("\u{03bc}: posterior mean and %d%% interval", lvl)),
          pl_tile(pl_pct(mean(r$pooling), 0), "Average pooling (0% = no pooling, 100% = complete pooling)"))
    })

    output$pp_tau_plot <- renderPlot({
      r <- pp()
      up <- tau_upper()
      keep <- r$tau <= up & r$tau > 0
      df <- rbind(
        data.frame(x = r$tau[keep], y = r$tau_prior_density[keep], what = "Prior"),
        data.frame(x = r$tau[keep], y = r$tau_post_density[keep], what = "Posterior")
      )
      df$what <- factor(df$what, levels = c("Prior", "Posterior"))
      cap <- pl_density_cap(list(r$tau_post_density[keep], r$tau_prior_density[keep]))
      ggplot(df, aes(x, y, colour = what, linetype = what)) +
        geom_line(linewidth = 1.2, na.rm = TRUE) +
        scale_colour_manual(values = PL_COLS[c("Prior", "Posterior")]) +
        scale_linetype_manual(values = c(Prior = "dashed", Posterior = "solid")) +
        coord_cartesian(xlim = c(0, up), ylim = c(0, cap), expand = FALSE) +
        labs(x = "\u{03c4} (between-group SD)", y = "Density", title = "\u{03c4}: prior vs posterior") +
        pl_theme(13)
    })

    output$pp_profile_plot <- renderPlot({
      r <- pp()
      up <- tau_upper()
      keep <- which(r$tau <= up)
      keep <- keep[unique(round(seq(1, length(keep), length.out = min(400, length(keep)))))]
      J <- length(r$groups)
      df <- data.frame(tau = rep(r$tau[keep], J),
                       m = as.vector(r$th_mean_by_tau[keep, , drop = FALSE]),
                       group = rep(r$groups, each = length(keep)))
      ends <- df[df$tau == max(df$tau), ]
      ggplot(df, aes(tau, m, group = group)) +
        annotate("rect", xmin = r$tau_q[1], xmax = min(r$tau_q[3], up), ymin = -Inf, ymax = Inf,
                 fill = PL_COLS[["Posterior"]], alpha = 0.10) +
        geom_vline(xintercept = r$tau_q[2], colour = PL_COLS[["Posterior"]], linetype = "dashed") +
        geom_line(colour = PL_COLS[["Prior"]], alpha = 0.8, linewidth = 0.8) +
        geom_text(data = ends, aes(label = group), hjust = -0.2, size = 3.6, colour = PL_INK) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
        labs(x = "\u{03c4}", y = "E[\u{03b8}\u{2c7c} | \u{03c4}, data]", title = "Shrinkage profile",
             subtitle = "Each line is one group. Shaded: posterior interval of \u{03c4}; dashed: its median.") +
        pl_theme(13)
    })

    output$pp_forest <- renderPlot({
      r <- pp()
      th <- r$theta
      J <- nrow(th)
      ypos <- rev(seq_len(J))   # first group at the top
      z <- qnorm(1 - (1 - input$pp_level) / 2)
      df <- rbind(
        data.frame(yy = ypos + 0.15, est = th$y, lo = th$y - z * th$s, hi = th$y + z * th$s, what = "Separate estimate (no pooling)"),
        data.frame(yy = ypos - 0.15, est = th$post_mean, lo = th$lower, hi = th$upper, what = "Hierarchical posterior")
      )
      df$what <- factor(df$what, levels = c("Separate estimate (no pooling)", "Hierarchical posterior"))
      ggplot(df, aes(y = yy, x = est, colour = what, shape = what)) +
        geom_vline(xintercept = r$mu[["mean"]], colour = PL_INK, linetype = "dashed") +
        geom_segment(aes(x = lo, xend = hi, yend = yy), linewidth = 0.9) +
        geom_point(size = 3, fill = "white", stroke = 1.2) +
        scale_y_continuous(breaks = ypos, labels = r$groups, minor_breaks = NULL) +
        scale_colour_manual(values = c(PL_INK, PL_COLS[["Posterior"]]), name = NULL) +
        scale_shape_manual(values = c(21, 16), name = NULL) +
        labs(x = "Effect", y = NULL,
             title = "Group estimates: raw vs partially pooled",
             subtitle = sprintf("%d%% intervals. Dashed line: posterior mean of \u{03bc}.", round(100 * input$pp_level))) +
        pl_theme(13)
    })

    output$pp_table <- renderTable({
      r <- pp()
      th <- r$theta
      lvl <- round(100 * input$pp_level)
      out <- data.frame(
        Group = r$groups, Estimate = pl_fmt(th$y, 3), SE = pl_fmt(th$s, 3),
        `Posterior mean` = pl_fmt(th$post_mean, 3), `Posterior SD` = pl_fmt(th$post_sd, 3),
        Interval = pl_interval_str(th$lower, th$upper),
        `Pooling` = pl_pct(r$pooling, 0),
        check.names = FALSE)
      names(out)[6] <- sprintf("%d%% interval", lvl)
      out
    }, striped = TRUE, align = "l", class = "pl-table")

    output$pp_code <- renderText({
      d <- pp_data()
      pr <- pp_tau$prior()
      on_var <- input$pp_on_var == "var"
      mp <- mu_prior()
      dat <- sprintf("d <- data.frame(\n  group = c(%s),\n  estimate = c(%s),\n  se = c(%s)\n)",
                     paste0("\"", d$group, "\"", collapse = ", "), paste(pl_fmt(d$y), collapse = ", "),
                     paste(pl_fmt(d$s), collapse = ", "))
      lines <- character(0); notes <- character(0)
      if (on_var) {
        notes <- c(notes, "# brms puts priors on tau itself (class = \"sd\"); a prior on tau^2 needs custom Stan code.")
      } else {
        lines <- c(lines, pl_brms_line(pr, "sd"))
      }
      if (is.null(mp)) {
        notes <- c(notes, "# Intercept (mu): brms default prior, a wide Student-t, close to flat here.")
      } else {
        lines <- c(lines, sprintf("prior(normal(%s, %s), class = \"Intercept\")", pl_fmt(mp$m0), pl_fmt(mp$s0)))
      }
      prior_arg <- if (length(lines)) {
        paste0("           prior = c(", paste(lines, collapse = ",\n                     "), "),\n")
      } else ""
      paste0(dat, "\n\n",
             if (length(notes)) paste0(paste(notes, collapse = "\n"), "\n") else "",
             "fit <- brm(estimate | se(se) ~ 1 + (1 | group), data = d,\n",
             prior_arg,
             "           control = list(adapt_delta = 0.99))")
    })

    # =========================================================================
    # (c) LKJ
    # =========================================================================
    eta_cols <- c(`0.5` = "#a03fb0", `1` = "#2a78d6", `2` = "#eb6834", `4` = "#0f9b7a", `10` = "#52514e")
    eta_lty <- c(`0.5` = "dotdash", `1` = "solid", `2` = "longdash", `4` = "dashed", `10` = "dotted")

    etas <- reactive({
      e <- input$lkj_eta
      validate(need(length(e) >= 1, "Pick at least one \u{03b7}."),
               need(length(e) <= 4, "Pick at most four values of \u{03b7} so the curves stay readable."))
      e
    })

    output$lkj_plot <- renderPlot({
      e <- etas()
      K <- input$lkj_K
      r <- seq(-0.995, 0.995, length.out = 799)
      df <- do.call(rbind, lapply(e, function(et) {
        data.frame(r = r, y = pl_lkj_marginal(r, as.numeric(et), K), eta = paste0("\u{03b7} = ", et), key = et)
      }))
      labs_ <- paste0("\u{03b7} = ", e)
      df$eta <- factor(df$eta, levels = labs_)
      cap <- pl_density_cap(split(df$y, df$eta))
      ggplot(df, aes(r, y, colour = eta, linetype = eta)) +
        geom_line(linewidth = 1.2) +
        scale_colour_manual(values = setNames(eta_cols[e], labs_)) +
        scale_linetype_manual(values = setNames(eta_lty[e], labs_)) +
        coord_cartesian(ylim = c(0, cap)) +
        labs(x = "Correlation r", y = "Density",
             title = sprintf("Marginal LKJ prior for one correlation (K = %d)", K)) +
        pl_theme()
    })

    output$lkj_table <- renderTable({
      e <- as.numeric(etas())
      K <- input$lkj_K
      cut <- input$lkj_cut
      out <- data.frame(
        eta = pl_fmt(e),
        shape = pl_fmt(e - 1 + K / 2),
        sd_r = pl_fmt(1 / sqrt(2 * (e - 1 + K / 2) + 1), 3),
        interval = vapply(e, function(et) {
          a <- et - 1 + K / 2
          q <- 2 * qbeta(c(0.05, 0.95), a, a) - 1
          pl_interval_str(q[1], q[2])
        }, ""),
        p_abs = pl_pct(pl_lkj_prob_abs_gt(cut, e, K))
      )
      setNames(out, c("\u{03b7}", "Beta shape", "Prior SD of r", "90% interval",
                      sprintf("P(|r| > %s)", pl_fmt(cut))))
    }, striped = TRUE, align = "l", class = "pl-table")

    output$lkj_code <- renderText({
      e <- etas()
      paste0("# brms: correlation between random effects, e.g. (1 + x | group)\n",
             paste(sprintf("prior(lkj(%s), class = \"cor\")", e), collapse = "\n"),
             "\n# Stan: L_Omega ~ lkj_corr_cholesky(", e[1], ");")
    })
  })
}
