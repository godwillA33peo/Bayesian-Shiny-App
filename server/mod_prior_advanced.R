priorAdvancedServer <- function(id, library) {
  moduleServer(id, function(input, output, session) {

    # =========================================================================
    # Shrinkage priors
    # =========================================================================
    observeEvent(input$sh_p, {
      updateSliderInput(session, "sh_p0", max = max(1, input$sh_p - 1),
                        value = min(input$sh_p0, max(1, input$sh_p - 1)))
    })

    sh_opts <- reactive({
      vals <- c(input$sh_normal_sd, input$sh_laplace_b, input$sh_n, input$sh_slab,
                input$sh_r2_mean, input$sh_r2_prec, input$sh_slab_sd, input$sh_spike_sd)
      validate(need(all(is.finite(vals)) && all(vals > 0), "All settings must be positive numbers."),
               need(input$sh_r2_mean < 1, "The prior mean of R\u{00b2} must be below 1."))
      list(normal_sd = input$sh_normal_sd, laplace_b = input$sh_laplace_b, p0 = input$sh_p0,
           n = input$sh_n, sigma = 1, slab_scale = input$sh_slab, slab_df = 4,
           r2_mean = input$sh_r2_mean, r2_prec = input$sh_r2_prec, cons_d2 = 0.5,
           spike_pi = min(input$sh_p0 / input$sh_p, 0.99), slab_sd = input$sh_slab_sd, spike_sd = input$sh_spike_sd)
    })

    sh_sims <- reactive({
      types <- input$sh_types
      validate(need(length(types) >= 1, "Pick at least one prior."),
               need(length(types) <= 4, "Pick at most four priors so the curves stay readable."))
      o <- sh_opts(); p <- input$sh_p
      set.seed(31)
      setNames(lapply(types, function(t) {
        B <- pl_sim_shrinkage(t, p, o, S = 4000)
        list(B = B, r2 = pl_implied_r2(B, 1), big = rowSums(abs(B) > 0.1))
      }), types)
    })

    sh_scales <- function(types) {
      labs <- unname(PL_SHRINK[types])
      list(scale_colour_manual(values = setNames(PL_SHRINK_COLS[types], labs), breaks = labs),
           scale_linetype_manual(values = setNames(PL_SHRINK_LTY[types], labs), breaks = labs))
    }

    output$sh_r2_plot <- renderPlot({
      sims <- sh_sims(); types <- names(sims)
      df <- do.call(rbind, lapply(types, function(t) data.frame(r2 = sims[[t]]$r2, prior = PL_SHRINK[[t]])))
      df$prior <- factor(df$prior, levels = unname(PL_SHRINK[types]))
      ggplot(df, aes(r2, colour = prior, linetype = prior)) +
        geom_freqpoly(aes(y = after_stat(density)), breaks = seq(0, 1, by = 0.025), linewidth = 1.1) +
        sh_scales(types) +
        coord_cartesian(xlim = c(0, 1)) +
        labs(x = "Implied R\u{00b2} (share of variance explained)", y = "Density",
             title = "What the priors say about R\u{00b2}", subtitle = sprintf("p = %d standardised predictors", input$sh_p)) +
        pl_theme(13) + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 2))
    })

    output$sh_beta_plot <- renderPlot({
      sims <- sh_sims(); types <- names(sims)
      df <- do.call(rbind, lapply(types, function(t) {
        b <- as.numeric(sims[[t]]$B[, 1:min(5, ncol(sims[[t]]$B))])
        data.frame(b = b[abs(b) <= 3], prior = PL_SHRINK[[t]])
      }))
      df$prior <- factor(df$prior, levels = unname(PL_SHRINK[types]))
      ggplot(df, aes(b, colour = prior, linetype = prior)) +
        geom_freqpoly(aes(y = after_stat(density)), breaks = seq(-3, 3, by = 0.05), linewidth = 1) +
        sh_scales(types) +
        scale_y_sqrt() +
        labs(x = "One coefficient \u{03b2}\u{2c7c} (SD units; draws within \u{00b1}3)", y = "Density (square-root axis)",
             title = "Prior for a single coefficient", subtitle = "Shrinkage priors: a spike at 0 plus heavy tails") +
        pl_theme(13) + theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 2))
    })

    output$sh_tiles <- renderUI({
      sims <- sh_sims()
      div(class = "pl-tiles", lapply(names(sims), function(t) {
        pl_tile(sprintf("%s [%s]", pl_fmt(median(sims[[t]]$r2), 2),
                        paste(pl_fmt(quantile(sims[[t]]$r2, c(0.05, 0.95), names = FALSE), 2), collapse = ", ")),
                paste(PL_SHRINK[[t]], "\u{2013} median R\u{00b2} and 90% range"))
      }))
    })

    output$sh_table <- renderTable({
      sims <- sh_sims()
      do.call(rbind, lapply(names(sims), function(t) {
        b <- as.numeric(sims[[t]]$B)
        data.frame(Prior = PL_SHRINK[[t]],
                   "P(|\u{03b2}| < 0.05)" = pl_pct(mean(abs(b) < 0.05)),
                   "P(|\u{03b2}| > 1)" = pl_pct(mean(abs(b) > 1)),
                   "Coefficients with |\u{03b2}| > 0.1 (median, 90%)" = sprintf("%s  [%s]", median(sims[[t]]$big),
                                                                            paste(quantile(sims[[t]]$big, c(0.05, 0.95), names = FALSE), collapse = ", ")),
                   check.names = FALSE)
      }))
    }, striped = TRUE, align = "l", class = "pl-table")

    output$sh_code <- renderText({
      o <- sh_opts(); types <- input$sh_types; p <- input$sh_p
      req(length(types) > 0)
      paste0("# Standardise predictors first, e.g. d[preds] <- scale(d[preds])\n",
             paste(vapply(types, function(t) paste0("# ", PL_SHRINK[[t]], "\n", pl_shrink_brms(t, p, o)), ""), collapse = "\n"))
    })

    # =========================================================================
    # Dirichlet
    # =========================================================================
    dir_alpha <- reactive({
      a <- suppressWarnings(as.numeric(trimws(strsplit(input$dir_alpha, ",")[[1]])))
      validate(need(length(a) >= 2 && all(is.finite(a)) && all(a > 0),
                    "Enter at least two positive numbers, separated by commas."),
               need(length(a) <= 8, "Up to 8 categories."))
      a
    })
    dir_names <- reactive({
      a <- dir_alpha()
      nm <- trimws(strsplit(input$dir_names, ",")[[1]])
      if (length(nm) != length(a) || any(!nzchar(nm))) nm <- paste("Category", seq_along(a))
      make.unique(nm)
    })
    set_alpha <- function(v) updateTextInput(session, "dir_alpha", value = paste(pl_fmt(v), collapse = ", "))
    observeEvent(input$dir_flat, set_alpha(rep(1, length(dir_alpha()))))
    observeEvent(input$dir_jeff, set_alpha(rep(0.5, length(dir_alpha()))))
    observeEvent(input$dir_sparse, set_alpha(rep(0.2, length(dir_alpha()))))
    observeEvent(input$dir_x10, set_alpha(10 * dir_alpha()))

    dir_cols <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")

    output$dir_marg_plot <- renderPlot({
      a <- dir_alpha(); nm <- dir_names(); a0 <- sum(a)
      x <- seq(0.0025, 0.9975, length.out = 400)
      df <- do.call(rbind, lapply(seq_along(a), function(k) data.frame(x = x, y = dbeta(x, a[k], a0 - a[k]), cat = nm[k])))
      df$cat <- factor(df$cat, levels = nm)
      ggplot(df, aes(x, y)) +
        geom_area(aes(fill = cat), alpha = 0.25) +
        geom_line(aes(colour = cat), linewidth = 1) +
        facet_wrap(~cat, nrow = 1) +
        scale_fill_manual(values = dir_cols, guide = "none") + scale_colour_manual(values = dir_cols, guide = "none") +
        coord_cartesian(ylim = c(0, pl_density_cap(split(df$y, df$cat)))) +
        scale_x_continuous(breaks = c(0, 0.5, 1)) +
        labs(x = "Share", y = "Density", title = sprintf("Prior for each share (\u{03b1}\u{2080} = %s)", pl_fmt(a0))) +
        pl_theme(12)
    })

    output$dir_draws_plot <- renderPlot({
      a <- dir_alpha(); nm <- dir_names()
      set.seed(12)
      D <- pl_rdirichlet(30, a)
      df <- data.frame(draw = rep(seq_len(30), length(a)), share = as.numeric(D),
                       cat = factor(rep(nm, each = 30), levels = rev(nm)))
      ggplot(df, aes(x = factor(draw), y = share, fill = cat)) +
        geom_col(width = 0.85, colour = "white", linewidth = 0.4) +
        scale_fill_manual(values = setNames(dir_cols[seq_along(nm)], nm), breaks = nm) +
        labs(x = "Prior draw", y = "Share", title = "30 draws from the prior: each bar is one possible split") +
        pl_theme(12) + theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())
    })

    output$dir_table <- renderTable({
      a <- dir_alpha(); nm <- dir_names(); a0 <- sum(a)
      data.frame(Category = nm, "\u{03b1}" = pl_fmt(a), `Expected share` = pl_pct(a / a0),
                 `90% interval` = vapply(seq_along(a), function(k) {
                   q <- qbeta(c(0.05, 0.95), a[k], a0 - a[k]); pl_interval_str(q[1], q[2])
                 }, ""),
                 `Marginal prior` = sprintf("Beta(%s, %s)", pl_fmt(a), pl_fmt(a0 - a)),
                 check.names = FALSE)
    }, striped = TRUE, align = "l", class = "pl-table")

    output$dir_code <- renderText({
      a <- dir_alpha()
      sprintf(paste0("// Stan\nsimplex[%d] theta;\ntheta ~ dirichlet([%s]');\n\n",
                     "# brms: monotonic effect of an ordinal predictor, y ~ mo(x)\n",
                     "# (these %d values fit an ordinal predictor x with %d levels)\n",
                     "prior(dirichlet(%s), class = \"simo\", coef = \"mox1\")"),
              length(a), paste(pl_fmt(a), collapse = ", "), length(a), length(a) + 1, paste(pl_fmt(a), collapse = ", "))
    })

    # =========================================================================
    # Covariance
    # =========================================================================
    cv_sd <- priorPickerServer("cv_sd", init = pl_prior("half_normal", 1), supports = "positive", library = library)

    cv_sims <- reactive({
      K <- input$cv_K; nu <- input$cv_nu; s <- input$cv_s; eta <- input$cv_eta
      validate(need(is.finite(nu) && nu > K - 1, sprintf("Inverse-Wishart needs df \u{03bd} > K \u{2212} 1 = %d.", K - 1)),
               need(is.finite(s) && s > 0, "Scale s must be positive."),
               need(is.finite(eta) && eta > 0, "\u{03b7} must be positive."))
      sdp <- cv_sd$prior()
      set.seed(21)
      list(iw = pl_sim_iw(K, nu, s), lkj = pl_sim_lkj(K, eta, sdp))
    })

    cv_long <- reactive({
      sm <- cv_sims()
      rbind(data.frame(sm$iw, prior = "Inverse-Wishart"),
            data.frame(sm$lkj, prior = "LKJ + independent SD"))
    })

    output$cv_tiles <- renderUI({
      sm <- cv_sims()
      rc <- function(d) suppressWarnings(cor(log(d$sd1), abs(d$r12), method = "spearman"))
      div(class = "pl-tiles",
          pl_tile(pl_fmt(rc(sm$iw), 2), "Inverse-Wishart: rank correlation of log SD with |correlation|"),
          pl_tile(pl_fmt(rc(sm$lkj), 2), "LKJ: the same (\u{2248} 0 means independent)"),
          pl_tile(pl_fmt(quantile(sm$iw$sd1, 0.95, names = FALSE) / quantile(sm$iw$sd1, 0.05, names = FALSE), 2),
                  "Inverse-Wishart: ratio of 95th to 5th percentile SD"))
    })

    output$cv_scatter <- renderPlot({
      df <- cv_long()
      df <- df[is.finite(df$sd1) & df$sd1 > 0, ]
      df <- df[ave(seq_len(nrow(df)), df$prior, FUN = seq_along) <= 1500, ]
      ggplot(df, aes(r12, sd1)) +
        geom_point(alpha = 0.25, size = 1, colour = PL_COLS[["Prior"]]) +
        facet_wrap(~prior) +
        scale_y_log10(labels = function(b) pl_fmt(b, 2)) +
        labs(x = "Correlation between variables 1 and 2", y = "SD of variable 1 (log axis)",
             title = "Do SDs and correlations depend on each other under the prior?",
             subtitle = "1,500 prior draws each") +
        pl_theme(13)
    })

    output$cv_r_plot <- renderPlot({
      df <- cv_long()
      ggplot(df, aes(r12, colour = prior, linetype = prior)) +
        geom_freqpoly(aes(y = after_stat(density)), breaks = seq(-1, 1, by = 0.05), linewidth = 1.1) +
        scale_colour_manual(values = c(`Inverse-Wishart` = PL_COLS[["Posterior"]], `LKJ + independent SD` = PL_COLS[["Prior"]])) +
        scale_linetype_manual(values = c(`Inverse-Wishart` = "solid", `LKJ + independent SD` = "dashed")) +
        labs(x = "Correlation r\u{2081}\u{2082}", y = "Density", title = "Prior for one correlation") +
        pl_theme(12) + theme(legend.position = "bottom")
    })

    output$cv_sd_plot <- renderPlot({
      df <- cv_long()
      df <- df[is.finite(df$sd1) & df$sd1 > 0, ]
      ggplot(df, aes(sd1, colour = prior, linetype = prior)) +
        geom_density(linewidth = 1.1, adjust = 1.2) +
        scale_x_log10(labels = function(b) pl_fmt(b, 2)) +
        scale_colour_manual(values = c(`Inverse-Wishart` = PL_COLS[["Posterior"]], `LKJ + independent SD` = PL_COLS[["Prior"]])) +
        scale_linetype_manual(values = c(`Inverse-Wishart` = "solid", `LKJ + independent SD` = "dashed")) +
        labs(x = "SD of variable 1 (log axis)", y = "Density", title = "Prior for one SD") +
        pl_theme(12) + theme(legend.position = "bottom")
    })

    output$cv_code <- renderText({
      K <- input$cv_K
      sdp <- tryCatch(cv_sd$prior(), error = function(e) NULL)
      req(sdp)
      sprintf(paste0(
        "// Stan: Inverse-Wishart (older style)\n",
        "cov_matrix[%d] Sigma;\nSigma ~ inv_wishart(%s, diag_matrix(rep_vector(%s, %d)));\n\n",
        "// Stan: LKJ + separate SDs (recommended)\n",
        "cholesky_factor_corr[%d] L_Omega;\nvector<lower=0>[%d] tau;\n",
        "L_Omega ~ lkj_corr_cholesky(%s);\n%s\n",
        "matrix[%d, %d] L_Sigma = diag_pre_multiply(tau, L_Omega);  // Sigma = L_Sigma * L_Sigma'\n\n",
        "# brms: (1 + x | group)\nprior(lkj(%s), class = \"cor\")\nprior(%s, class = \"sd\")"),
        K, pl_fmt(input$cv_nu), pl_fmt(input$cv_s^2), K, K, K, pl_fmt(input$cv_eta),
        pl_stan_prior_stmt(sdp, "tau"), K, K, pl_fmt(input$cv_eta), pl_stan_string(sdp))
    })
  })
}
