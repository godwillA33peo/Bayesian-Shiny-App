# ---- predictive-check helpers -------------------------------------------------------------

pl_block_support <- function(type) switch(type, real = "real", positive = "positive", unit = "unit", ordered = "real")

# Test statistics used for predictive checks, by outcome type
pl_ppc_stats <- function(model) {
  yt <- PL_MODELS[[model]]$y_type
  if (isTRUE(PL_MODELS[[model]]$needs_event)) {
    return(list(`Median time` = median, `90th percentile time` = function(v) quantile(v, 0.9, names = FALSE)))
  }
  switch(yt,
    binary = list(`Share of 1s` = mean),
    count = list(Mean = mean, SD = sd, `Share of zeros` = function(v) mean(v == 0), Max = max),
    ordinal = list(`Mean category` = mean, `Share in lowest category` = function(v) mean(v == 1),
                   `Share in highest category` = function(v) mean(v == max(v))),
    list(Mean = mean, SD = sd, Min = min, Max = max)
  )
}

#' Survival: compare the Kaplan-Meier estimate of the data with the predictive
#' (population-averaged) survival curve at the quartiles of follow-up; valid
#' under censoring, unlike statistics of raw times.
pl_ppc_table_surv <- function(d, par_list) {
  km <- pl_km(d$y, d$event)
  tq <- quantile(d$y, c(0.25, 0.5, 0.75), names = FALSE)
  rows <- lapply(tq, function(tt) {
    obs <- km$s[max(which(km$t <= tt))]
    sim <- vapply(par_list, function(p) mean(pweibull(tt, p$shape, exp(pl_eta(p, d)), lower.tail = FALSE)), 0)
    sim <- sim[is.finite(sim)]
    q <- quantile(sim, c(0.05, 0.5, 0.95), names = FALSE)
    data.frame(Statistic = sprintf("Event-free at t = %s", pl_fmt(tt, 3)), "Your data (Kaplan\u2013Meier)" = pl_pct(obs),
               "Simulated median" = pl_pct(q[2]), "Simulated 90% range" = sprintf("[%s, %s]", pl_pct(q[1]), pl_pct(q[3])),
               "P(simulated \u2265 yours)" = formatC(mean(sim >= obs), format = "f", digits = 2), check.names = FALSE)
  })
  do.call(rbind, rows)
}

pl_ppc_table <- function(model, y, yrep, K = NULL, d = NULL, par_list = NULL) {
  if (isTRUE(PL_MODELS[[model]]$needs_event) && !is.null(d)) return(pl_ppc_table_surv(d, par_list))
  st <- pl_ppc_stats(model)
  if (PL_MODELS[[model]]$y_type == "ordinal" && !is.null(K)) {
    st[["Share in highest category"]] <- function(v) mean(v == K)
  }
  rows <- lapply(names(st), function(nm) {
    f <- st[[nm]]
    obs <- f(y)
    sim <- apply(yrep, 1, function(r) { r <- r[is.finite(r)]; if (length(r)) f(r) else NA })
    sim <- sim[is.finite(sim)]
    q <- if (length(sim)) quantile(sim, c(0.05, 0.5, 0.95), names = FALSE) else rep(NA, 3)
    data.frame(Statistic = nm, `Your data` = pl_fmt(obs, 3), `Simulated median` = pl_fmt(q[2], 3),
               `Simulated 90% range` = pl_interval_str(q[1], q[3]),
               "P(simulated \u{2265} yours)" = if (length(sim)) formatC(mean(sim >= obs), format = "f", digits = 2) else "\u{2013}",
               check.names = FALSE)
  })
  do.call(rbind, rows)
}

pl_km <- function(time, event) {
  o <- order(time); t <- time[o]; e <- event[o]
  ut <- unique(t[e == 1])
  s <- 1; out_t <- 0; out_s <- 1
  for (tt in ut) {
    n_risk <- sum(t >= tt); d <- sum(t == tt & e == 1)
    s <- s * (1 - d / n_risk)
    out_t <- c(out_t, tt); out_s <- c(out_s, s)
  }
  data.frame(t = out_t, s = out_s)
}

#' Predictive check plot. yrep: matrix [draws, obs]
pl_ppc_plot <- function(model, d, yrep, title, par_list = NULL) {
  m <- PL_MODELS[[model]]
  y <- d$y
  col_rep <- PL_COLS[["Prior"]]
  if (isTRUE(m$needs_event)) {
    # survival: Kaplan-Meier of the data vs population-averaged predictive S(t)
    km <- pl_km(y, d$event)
    tg <- seq(0, max(y) * 1.05, length.out = 120)
    use <- par_list[seq_len(min(60, length(par_list)))]
    curves <- do.call(rbind, lapply(seq_along(use), function(i) {
      p <- use[[i]]
      sc <- exp(pl_eta(p, d))
      S <- vapply(tg, function(tt) mean(pweibull(tt, p$shape, sc, lower.tail = FALSE)), 0)
      data.frame(t = tg, s = S, draw = i)
    }))
    return(ggplot() +
      geom_line(data = curves, aes(t, s, group = draw), colour = col_rep, alpha = 0.18, linewidth = 0.5) +
      geom_step(data = km, aes(t, s), colour = "#0b0b0b", linewidth = 1.1) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = "Time", y = "Proportion still event-free", title = title,
           subtitle = "Black: Kaplan\u{2013}Meier curve of your data. Blue: curves implied by 60 draws.") +
      pl_theme(13))
  }
  if (m$y_type %in% c("count", "ordinal", "binary")) {
    vals <- if (m$y_type == "ordinal") seq_len(d$K) else if (m$y_type == "binary") 0:1 else {
      top <- max(y); 0:min(max(top, 5), 60)
    }
    last <- max(vals)
    bucket <- function(v) if (m$y_type == "count") pmin(v, last) else v
    obs <- vapply(vals, function(k) mean(bucket(y) == k), 0)
    sim <- t(apply(yrep, 1, function(r) {
      r <- r[is.finite(r)]
      if (!length(r)) return(rep(NA_real_, length(vals)))
      vapply(vals, function(k) mean(bucket(r) == k), 0)
    }))
    if (length(vals) == 1) sim <- t(sim)
    df <- data.frame(k = vals, obs = obs,
                     lo = apply(sim, 2, quantile, 0.05, na.rm = TRUE),
                     md = apply(sim, 2, quantile, 0.5, na.rm = TRUE),
                     hi = apply(sim, 2, quantile, 0.95, na.rm = TRUE))
    xl <- if (m$y_type == "count") sprintf("Value (%s = %d or more)", last, last) else "Category"
    return(ggplot(df, aes(x = k)) +
      geom_col(aes(y = obs), fill = "#d6d6d2", width = 0.8) +
      geom_linerange(aes(ymin = lo, ymax = hi), colour = col_rep, linewidth = 1.2) +
      geom_point(aes(y = md), colour = col_rep, size = 2.6) +
      scale_x_continuous(breaks = if (length(vals) <= 15) vals else waiver()) +
      labs(x = xl, y = "Share of observations", title = title,
           subtitle = "Grey bars: your data. Blue: simulated median and 90% range.") +
      pl_theme(13))
  }
  # continuous: density overlay on a range anchored to the data
  rng <- range(y); w <- diff(rng); if (w == 0) w <- 1
  lo <- rng[1] - 0.5 * w; hi <- rng[2] + 0.5 * w
  if (m$y_type == "positive") lo <- max(0, lo)
  if (m$y_type == "unit") { lo <- 0; hi <- 1 }
  use <- seq_len(min(50, nrow(yrep)))
  dens <- do.call(rbind, lapply(use, function(i) {
    r <- yrep[i, ]; r <- r[is.finite(r)]
    r <- r[r >= lo & r <= hi]
    if (length(r) < 5) return(NULL)
    dd <- density(r, from = lo, to = hi, n = 256)
    data.frame(x = dd$x, y = dd$y * length(r) / ncol(yrep), draw = i)
  }))
  do <- density(y, from = lo, to = hi, n = 256)
  outside <- mean(!(yrep >= lo & yrep <= hi), na.rm = TRUE)
  p <- ggplot()
  if (!is.null(dens)) p <- p + geom_line(data = dens, aes(x, y, group = draw), colour = col_rep, alpha = 0.22, linewidth = 0.5)
  p + geom_line(data = data.frame(x = do$x, y = do$y), aes(x, y), colour = "#0b0b0b", linewidth = 1.2) +
    labs(x = "y", y = "Density", title = title,
         subtitle = "Black: your data. Blue: 50 simulated data sets.",
         caption = if (outside > 0.01) sprintf("%s of simulated values fall outside the plotted range.", pl_pct(outside)) else NULL) +
    pl_theme(13)
}

modelLabServer <- function(id, library) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- data -----------------------------------------------------------------------------
    uploaded <- reactive({
      f <- input$file
      req(f)
      df <- tryCatch(read.csv(f$datapath, stringsAsFactors = FALSE), error = function(e) NULL)
      validate(need(!is.null(df) && nrow(df) > 0, "Could not read that CSV file."))
      df
    })

    output$col_ui <- renderUI({
      df <- uploaded()
      num <- names(df)[vapply(df, is.numeric, logical(1))]
      validate(need(length(num) > 0, "The file has no numeric columns."))
      tagList(
        selectInput(ns("col_y"), "Outcome column y", choices = num, selected = num[1]),
        selectInput(ns("col_x"), "Predictor column x", choices = c("(none)" = "", num),
                    selected = if (length(num) > 1) num[2] else ""),
        if (isTRUE(PL_MODELS[[input$model]]$needs_event))
          selectInput(ns("col_event"), "Event column (1 = event, 0 = censored)", choices = num,
                      selected = if ("event" %in% num) "event" else num[length(num)])
      )
    })

    raw <- reactive({
      model <- input$model
      if (input$source == "example") {
        ex <- PL_EXAMPLES[[model]]
        df <- ex$make()
        list(y = df$y, x = df$x, event = df$event, title = ex$title,
             y_label = ex$y_label, x_label = ex$x_label, key = paste("example", model))
      } else {
        df <- uploaded()
        req(input$col_y)
        y <- df[[input$col_y]]
        x <- if (!is.null(input$col_x) && nzchar(input$col_x)) df[[input$col_x]] else NULL
        ev <- if (isTRUE(PL_MODELS[[model]]$needs_event)) { req(input$col_event); df[[input$col_event]] } else NULL
        keep <- is.finite(y) & (if (is.null(x)) TRUE else is.finite(x)) & (if (is.null(ev)) TRUE else is.finite(ev))
        list(y = y[keep], x = if (is.null(x)) NULL else x[keep], event = if (is.null(ev)) NULL else ev[keep],
             title = sprintf("Your data: %s (%d rows used)", input$file$name, sum(keep)),
             y_label = input$col_y, x_label = if (is.null(x)) "x" else input$col_x,
             key = paste("upload", input$file$name, input$col_y, input$col_x, input$col_event))
      }
    })

    output$data_note <- renderUI({
      r <- raw()
      p(class = "pl-note", strong("Data: "), r$title)
    })

    mdata <- reactive({
      model <- input$model
      r <- raw()
      msg <- pl_check_y(model, r$y, r$event)
      validate(need(is.null(msg), msg))
      validate(need(length(r$y) <= 3000, "The in-app sampler is limited to 3,000 rows. Use the Stan code tab to fit larger data in Stan."))
      x <- if (isTRUE(input$use_x)) r$x else NULL
      d <- pl_model_data(model, r$y, x, r$event)
      d$labels <- r[c("y_label", "x_label", "title")]
      d
    })

    blocks <- reactive({
      d <- mdata()
      PL_MODELS[[input$model]]$blocks(!is.null(d$x), d$K)
    })

    defaults <- reactive(PL_MODELS[[input$model]]$defaults(mdata()))

    # ---- dynamic prior pickers ------------------------------------------------------------
    reset_tick <- reactiveVal(0)
    observeEvent(input$reset, reset_tick(reset_tick() + 1))
    data_key <- reactive(paste(input$model, raw()$key))

    pickers <- new.env()
    pickers_version <- reactiveVal(0)   # bumps when a picker is created, so priors() re-reads
    pid <- function(model, name) paste0("pk_", model, "_", name)

    observe({
      bl <- tryCatch(blocks(), error = function(e) NULL)
      if (is.null(bl)) return()
      mdl <- isolate(input$model)
      for (b in bl) {
        id <- pid(mdl, b$name)
        if (is.null(pickers[[id]])) {
          local({
            bname <- b$name; btype <- b$type; mm <- mdl; pid_ <- id
            rs <- reactive({
              reset_tick(); data_key()
              req(isolate(input$model) == mm)
              list(supports = pl_block_support(btype), prior = isolate(defaults())[[bname]])
            })
            assign(pid_, priorPickerServer(pid_, init = isolate(defaults())[[bname]],
                                           supports = pl_block_support(btype),
                                           library = library, reset = rs), envir = pickers)
            pickers_version(isolate(pickers_version()) + 1)
          })
        }
      }
    })

    output$prior_ui <- renderUI({
      bl <- blocks()
      mdl <- input$model
      dfl <- isolate(defaults())
      tagList(lapply(bl, function(b) {
        id <- pid(mdl, b$name)
        pk <- pickers[[id]]
        cur <- if (!is.null(pk)) tryCatch(isolate(pk$prior()), error = function(e) NULL) else NULL
        sel <- if (!is.null(cur)) cur$family else dfl[[b$name]]$family
        div(style = "border-top:1px dashed #d6d6d2; padding-top:8px; margin-top:4px;",
            div(class = "pl-section", style = "margin-top:0;", b$label,
                if (b$type == "ordered") span(class = "pl-note", sprintf(" (%d cutpoints)", b$size))),
            p(class = "pl-note", style = "margin-bottom:4px;", b$desc),
            priorPickerUI(ns(id), "Prior", supports = pl_block_support(b$type), selected = sel))
      }))
    })

    priors <- reactive({
      pickers_version()
      bl <- blocks()
      mdl <- input$model
      out <- list()
      for (b in bl) {
        pk <- pickers[[pid(mdl, b$name)]]
        req(pk)
        pr <- pk$prior()
        if (b$type == "ordered") {
          validate(need(pr$family != "uniform", "Uniform priors are not supported for cutpoints; choose another family."))
        }
        out[[b$name]] <- pr
      }
      out
    })

    # ---- prior predictive (live) -------------------------------------------------------------
    prior_sim <- reactive({
      d <- mdata(); pr <- priors()
      set.seed(99)
      pars <- pl_prior_draws(input$model, d, pr, S = 200)
      list(pars = pars, yrep = pl_yrep(input$model, d, pars))
    })

    output$data_plot <- renderPlot({
      d <- mdata()
      m <- PL_MODELS[[input$model]]
      lab <- d$labels
      xr <- if (!is.null(d$x)) d$x + d$x_mean else NULL
      if (!is.null(xr)) {
        df <- data.frame(x = xr, y = d$y)
        p <- ggplot(df, aes(x, y))
        p <- if (m$y_type %in% c("count", "ordinal", "binary")) {
          p + geom_jitter(width = 0.15 * sd(xr) / 4, height = 0.12, alpha = 0.55, colour = PL_INK, size = 1.8)
        } else p + geom_point(alpha = 0.55, colour = PL_INK, size = 1.8)
        if (isTRUE(m$needs_event)) {
          df$status <- ifelse(d$event == 1, "Event", "Censored")
          p <- ggplot(df, aes(x, y, shape = status)) + geom_point(alpha = 0.7, colour = PL_INK, size = 2) +
            scale_shape_manual(values = c(Event = 16, Censored = 1), name = NULL)
        }
        p + labs(x = lab$x_label, y = lab$y_label, title = "Your data") + pl_theme(13)
      } else {
        df <- data.frame(y = d$y)
        p <- if (m$y_type %in% c("count", "ordinal", "binary")) {
          ggplot(df, aes(y)) + geom_bar(fill = "#9a9a96", width = 0.8)
        } else ggplot(df, aes(y)) + geom_histogram(bins = 30, fill = "#9a9a96", colour = "white", linewidth = 0.2)
        p + labs(x = lab$y_label, y = "Count", title = "Your data") + pl_theme(13)
      }
    })

    output$prior_pc_plot <- renderPlot({
      d <- mdata(); ps <- prior_sim()
      pl_ppc_plot(input$model, d, ps$yrep, "Prior predictive check", ps$pars)
    })

    output$prior_pc_table <- renderTable({
      d <- mdata(); ps <- prior_sim()
      pl_ppc_table(input$model, d$y, ps$yrep, d$K, d, ps$pars)
    }, striped = TRUE, align = "l", class = "pl-table")

    # ---- fitting ------------------------------------------------------------------------------
    sig <- reactive(list(input$model, raw()$key, input$use_x, priors(), input$n_iter))
    # Observers must never see validation errors, and debounce() drops the first
    # valid value if its input was still erroring, so return NULL instead.
    sig_safe <- reactive(tryCatch(sig(), error = function(e) NULL))
    sig_d <- debounce(sig_safe, 900)
    fit_request <- reactiveVal(0)
    observeEvent(sig_d(), { if (isTRUE(input$auto)) fit_request(fit_request() + 1) })
    observeEvent(input$fit, fit_request(fit_request() + 1))

    fit <- eventReactive(fit_request(), {
      req(fit_request() > 0)
      model <- isolate(input$model)
      d <- isolate(mdata()); pr <- isolate(priors())
      n_iter <- isolate(input$n_iter)
      s <- isolate(sig())
      res <- withProgress(message = "Sampling 4 chains\u{2026}", value = 0.3, {
        set.seed(2026)
        tryCatch(pl_model_fit(model, d, pr, n_warmup = 1000, n_iter = n_iter), error = function(e) e)
      })
      validate(need(!inherits(res, "error"), paste("Sampling failed:", if (inherits(res, "error")) conditionMessage(res) else "")))
      res$sig <- s
      res$summary <- pl_draws_summary(res$draws, res$names)
      res
    })

    fit_current <- reactive({
      f <- fit()
      s <- tryCatch(sig(), error = function(e) NULL)
      list(fit = f, stale = !identical(f$sig, s))
    })

    output$status <- renderUI({
      fc <- tryCatch(fit_current(), error = function(e) NULL)
      if (is.null(fc)) {
        return(pl_diag("warn", "Not fitted yet:", "set your priors and click \u{201c}Fit model\u{201d} (or tick automatic refitting)."))
      }
      f <- fc$fit; s <- f$summary
      rmax <- max(s$rhat, na.rm = TRUE); emin <- min(s$ess_bulk, na.rm = TRUE)
      lvl <- if (rmax > 1.05 || emin < 100) "alert" else if (rmax > 1.01 || emin < 400) "warn" else "ok"
      tiles <- div(class = "pl-tiles",
                   pl_tile(sprintf("%d \u00d7 %s", dim(f$draws)[2], format(dim(f$draws)[1], big.mark = ",")),
                           sprintf("Chains \u00d7 draws, %d observations", f$data$N)),
                   pl_tile(sprintf("%.3f", rmax), "Largest R-hat (want < 1.01)"),
                   pl_tile(sprintf("%.0f", emin), "Smallest bulk ESS (want > 400)"),
                   pl_tile(sprintf("%.1f s", f$seconds), sprintf("Sampling time (acceptance %s)", paste(round(100 * f$accept), collapse = "/"))))
      note <- if (fc$stale) pl_diag("warn", "Inputs changed:", "the results below are from the previous settings. Click \u{201c}Fit model\u{201d} to update.") else
        if (lvl == "alert") pl_diag("alert", "Poor mixing:", "R-hat or ESS is far from target. Increase the draws per chain, or check that the priors and data are compatible.") else
        if (lvl == "warn") pl_diag("warn", "Borderline diagnostics:", "increase the draws per chain for more precise estimates.") else NULL
      tagList(tiles, note)
    })

    flat <- reactive(pl_fit_flat(fit()))

    post_pars <- reactive({
      f <- fit(); fl <- flat()
      set.seed(7)
      use <- sample.int(nrow(fl), min(200, nrow(fl)))
      lapply(use, function(i) pl_draw_par(f, fl, i))
    })

    # ---- posterior -------------------------------------------------------------------------------
    output$effect_tiles <- renderUI({
      f <- fit()
      if (!("b" %in% f$names)) return(NULL)
      m <- PL_MODELS[[f$model]]
      bd <- flat()[, match("b", f$names)]
      eff <- m$effect$f(bd)
      q <- quantile(eff, c(0.025, 0.5, 0.975), names = FALSE)
      xl <- f$data$labels$x_label
      div(class = "pl-tiles",
          pl_tile(sprintf("%s  %s", pl_fmt(q[2], 3), pl_interval_str(q[1], q[3])),
                  paste0(m$effect$label, " (", xl, "); median and 95% interval")),
          pl_tile(pl_pct(mean(bd > 0)), "Posterior probability that \u{03b2} > 0"))
    })

    output$post_table <- renderTable({
      f <- fit()
      s <- f$summary
      fl <- flat()
      # prior variance for contraction
      set.seed(5)
      pd <- pl_prior_draws(f$model, f$data, f$priors, S = 4000)
      pv <- vapply(seq_along(f$names), function(k) {
        nm <- f$names[k]
        base <- sub("\\[.*", "", nm)
        b <- f$blocks[[which(vapply(f$blocks, `[[`, "", "name") == base)]]
        if (b$type != "ordered") {
          sdv <- tryCatch(pl_dist(f$priors[[base]]$family)$sd(f$priors[[base]]$params), error = function(e) NA)
          if (is.finite(sdv)) return(sdv^2)
        }
        j <- if (b$size > 1) as.integer(sub(".*\\[(\\d+)\\]", "\\1", nm)) else 1
        v <- vapply(pd, function(p) p[[base]][j], 0)
        v <- v[is.finite(v)]
        # heavy tails: an infinite-variance prior shows as no contraction measure
        if (b$type != "ordered") NA_real_ else var(v)
      }, 0)
      contraction <- 1 - s$sd^2 / pv
      data.frame(
        Parameter = s$param,
        Mean = pl_fmt(s$mean, 3), SD = pl_fmt(s$sd, 3),
        `95% interval` = pl_interval_str(s$lower, s$upper),
        Contraction = ifelse(is.finite(contraction), formatC(contraction, format = "f", digits = 2), "\u{2013}"),
        `R-hat` = formatC(s$rhat, format = "f", digits = 3),
        `Bulk ESS` = formatC(s$ess_bulk, format = "f", digits = 0),
        `Tail ESS` = formatC(s$ess_tail, format = "f", digits = 0),
        Prior = vapply(s$param, function(nm) pl_prior_label(f$priors[[sub("\\[.*", "", nm)]]), ""),
        check.names = FALSE
      )
    }, striped = TRUE, align = "l", class = "pl-table")

    output$post_plot <- renderPlot({
      f <- fit(); fl <- flat()
      set.seed(5)
      pd <- pl_prior_draws(f$model, f$data, f$priors, S = 4000)
      curves <- do.call(rbind, lapply(seq_along(f$names), function(k) {
        nm <- f$names[k]; base <- sub("\\[.*", "", nm)
        b <- f$blocks[[which(vapply(f$blocks, `[[`, "", "name") == base)]]
        v <- fl[, k]
        q <- quantile(v, c(0.001, 0.999), names = FALSE); w <- diff(q)
        lim <- PL_SUPPORT_LIMITS[[pl_block_support(b$type)]]
        rng <- c(max(q[1] - 0.4 * w, lim[1]), min(q[2] + 0.4 * w, lim[2]))
        dpost <- density(v, from = rng[1], to = rng[2], n = 256)
        xs <- dpost$x
        prior_y <- if (b$type == "ordered") {
          j <- as.integer(sub(".*\\[(\\d+)\\]", "\\1", nm))
          pv <- vapply(pd, function(p) p[[base]][j], 0)
          pv <- pv[pv >= rng[1] - w & pv <= rng[2] + w]
          if (length(pv) > 20) {
            dp <- density(pv, from = rng[1], to = rng[2], n = 256)
            dp$y * length(pv) / length(pd)
          } else rep(0, 256)
        } else pl_d(f$priors[[base]], xs)
        rbind(data.frame(param = nm, x = xs, y = dpost$y, what = "Posterior"),
              data.frame(param = nm, x = xs, y = prior_y, what = "Prior"))
      }))
      curves$param <- factor(curves$param, levels = f$names)
      curves$what <- factor(curves$what, levels = c("Prior", "Posterior"))
      ggplot(curves, aes(x, y, colour = what, linetype = what)) +
        geom_line(linewidth = 1.05, na.rm = TRUE) +
        facet_wrap(~param, scales = "free", ncol = 3) +
        scale_colour_manual(values = PL_COLS[c("Prior", "Posterior")]) +
        scale_linetype_manual(values = c(Prior = "dashed", Posterior = "solid")) +
        labs(x = NULL, y = "Density", title = "Prior vs posterior for each parameter",
             subtitle = "Plotted over the range where the posterior lives; a flat dashed line means the prior is wide here.") +
        pl_theme(12)
    })

    # ---- posterior predictive ---------------------------------------------------------------------
    post_yrep <- reactive({
      f <- fit()
      set.seed(8)
      pl_yrep(f$model, f$data, post_pars())
    })

    output$post_pc_plot <- renderPlot({
      f <- fit()
      pl_ppc_plot(f$model, f$data, post_yrep(), "Posterior predictive check", post_pars())
    })

    output$post_pc_table <- renderTable({
      f <- fit()
      pl_ppc_table(f$model, f$data$y, post_yrep(), f$data$K, f$data, post_pars())
    }, striped = TRUE, align = "l", class = "pl-table")

    # ---- diagnostics ----------------------------------------------------------------------------------
    output$trace_plot <- renderPlot({
      f <- fit()
      dr <- f$draws
      n <- dim(dr)[1]
      keep <- unique(round(seq(1, n, length.out = min(n, 500))))
      df <- do.call(rbind, lapply(seq_along(f$names), function(k) {
        do.call(rbind, lapply(seq_len(dim(dr)[2]), function(ch) {
          data.frame(param = f$names[k], iter = keep, value = dr[keep, ch, k], chain = paste("Chain", ch))
        }))
      }))
      df$param <- factor(df$param, levels = f$names)
      ggplot(df, aes(iter, value, colour = chain)) +
        geom_line(linewidth = 0.35, alpha = 0.8) +
        facet_wrap(~param, scales = "free_y", ncol = 2) +
        scale_colour_manual(values = unname(PL_SLOT_COLS)) +
        labs(x = "Iteration (after warm-up)", y = NULL, title = "Trace plots",
             subtitle = "Well-mixed chains overlap like a hairy caterpillar, with no trends.") +
        pl_theme(12)
    })

    output$diag_table <- renderTable({
      f <- fit()
      data.frame(Chain = seq_along(f$accept), `Acceptance rate` = pl_pct(f$accept, 0),
                 check.names = FALSE)
    }, striped = TRUE, align = "l", class = "pl-table")

    # ---- model comparison ------------------------------------------------------------------------------
    comparisons <- reactiveVal(list())
    observeEvent(input$add_compare, {
      f <- tryCatch(fit(), error = function(e) NULL)
      if (is.null(f)) { showNotification("Fit a model first.", type = "error"); return() }
      w <- pl_waic(pl_loglik_matrix(f, 2000))
      lab <- paste0(PL_MODELS[[f$model]]$label, if ("b" %in% f$names) " + x" else "")
      lst <- comparisons()
      lst[[length(lst) + 1]] <- list(label = lab, w = w, ykey = paste(f$data$y, collapse = ","))
      comparisons(lst)
      showNotification(paste("Added", lab), type = "message")
    })
    observeEvent(input$clear_compare, comparisons(list()))

    output$compare_table <- renderTable({
      lst <- comparisons()
      validate(need(length(lst) > 0, "No models added yet."))
      same <- vapply(lst, `[[`, "", "ykey") == lst[[length(lst)]]$ykey
      lst <- lst[same]
      el <- vapply(lst, function(z) z$w$elpd, 0)
      best <- which.max(el)
      out <- data.frame(
        Model = vapply(lst, `[[`, "", "label"),
        `elpd (WAIC)` = formatC(el, format = "f", digits = 1),
        SE = formatC(vapply(lst, function(z) z$w$se, 0), format = "f", digits = 1),
        "\u{0394}elpd vs best" = formatC(el - el[best], format = "f", digits = 1),
        "SE of \u{0394}" = vapply(seq_along(lst), function(i) {
          if (i == best) return("\u{2013}")
          dif <- lst[[i]]$w$pointwise - lst[[best]]$w$pointwise
          formatC(sqrt(length(dif) * var(dif)), format = "f", digits = 1)
        }, ""),
        p_waic = formatC(vapply(lst, function(z) z$w$p, 0), format = "f", digits = 1),
        check.names = FALSE
      )
      out[order(-el), ]
    }, striped = TRUE, align = "l", class = "pl-table")

    # ---- Stan code ---------------------------------------------------------------------------------------
    stan_code <- reactive({
      d <- mdata(); pr <- priors()
      pl_stan_model_program(input$model, !is.null(d$x), pr, d$K)
    })
    output$stan_code <- renderText(stan_code())
    output$r_code <- renderText({
      d <- mdata()
      pl_stan_r_code(input$model, !is.null(d$x))
    })
    output$dl_stan <- downloadHandler(
      filename = function() paste0(input$model, ".stan"),
      content = function(file) writeLines(stan_code(), file)
    )
    output$dl_data <- downloadHandler(
      filename = function() paste0(input$model, "_data.csv"),
      content = function(file) {
        d <- mdata()
        out <- data.frame(y = d$y)
        if (!is.null(d$x)) out$x <- d$x + d$x_mean
        if (!is.null(d$event)) out$event <- d$event
        write.csv(out, file, row.names = FALSE)
      }
    )
    output$dl_draws <- downloadHandler(
      filename = function() paste0(input$model, "_draws.csv"),
      content = function(file) {
        f <- fit(); dr <- f$draws
        out <- do.call(rbind, lapply(seq_len(dim(dr)[2]), function(ch) {
          m <- dr[, ch, , drop = TRUE]
          if (is.null(dim(m))) m <- matrix(m, ncol = 1)
          df <- as.data.frame(m); names(df) <- f$names
          cbind(chain = ch, iteration = seq_len(nrow(df)), df)
        }))
        write.csv(out, file, row.names = FALSE)
      }
    )
  })
}
