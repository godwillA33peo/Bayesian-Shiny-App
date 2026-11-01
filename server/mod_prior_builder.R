priorBuilderServer <- function(id, library) {
  moduleServer(id, function(input, output, session) {

    slots <- c("A", "B", "C", "D")
    inits <- list(
      A = pl_prior("normal", 0, 1),
      B = pl_prior("normal", 0, 2.5),
      C = pl_prior("student_t", 3, 0, 2.5),
      D = pl_prior("normal", 0, 10)
    )
    pickers <- setNames(lapply(slots, function(s) {
      priorPickerServer(paste0("pick_", s), init = inits[[s]], library = library)
    }), slots)

    # ---- active priors ------------------------------------------------------------
    slot_state <- reactive({
      active <- list(); msgs <- character(0)
      for (s in slots) {
        if (!isTRUE(input[[paste0("show_", s)]])) next
        pr <- pl_try(pickers[[s]]$prior())
        if (inherits(pr, "error")) {
          m <- pl_err_msg(pr)
          if (!is.null(m)) msgs <- c(msgs, paste0("Prior ", s, ": ", m))
          next
        }
        lab <- trimws(input[[paste0("label_", s)]] %||% "")
        if (!nzchar(lab)) lab <- pl_prior_label(pr)
        active[[s]] <- list(slot = s, prior = pr, label = paste0(s, ": ", lab), short = lab)
      }
      list(active = active, msgs = msgs)
    })

    active <- reactive({
      st <- slot_state()
      validate(need(length(st$active) > 0,
                    if (length(st$msgs)) paste(st$msgs, collapse = "\n") else "Tick \u{201c}Show this prior\u{201d} for at least one prior."))
      st$active
    })

    output$slot_messages <- renderUI({
      st <- slot_state()
      if (!length(st$msgs)) return(NULL)
      pl_diag("warn", "Check inputs:", paste(st$msgs, collapse = " \u{00b7} "))
    })

    x_range <- reactive({
      act <- active()
      if (isTRUE(input$manual_range)) {
        validate(need(is.finite(input$xmin) && is.finite(input$xmax) && input$xmax > input$xmin,
                      "Manual range: \u{2018}To\u{2019} must be greater than \u{2018}From\u{2019}."))
        return(c(input$xmin, input$xmax))
      }
      pl_union_range(lapply(act, `[[`, "prior"))
    })

    # Monte Carlo draws, shared by the implied-scale plot, table and download
    draws <- reactive({
      act <- active()
      set.seed(20261101)
      lapply(act, function(a) pl_r(a$prior, 10000))
    })

    series_scales <- function(act) {
      labs <- vapply(act, `[[`, "", "label")
      sl <- vapply(act, `[[`, "", "slot")
      list(
        scale_colour_manual(values = setNames(PL_SLOT_COLS[sl], labs), breaks = labs),
        scale_linetype_manual(values = setNames(PL_SLOT_LTY[sl], labs), breaks = labs)
      )
    }

    # ---- density / CDF plot ---------------------------------------------------------
    output$density_plot <- renderPlot({
      act <- active()
      rng <- x_range()
      dfs <- lapply(act, function(a) {
        df <- pl_density_df(a$prior, rng)
        df$series <- a$label
        df
      })
      df <- do.call(rbind, dfs)
      df$series <- factor(df$series, levels = vapply(act, `[[`, "", "label"))
      is_cdf <- identical(input$curve, "cdf")
      df$y <- if (is_cdf) df$cdf else df$density
      cap <- if (is_cdf) 1.02 else pl_density_cap(lapply(dfs, `[[`, "density"))
      c0 <- input$threshold

      p <- ggplot(df, aes(x = x, y = y, colour = series, linetype = series)) +
        geom_line(linewidth = 1.1, na.rm = TRUE) +
        series_scales(act) +
        coord_cartesian(ylim = c(0, cap), xlim = rng, expand = FALSE) +
        labs(x = "\u{03b8} (parameter scale)", y = if (is_cdf) "P(\u{03b8} \u{2264} x)" else "Density",
             title = if (is_cdf) "Cumulative distribution" else "Prior densities") +
        pl_theme()
      if (is.finite(c0) && c0 > rng[1] && c0 < rng[2]) {
        p <- p + geom_vline(xintercept = c0, colour = PL_INK, linetype = "dotted", linewidth = 0.5)
      }
      p
    })

    # ---- implied scale -----------------------------------------------------------------
    implied <- reactive({
      tr <- input$transform
      req(tr, tr != "none")
      act <- active()
      dr <- draws()
      f <- PL_TRANSFORMS[[tr]]$f
      out <- lapply(names(act), function(s) {
        z <- suppressWarnings(f(dr[[s]]))
        z[is.nan(z)] <- NA
        list(label = act[[s]]$label, short = act[[s]]$short, z = z)
      })
      names(out) <- names(act)
      out
    })

    output$implied_plot <- renderPlot({
      tr <- input$transform
      req(tr, tr != "none")
      act <- active()
      imp <- implied()
      info <- PL_TRANSFORMS[[tr]]
      df <- do.call(rbind, lapply(imp, function(it) {
        z <- it$z[is.finite(it$z)]
        if (info$log_axis) z <- pmin(pmax(z, 1e-6), 1e6)
        data.frame(series = it$label, z = z)
      }))
      df$series <- factor(df$series, levels = vapply(act, `[[`, "", "label"))
      is_prob <- tr %in% c("inv_logit", "probit")
      p <- ggplot(df, aes(x = z, colour = series, linetype = series)) +
        geom_freqpoly(aes(y = after_stat(density)), bins = 60, linewidth = 1.1,
                      boundary = if (is_prob) 0 else NULL, na.rm = TRUE) +
        series_scales(act) +
        labs(x = info$axis, y = "Density",
             title = "Implied prior after the transform",
             subtitle = "From 10,000 draws per prior. Spikes at the edges mean the prior piles mass on extreme values.") +
        pl_theme()
      if (info$log_axis) p <- p + scale_x_log10(labels = function(b) pl_fmt(b, 3))
      if (is_prob) p <- p + scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))
      p
    })

    output$implied_table <- renderTable({
      tr <- input$transform
      req(tr, tr != "none")
      imp <- implied()
      lvl <- input$level
      a <- (1 - lvl) / 2
      rows <- lapply(imp, function(it) {
        z <- it$z[is.finite(it$z)]
        q <- quantile(z, c(a, 0.5, 1 - a), names = FALSE)
        ex <- pl_extreme_mass(tr, it$z)
        r <- data.frame(Prior = it$label, Median = pl_fmt(q[2], 3),
                        Interval = pl_interval_str(q[1], q[3]), check.names = FALSE)
        names(r)[3] <- sprintf("%d%% interval", round(100 * lvl))
        if (!is.null(ex)) r[[ex[["label"]]]] <- pl_pct(as.numeric(ex[["value"]]))
        r
      })
      do.call(rbind, rows)
    }, striped = TRUE, align = "l", class = "pl-table")

    # ---- summary table -------------------------------------------------------------------
    output$summary_table <- renderTable({
      act <- active()
      lvl <- input$level
      c0 <- input$threshold
      rows <- lapply(act, function(a) {
        s <- pl_summary(a$prior, lvl, if (is.finite(c0)) c0 else NULL)
        r <- data.frame(
          Prior = a$label,
          Mean = pl_fmt(s$Mean, 3), SD = pl_fmt(s$SD, 3), Median = pl_fmt(s$Median, 3),
          Interval = pl_interval_str(s$Lower, s$Upper),
          check.names = FALSE
        )
        names(r)[5] <- sprintf("%d%% interval", round(100 * lvl))
        if (!is.null(s$`P(> c)`)) r[[sprintf("P(\u{03b8} > %s)", pl_fmt(c0))]] <- pl_pct(s$`P(> c)`)
        r
      })
      do.call(rbind, rows)
    }, striped = TRUE, align = "l", class = "pl-table")

    # ---- code ------------------------------------------------------------------------------
    output$code_brms <- renderText({
      act <- active()
      cls <- input$brms_class
      if (identical(cls, "none")) return("# Choose a brms class above to get brms syntax.")
      lines <- vapply(act, function(a) {
        sprintf("  %s,  # %s%s", pl_brms_line(a$prior, cls), a$label, pl_brms_note(a$prior))
      }, "")
      lines[length(lines)] <- sub(",  #", "   #", lines[length(lines)], fixed = TRUE)
      paste0("# Pick ONE of these for your model; they are alternatives.\n",
             "priors <- c(\n", paste(lines, collapse = "\n"), "\n)\n",
             "# brm(y ~ x, data = d, prior = priors[1], sample_prior = \"only\")  # prior predictive check")
    })

    output$code_stan <- renderText({
      act <- active()
      paste(vapply(act, function(a) sprintf("%s  // %s", pl_stan_line(a$prior, "theta"), a$label), ""),
            collapse = "\n")
    })

    output$code_r <- renderText({
      act <- active()
      paste0("n <- 10000\n",
             paste(vapply(act, function(a) {
               sprintf("%s  # %s", pl_r_line(a$prior, paste0("theta_", a$slot)), a$label)
             }, ""), collapse = "\n"))
    })

    output$download_draws <- downloadHandler(
      filename = function() "prior_draws.csv",
      content = function(file) {
        act <- active(); dr <- draws()
        tr <- input$transform
        f <- PL_TRANSFORMS[[tr]]$f
        out <- do.call(rbind, lapply(names(act), function(s) {
          data.frame(prior = act[[s]]$label, draw = seq_along(dr[[s]]), theta = dr[[s]],
                     implied = if (tr != "none") suppressWarnings(f(dr[[s]])) else NA_real_)
        }))
        names(out)[4] <- paste0("implied_", tr)
        write.csv(out, file, row.names = FALSE)
      }
    )

    # ---- library -------------------------------------------------------------------------------
    observeEvent(input$save, {
      s <- input$save_slot
      pr <- pl_try(pickers[[s]]$prior())
      if (inherits(pr, "error")) {
        showNotification(paste("Prior", s, "is not valid yet."), type = "error")
        return()
      }
      nm <- trimws(input$save_name)
      if (!nzchar(nm)) {
        lab <- trimws(input[[paste0("label_", s)]] %||% "")
        nm <- if (nzchar(lab)) lab else pl_prior_label(pr)
      }
      library(pl_library_add(library(), nm, pr, "Prior Builder"))
      updateTextInput(session, "save_name", value = "")
      showNotification(paste0("Saved \u{201c}", nm, "\u{201d} to your prior library."), type = "message")
    })

    output$library_table <- renderTable({
      df <- pl_library_df(library())
      validate(need(nrow(df) > 0, "Your library is empty. Save a prior above, or from the Elicitation tab."))
      df
    }, striped = TRUE, align = "l", class = "pl-table")

    output$download_library <- downloadHandler(
      filename = function() "prior_library.csv",
      content = function(file) write.csv(pl_library_export(library()), file, row.names = FALSE)
    )

    observeEvent(input$upload_library, {
      f <- input$upload_library
      req(f)
      res <- tryCatch({
        df <- read.csv(f$datapath, stringsAsFactors = FALSE)
        pl_library_import(library(), df)
      }, error = function(e) e)
      if (inherits(res, "error")) {
        showNotification(conditionMessage(res), type = "error")
      } else {
        n <- attr(res, "n_imported")
        attr(res, "n_imported") <- NULL
        library(res)
        showNotification(sprintf("Imported %d prior(s).", n), type = "message")
      }
    })

    observeEvent(input$clear_library, {
      library(list())
    })
  })
}
