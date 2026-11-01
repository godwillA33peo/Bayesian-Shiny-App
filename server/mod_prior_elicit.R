priorElicitServer <- function(id, library) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    examples <- list(
      real = list(q = "Effect of weekly GenAI use on exam score (SD units)",
                  vals = c(-0.2, 0.1, 0.5), mean = 0.1, sd = 0.2),
      positive = list(q = "Between-module SD of pass rates (log-odds)",
                      vals = c(0.1, 0.4, 1.2), mean = 0.5, sd = 0.35),
      unit = list(q = "Proportion of students using GenAI weekly",
                  vals = c(0.35, 0.55, 0.75), mean = 0.55, sd = 0.12)
    )
    default_selected <- list(
      real = PL_ELICIT_FAMILIES$real,
      positive = c("gamma", "lognormal", "half_normal", "half_t"),
      unit = PL_ELICIT_FAMILIES$unit
    )
    # Fixed colour + line type per family (colour follows the family, never its rank)
    fam_cols <- c(normal = "#2a78d6", student_t = "#eb6834", logistic = "#0f9b7a", laplace = "#a03fb0",
                  gamma = "#2a78d6", lognormal = "#eb6834", inv_gamma = "#0f9b7a",
                  half_normal = "#a03fb0", half_t = "#eda100", exponential = "#e87ba4",
                  beta = "#2a78d6")
    fam_lty <- c(normal = "solid", student_t = "longdash", logistic = "dashed", laplace = "dotdash",
                 gamma = "solid", lognormal = "longdash", inv_gamma = "dashed",
                 half_normal = "dotdash", half_t = "twodash", exponential = "dotted",
                 beta = "solid")

    observeEvent(input$support, {
      sup <- input$support
      ex <- examples[[sup]]
      updateTextInput(session, "quantity", value = ex$q)
      probs <- c(0.05, 0.5, 0.95)
      for (i in 1:3) {
        updateNumericInput(session, paste0("qp_", i), value = probs[i])
        updateNumericInput(session, paste0("qv_", i), value = ex$vals[i])
      }
      for (i in 4:5) {
        updateNumericInput(session, paste0("qp_", i), value = "")
        updateNumericInput(session, paste0("qv_", i), value = "")
      }
      updateNumericInput(session, "m_mean", value = ex$mean)
      updateNumericInput(session, "m_sd", value = ex$sd)
      ch <- pl_family_choices(sup)
      ch <- ch[ch %in% PL_ELICIT_FAMILIES[[sup]]]
      updateCheckboxGroupInput(session, "families", choices = ch, selected = default_selected[[sup]])
    }, ignoreInit = TRUE)

    judgements <- reactive({
      sup <- input$support
      req(input$method)
      if (input$method == "quantiles") {
        num <- function(v) if (is.null(v) || length(v) == 0 || is.na(v)) NA_real_ else as.numeric(v)
        p <- vapply(1:5, function(i) num(input[[paste0("qp_", i)]]), numeric(1))
        v <- vapply(1:5, function(i) num(input[[paste0("qv_", i)]]), numeric(1))
        keep <- !is.na(p) & !is.na(v)
        p <- p[keep]; v <- v[keep]
        validate(need(length(p) >= 2, "Give at least two probability\u{2013}value pairs."))
        validate(need(all(p > 0 & p < 1), "Probabilities must be strictly between 0 and 1."))
        o <- order(p); p <- p[o]; v <- v[o]
        validate(need(!anyDuplicated(p), "Use each probability only once."))
        validate(need(all(diff(v) > 0), "Values must increase as the probabilities increase (a higher probability means a larger value)."))
        if (sup == "positive") validate(need(all(v > 0), "For a positive quantity, every value must be above 0."))
        if (sup == "unit") validate(need(all(v > 0 & v < 1), "For a proportion, every value must be between 0 and 1."))
        list(method = "quantiles", probs = p, vals = v)
      } else {
        m <- input$m_mean; s <- input$m_sd
        validate(need(is.finite(m) && is.finite(s) && s > 0, "Enter a mean and a positive SD."))
        if (sup == "positive") validate(need(m > 0, "For a positive quantity the mean must be above 0."))
        if (sup == "unit") validate(need(m > 0 && m < 1, "For a proportion the mean must be between 0 and 1."))
        list(method = "moments", mean = m, sd = s)
      }
    })

    fits <- reactive({
      j <- judgements()
      fams <- input$families
      validate(need(length(fams) > 0, "Tick at least one family to fit."))
      nu <- input$nu
      validate(need(is.finite(nu) && nu > 0, "Degrees of freedom must be positive."))
      out <- list(); notes <- character(0)
      for (f in fams) {
        fixed <- if (f %in% c("student_t", "half_t")) list(nu = nu) else list()
        res <- if (j$method == "quantiles") {
          tryCatch(pl_fit_quantiles(f, j$probs, j$vals, fixed), error = function(e) NULL)
        } else {
          pl_fit_moments(f, j$mean, j$sd, fixed)
        }
        if (is.null(res) || is.null(res$prior)) {
          why <- if (!is.null(res$why)) res$why else "fit failed"
          notes <- c(notes, paste0(pl_dist(f)$short, ": ", why))
          next
        }
        if (!is.null(pl_check_prior(res$prior))) {
          notes <- c(notes, paste0(pl_dist(f)$short, ": no valid fit"))
          next
        }
        out[[f]] <- list(family = f, prior = res$prior, sse = res$sse)
      }
      validate(need(length(out) > 0, paste(c("No family could be fitted.", notes), collapse = " ")))
      if (j$method == "quantiles") out <- out[order(vapply(out, `[[`, 0, "sse"))]
      list(fits = out, notes = notes, j = j)
    })

    output$input_messages <- renderUI({
      ft <- pl_try(fits())
      if (inherits(ft, "error") || !length(ft$notes)) return(NULL)
      pl_diag("warn", "Not fitted:", paste(ft$notes, collapse = " \u{00b7} "))
    })

    chosen <- reactive({
      ft <- fits()
      ch <- input$chosen
      if (is.null(ch) || is.null(ft$fits[[ch]])) ch <- names(ft$fits)[1]
      ft$fits[[ch]]
    })

    plot_range <- reactive({
      ft <- fits()
      rng <- pl_union_range(lapply(ft$fits, `[[`, "prior"))
      if (ft$j$method == "quantiles") {
        v <- ft$j$vals
        w <- diff(range(v))
        rng <- range(c(rng, min(v) - 0.3 * w, max(v) + 0.3 * w))
      }
      sup <- input$support
      if (sup == "positive") rng[1] <- 0
      if (sup == "unit") rng <- c(0, 1)
      rng
    })

    curves <- reactive({
      ft <- fits()
      rng <- plot_range()
      ch <- chosen()$family
      df <- do.call(rbind, lapply(ft$fits, function(f) {
        d <- pl_density_df(f$prior, rng)
        d$family <- pl_dist(f$family)$short
        d$key <- f$family
        d$chosen <- f$family == ch
        d
      }))
      df$family <- factor(df$family, levels = vapply(ft$fits, function(f) pl_dist(f$family)$short, ""))
      df
    })

    fam_scales <- function(ft) {
      keys <- names(ft$fits)
      labs <- vapply(keys, function(k) pl_dist(k)$short, "")
      list(scale_colour_manual(values = setNames(fam_cols[keys], labs), breaks = labs),
           scale_linetype_manual(values = setNames(fam_lty[keys], labs), breaks = labs),
           scale_linewidth_manual(values = c(`TRUE` = 1.5, `FALSE` = 0.8), guide = "none"))
    }

    output$cdf_plot <- renderPlot({
      ft <- fits()
      df <- curves()
      p <- ggplot(df, aes(x = x, y = cdf, colour = family, linetype = family,
                          linewidth = as.character(chosen))) +
        geom_line(na.rm = TRUE) +
        fam_scales(ft) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(x = input$quantity, y = "P(value \u{2264} x)", title = "Fitted CDFs vs your judgements") +
        pl_theme(13)
      if (ft$j$method == "quantiles") {
        p <- p + geom_point(data = data.frame(x = ft$j$vals, cdf = ft$j$probs),
                            aes(x = x, y = cdf), inherit.aes = FALSE,
                            size = 3.5, shape = 21, fill = "#0b0b0b", colour = "white", stroke = 1)
      }
      p
    })

    output$density_plot <- renderPlot({
      ft <- fits()
      df <- curves()
      cap <- pl_density_cap(split(df$density, df$family))
      p <- ggplot(df, aes(x = x, y = density, colour = family, linetype = family,
                          linewidth = as.character(chosen))) +
        geom_line(na.rm = TRUE) +
        fam_scales(ft) +
        coord_cartesian(ylim = c(0, cap)) +
        labs(x = input$quantity, y = "Density", title = "Fitted prior densities") +
        pl_theme(13)
      if (ft$j$method == "quantiles") {
        p <- p + geom_vline(xintercept = ft$j$vals, colour = PL_INK, linetype = "dotted", linewidth = 0.4)
      }
      p
    })

    output$fit_table <- renderTable({
      ft <- fits()
      qs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
      rows <- lapply(ft$fits, function(f) {
        d <- pl_dist(f$family)
        q <- pl_q(f$prior, qs)
        r <- data.frame(
          Family = d$short,
          Parameters = paste(sprintf("%s = %s", names(f$prior$params), pl_fmt(f$prior$params)), collapse = ", "),
          SSE = if (is.na(f$sse)) "\u{2013}" else formatC(f$sse, format = "e", digits = 1),
          check.names = FALSE
        )
        for (i in seq_along(qs)) r[[paste0(100 * qs[i], "%")]] <- pl_fmt(q[i], 3)
        r$Mean <- pl_fmt(d$mean(f$prior$params), 3)
        r$SD <- pl_fmt(d$sd(f$prior$params), 3)
        r
      })
      do.call(rbind, rows)
    }, striped = TRUE, align = "l", class = "pl-table")

    output$pick_ui <- renderUI({
      ft <- fits()
      keys <- names(ft$fits)
      labs <- vapply(keys, function(k) pl_dist(k)$short, "")
      cur <- isolate(input$chosen)
      sel <- if (!is.null(cur) && cur %in% keys) cur else keys[1]
      radioButtons(ns("chosen"), "Use this fit", choices = setNames(keys, labs), selected = sel)
    })

    output$feedback <- renderUI({
      f <- chosen()
      q <- pl_q(f$prior, c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
      fm <- function(x) pl_fmt(x, 3)
      div(
        class = "pl-diag ok",
        span(class = "tag", "Feedback:"),
        sprintf("Under %s, ", pl_prior_label(f$prior)),
        tags$ul(
          tags$li(sprintf("the median is %s;", fm(q[4]))),
          tags$li(sprintf("the value is as likely to be inside [%s, %s] as outside it (50%% interval);", fm(q[3]), fm(q[5]))),
          tags$li(sprintf("there is a 1-in-10 chance it is below %s and a 1-in-10 chance it is above %s;", fm(q[2]), fm(q[6]))),
          tags$li(sprintf("90%% of the prior lies in [%s, %s].", fm(q[1]), fm(q[7])))
        ),
        "Does that match what you believe? If not, revise your judgements."
      )
    })

    output$code <- renderText({
      f <- chosen()
      sprintf("# %s\n# brms (choose the class that fits your model):\nprior(%s, class = \"b\")\n# Stan:\ntheta ~ %s;",
              input$quantity, pl_stan_string(f$prior), pl_stan_string(f$prior))
    })

    observeEvent(input$save, {
      f <- pl_try(chosen())
      if (inherits(f, "error")) {
        showNotification("Fix the judgements first: there is no valid fit to save.", type = "error")
        return()
      }
      nm <- trimws(input$save_name)
      if (!nzchar(nm)) nm <- paste0(input$quantity, ": ", pl_prior_label(f$prior))
      library(pl_library_add(library(), nm, f$prior, paste("Elicitation:", input$quantity)))
      updateTextInput(session, "save_name", value = "")
      showNotification(paste0("Saved \u{201c}", nm, "\u{201d} to your prior library."), type = "message")
    })
  })
}
