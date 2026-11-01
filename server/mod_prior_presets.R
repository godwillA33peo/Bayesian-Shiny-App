pl_mini_density <- function(priors, labels, xlab, transform = NULL) {
  df <- do.call(rbind, lapply(seq_along(priors), function(i) {
    d <- pl_density_df(priors[[i]], pl_union_range(priors))
    if (!is.null(transform)) d$x <- transform(d$x)
    d$what <- labels[i]
    d
  }))
  df$what <- factor(df$what, levels = labels)
  cols <- setNames(unname(PL_SLOT_COLS[seq_along(labels)]), labels)
  ltys <- setNames(unname(PL_SLOT_LTY[seq_along(labels)]), labels)
  ggplot(df, aes(x, density, colour = what, linetype = what)) +
    geom_line(linewidth = 1.1, na.rm = TRUE) +
    scale_colour_manual(values = cols) + scale_linetype_manual(values = ltys) +
    coord_cartesian(ylim = c(0, pl_density_cap(split(df$density, df$what)))) +
    labs(x = xlab, y = NULL) +
    pl_theme(12) + theme(legend.position = if (length(labels) > 1) "top" else "none")
}

priorPresetsServer <- function(id, library) {
  moduleServer(id, function(input, output, session) {

    preset_ids <- function(dom) {
      ids <- seq_along(PL_PRESETS)
      if (dom != "All") ids <- ids[vapply(PL_PRESETS, `[[`, "", "domain") == dom]
      ids
    }

    observeEvent(input$domain, ignoreInit = TRUE, {
      ids <- preset_ids(input$domain)
      ch <- setNames(as.character(ids), vapply(PL_PRESETS[ids], function(p) paste0(p$name, "  \u{2013}  ", pl_prior_label(p$prior)), ""))
      keep <- intersect(input$pick, as.character(ids))
      if (!length(keep)) keep <- as.character(ids[seq_len(min(2, length(ids)))])
      updateCheckboxGroupInput(session, "pick", choices = ch, selected = keep)
    })
    observeEvent(input$tick_none, updateCheckboxGroupInput(session, "pick", selected = character(0)))

    output$preset_table <- renderTable({
      ids <- preset_ids(input$domain)
      data.frame(
        Domain = vapply(PL_PRESETS[ids], `[[`, "", "domain"),
        Preset = vapply(PL_PRESETS[ids], `[[`, "", "name"),
        `For` = vapply(PL_PRESETS[ids], `[[`, "", "param"),
        Prior = vapply(PL_PRESETS[ids], function(p) pl_prior_label(p$prior), ""),
        Stan = vapply(PL_PRESETS[ids], function(p) pl_stan_string(p$prior), ""),
        Why = vapply(PL_PRESETS[ids], `[[`, "", "why"),
        check.names = FALSE
      )
    }, striped = TRUE, align = "l", class = "pl-table")

    output$preset_plot <- renderPlot({
      ids <- as.integer(input$pick)
      validate(need(length(ids) >= 1, "Tick one or more presets to plot them."),
               need(length(ids) <= 8, "Tick at most 8 presets to plot."))
      df <- do.call(rbind, lapply(ids, function(i) {
        p <- PL_PRESETS[[i]]
        d <- pl_density_df(p$prior, pl_auto_range(p$prior))
        cap <- pl_density_cap(list(d$density))
        d$density <- pmin(d$density, cap)
        d$panel <- paste0(p$name, "\n", pl_prior_label(p$prior))
        d
      }))
      df$panel <- factor(df$panel, levels = unique(df$panel))
      ggplot(df, aes(x, density)) +
        geom_area(fill = PL_COLS[["Prior"]], alpha = 0.15, na.rm = TRUE) +
        geom_line(colour = PL_COLS[["Prior"]], linewidth = 1, na.rm = TRUE) +
        facet_wrap(~panel, scales = "free", ncol = min(4, length(ids))) +
        labs(x = NULL, y = NULL) +
        pl_theme(11) + theme(axis.text.y = element_blank(), strip.text = element_text(size = 10))
    })

    output$refs <- renderUI({
      tags$ol(lapply(unname(PL_REFERENCES), function(r) tags$li(class = "pl-note", r)))
    })

    observeEvent(input$add, {
      ids <- as.integer(input$pick)
      if (!length(ids)) { showNotification("Tick at least one preset.", type = "warning"); return() }
      lib <- library()
      for (i in ids) {
        p <- PL_PRESETS[[i]]
        lib <- pl_library_add(lib, paste0(p$name, " (", p$domain, ")"), p$prior, paste("Preset:", p$param))
      }
      library(lib)
      showNotification(sprintf("Saved %d preset(s) to your library.", length(ids)), type = "message")
    })

    save_one <- function(name, prior, source) {
      library(pl_library_add(library(), name, prior, source))
      showNotification(paste0("Saved \u{201c}", name, "\u{201d}."), type = "message")
    }
    prior_text <- function(pr, extra = NULL) {
      p(class = "pl-note", strong(pl_prior_label(pr)), "  \u{00b7}  Stan: ", code(pl_stan_string(pr)), extra)
    }

    # ---- trial priors -------------------------------------------------------------
    tr <- reactive({
      validate(need(is.finite(input$tr_ratio) && input$tr_ratio > 0 && input$tr_ratio != 1, "The ratio must be positive and not 1."),
               need(is.finite(input$tr_tail) && input$tr_tail > 0 && input$tr_tail < 0.5, "The tail probability must be between 0 and 0.5."))
      pl_build_trial_priors(input$tr_ratio, input$tr_tail)
    })
    output$tr_plot <- renderPlot({
      t <- tr()
      pl_mini_density(list(t$sceptical, t$enthusiastic), c("Sceptical", "Enthusiastic"), "Ratio (log axis)", transform = exp) +
        scale_x_log10(labels = function(b) pl_fmt(b, 2)) +
        geom_vline(xintercept = c(1, input$tr_ratio), colour = PL_INK, linetype = "dotted")
    })
    output$tr_text <- renderUI({
      t <- tr()
      tagList(prior_text(t$sceptical, sprintf(" on the log ratio (sceptical): P(ratio beyond %s) = %s", pl_fmt(input$tr_ratio), pl_pct(input$tr_tail))),
              prior_text(t$enthusiastic, sprintf(" (enthusiastic): P(no benefit) = %s", pl_pct(input$tr_tail))))
    })
    observeEvent(input$tr_save, {
      t <- pl_try(tr()); if (inherits(t, "error")) return()
      save_one(sprintf("Sceptical, ratio %s", pl_fmt(input$tr_ratio)), t$sceptical, "Quick builder: trial prior (log ratio)")
      save_one(sprintf("Enthusiastic, ratio %s", pl_fmt(input$tr_ratio)), t$enthusiastic, "Quick builder: trial prior (log ratio)")
    })

    # ---- PC prior -----------------------------------------------------------------------
    pc <- reactive({
      validate(need(is.finite(input$pc_U) && input$pc_U > 0, "U must be positive."),
               need(is.finite(input$pc_alpha) && input$pc_alpha > 0 && input$pc_alpha < 1, "\u{03b1} must be between 0 and 1."))
      pl_build_pc_sd(input$pc_U, input$pc_alpha)
    })
    output$pc_plot <- renderPlot({
      pl_mini_density(list(pc()), "PC prior", "\u{03c3}") +
        geom_vline(xintercept = input$pc_U, colour = PL_INK, linetype = "dotted")
    })
    output$pc_text <- renderUI(prior_text(pc(), sprintf("  \u{00b7}  median %s", pl_fmt(pl_q(pc(), 0.5), 3))))
    observeEvent(input$pc_save, {
      pr <- pl_try(pc()); if (inherits(pr, "error")) return()
      save_one(sprintf("PC prior: P(SD > %s) = %s", pl_fmt(input$pc_U), pl_fmt(input$pc_alpha)), pr, "Quick builder: PC prior")
    })

    # ---- Beta from mean and n --------------------------------------------------------------
    bp <- reactive({
      validate(need(is.finite(input$bp_mean) && input$bp_mean > 0 && input$bp_mean < 1, "The mean must be between 0 and 1."),
               need(is.finite(input$bp_n) && input$bp_n > 0, "n must be positive."))
      pl_prior("beta_proportion", input$bp_mean, input$bp_n)
    })
    output$bp_plot <- renderPlot(pl_mini_density(list(bp()), "Beta", "Proportion"))
    output$bp_text <- renderUI({
      pr <- bp(); q <- pl_q(pr, c(0.05, 0.95))
      prior_text(pr, sprintf("  \u{00b7}  = Beta(%s, %s)  \u{00b7}  90%% interval %s",
                             pl_fmt(input$bp_mean * input$bp_n), pl_fmt((1 - input$bp_mean) * input$bp_n), pl_interval_str(q[1], q[2])))
    })
    observeEvent(input$bp_save, {
      pr <- pl_try(bp()); if (inherits(pr, "error")) return()
      save_one(sprintf("Beta: mean %s, worth %s obs", pl_fmt(input$bp_mean), pl_fmt(input$bp_n)), pr, "Quick builder: Beta from mean and n")
    })

    # ---- effect size ---------------------------------------------------------------------------
    es <- reactive({
      validate(need(is.finite(input$es_d) && input$es_d > 0, "d must be positive."),
               need(is.finite(input$es_p) && input$es_p > 0 && input$es_p < 1, "p must be between 0 and 1."))
      pl_build_effect(input$es_d, input$es_p)
    })
    output$es_plot <- renderPlot({
      pl_mini_density(list(es()), "Effect size", "Standardised effect (SD units)") +
        geom_vline(xintercept = c(-input$es_d, input$es_d), colour = PL_INK, linetype = "dotted")
    })
    output$es_text <- renderUI(prior_text(es()))
    observeEvent(input$es_save, {
      pr <- pl_try(es()); if (inherits(pr, "error")) return()
      save_one(sprintf("Effect size: %s%% within \u{00b1}%s SD", round(100 * input$es_p), pl_fmt(input$es_d)), pr, "Quick builder: effect size")
    })
  })
}
