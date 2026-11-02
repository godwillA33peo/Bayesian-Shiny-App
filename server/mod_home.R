HOME_PRIORS <- list(flat = c(1, 1), sceptical = c(3, 7), hopeful = c(7, 3))

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    counts <- reactiveValues(s = 6, f = 3)
    observeEvent(input$pass, counts$s <- counts$s + 1)
    observeEvent(input$fail, counts$f <- counts$f + 1)
    observeEvent(input$reset, { counts$s <- 0; counts$f <- 0 })

    post <- reactive({
      ab <- HOME_PRIORS[[input$prior %||% "flat"]]
      list(a0 = ab[1], b0 = ab[2], a = ab[1] + counts$s, b = ab[2] + counts$f)
    })

    output$demo_plot <- renderPlot({
      p <- post()
      x <- seq(0.0025, 0.9975, length.out = 400)
      prior <- dbeta(x, p$a0, p$b0); postd <- dbeta(x, p$a, p$b)
      ci <- qbeta(c(0.025, 0.975), p$a, p$b)
      shade <- data.frame(x = x, y = postd)[x >= ci[1] & x <= ci[2], ]
      df <- rbind(data.frame(x = x, y = prior, what = "Prior"),
                  data.frame(x = x, y = postd, what = "Posterior after your data"))
      df$what <- factor(df$what, levels = c("Prior", "Posterior after your data"))
      ggplot(df, aes(x, y, colour = what, linetype = what)) +
        geom_area(data = shade, aes(x, y), inherit.aes = FALSE, fill = PL_COLS[["Posterior"]], alpha = 0.16) +
        geom_vline(xintercept = 0.5, colour = "#c9ced4", linetype = "dotted") +
        geom_line(linewidth = 1.2) +
        scale_colour_manual(values = c(PL_COLS[["Prior"]], PL_COLS[["Posterior"]])) +
        scale_linetype_manual(values = c("dashed", "solid")) +
        scale_x_continuous(labels = function(b) paste0(100 * b, "%"), breaks = seq(0, 1, 0.25)) +
        labs(x = "Pass rate", y = NULL) +
        pl_theme(12) +
        theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(),
              plot.background = element_rect(fill = "white", colour = NA))
    }, res = 96)

    output$demo_data <- renderText(sprintf("Data: %d passed, %d failed", counts$s, counts$f))

    output$demo_tiles <- renderUI({
      p <- post()
      ci <- qbeta(c(0.025, 0.975), p$a, p$b)
      pct <- function(v) paste0(round(100 * v), "%")
      div(class = "pl-tiles", style = "margin:0.6rem 0 0;",
          pl_tile(pct(p$a / (p$a + p$b)), "Posterior mean"),
          pl_tile(paste0(pct(ci[1]), "–", pct(ci[2])), "95% credible interval"),
          pl_tile(pct(1 - pbeta(0.5, p$a, p$b)), "P(rate > 50%)"))
    })

    output$field_cards <- renderUI({
      ex <- HOME_FIELDS[[input$field %||% "education"]]
      do.call(bslib::layout_column_wrap, c(
        list(width = "300px", fill = FALSE),
        lapply(ex, function(e) {
          div(class = "home-example",
              div(class = "q", e$q),
              div(class = "how", e$how),
              div(class = "where", pl_goto(paste(e$where, "→"), e$page, e$sub, model = e$model, domain = e$domain)))
        })))
    })

    observeEvent(input$cite, {
      yr <- format(Sys.Date(), "%Y")
      showModal(modalDialog(
        title = "How to cite Bayesian Explorer",
        p("If the app helped your work or teaching, please cite it as:"),
        tags$pre(class = "pl-code", style = "white-space:pre-wrap;",
                 sprintf("Zulu, G. (%s). Bayesian Explorer: an interactive app for learning Bayesian inference and experimenting with priors [R Shiny application]. %s", yr, APP_REPO_URL)),
        easyClose = TRUE, footer = modalButton("Close")))
    })
  })
}
