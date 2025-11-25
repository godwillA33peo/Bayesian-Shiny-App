poissonServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # ---- Paste the server logic from app.R.backup for Normal here ----
    # For example:
    posterior <- reactive({
      # Your Bayesian update code
    })
    
    output$posterior_plot <- renderPlot({ plot(posterior()) })
    output$prior_posterior <- renderPlot({ plot_prior_vs_posterior(posterior()) })
  })
}