normalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    posterior <- reactive({
     
    })
    
    output$posterior_plot <- renderPlot({ plot(posterior()) })
    output$prior_posterior <- renderPlot({ plot_prior_vs_posterior(posterior()) })
  })
}