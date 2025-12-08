poissonUI <- function(id){
  ns <- NS(id)
  tabPanel(
    "Poison Distribtuion",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(ns("poisson_prior_type"), label = "Type of prior (continuous/discrete)",
                     choices = c("Continuous (Gamma)", "Discrete"), 
                     selected = "Continuous (Gamma)"),
        "Gamma Distribution Parameters",
        sliderInput(ns("mean"), "Mean", min = 1, max = 100, step = 1, value = 5),
        sliderInput(ns("sd"), "Standard Deviation", min = 1, max = 100, step = 1, value = 10),
        numericInput(ns("n_observations"),"Number of Observations", value = 5, min = 1),
        helpText("Total samples, days, or units observed"),
        numericInput(ns("n_counts"), "Total Counts", value= 20, min = 1),
        helpText("Total # of events recorded"),
        "Discrte Prior Parameters",
        textInput(ns("poisson_discrete_thetas"), "Enter Theta Values (csv):", 
                  value = "1,2,3,4,5"),
        textInput(ns("poisson_prior"), "Enter prior Values (sum to 1):", 
                  value = "0.2, 0.2, 0.2, 0.2, 0.2")
      ),
      mainPanel(
        fluidRow(
          column(width = 6,
                 h4("Prior Plot"),
                 plotOutput(ns("prior_plot"))
                 ),
          column(width = 6,
                 h4("Posterior Plot"),
                 plotOutput(ns("post_plot"))
                 )
        ),
        fluidRow(
          column(width = 6,
                 h4("Data Table"),
                 tableOutput(ns("summary_table"))
                 ),
          column(width = 6,
                 conditionalPanel(
                   condition = "input$poission_prior_type =='Continuous (Gamma)'",
                   h4("Confidence Interval Posterior"),
                   plotOutput(ns("ci_plot"))) # conditional only for the Gamma (Continuous) Cases
                 )
                 
        )
      )# end of Poi main panel
    )
  )

}