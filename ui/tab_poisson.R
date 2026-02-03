poissonUI <- function(id){
  ns <- NS(id)
  tabPanel(
    "Poisson Distribution",
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
        # Helper text section
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: #f0fff0; padding: 15px; margin-bottom: 20px; border-left: 4px solid #2c3e50; border-radius: 4px;",
              h4(style = "margin-top: 0;", "How to Use This Tab"),
              p(
                strong("Purpose:"), "Learn how Bayesian inference works for count data using the Gamma-Poisson conjugate model."
              ),
              p(
                strong("Steps:"),
                tags$ol(
                  tags$li("Choose your prior type: Continuous (Gamma distribution) or Discrete"),
                  tags$li("For Gamma prior: Set the mean and standard deviation reflecting your prior belief about the rate parameter λ"),
                  tags$li("Enter your count data: number of observations and total counts"),
                  tags$li("Observe how the data updates your prior to form the posterior distribution")
                )
              ),
              p(
                strong("Example:"), "If counting customer arrivals per hour, λ represents the average arrival rate."
              )
            )
          )
        ),
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
                   condition = "input.poisson_prior_type == 'Continuous (Gamma)'",
                   h4("Confidence Interval Posterior"),
                   plotOutput(ns("ci_plot"))) # conditional only for the Gamma (Continuous) Cases
                 )
                 
        )
      )# end of Poi main panel
    )
  )

}