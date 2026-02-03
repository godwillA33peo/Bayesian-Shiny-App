binomialUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Binomial Distribution",
    sidebarLayout(
      sidebarPanel( 
        width = 3,
        "Prior Beliefs (Prior)",
        radioButtons(ns("prior_type"), "Type of prior (cont/discrete)",
                    choices = c("Continuous (Beta)", "Discrete"), 
                    selected = "Continuous (Beta)"),
        "Observed Data",
        sliderInput(ns("n_trails"), label = "Number of trails", min = 0,
                    max = 100, value = 10),
        sliderInput(ns("x_successes"), "No of successes (x):", min = 0, max = 100, 
                    value = 6),
        "Beta Distribution Parameters",
        sliderInput(ns("alpha"), "Alpha", min = 1, max=100, value=4, step=1),
        sliderInput(ns("beta"), "Beta", min=1, max=100, value=4, step=1),
        "Discrte Prior Parameters",
        textInput(ns("discrete_thetas"), "Enter Theta Values (csv):", 
                  value = "0.05, 0.2, 0.35, 0.5, 0.65"),
        textInput(ns("prior"), "Enter prior Values (sum to 1):", 
                  value = "0.05, 0.05, 0.35, 0.5, 0.05")
      ),
      mainPanel(
        # Helper text section
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: #f0f8ff; padding: 15px; margin-bottom: 20px; border-left: 4px solid #2c3e50; border-radius: 4px;",
              h4(style = "margin-top: 0;", "How to Use This Tab"),
              p(
                strong("Purpose:"), "Explore how prior beliefs combine with observed data to form posterior distributions in binomial models."
              ),
              p(
                strong("Steps:"),
                tags$ol(
                  tags$li("Choose your prior type: Continuous (Beta distribution) or Discrete"),
                  tags$li("For Beta prior: Adjust α and β parameters to reflect your prior belief about the success probability"),
                  tags$li("Enter your observed data: number of trials and successes"),
                  tags$li("View how the prior updates to the posterior based on your data")
                )
              ),
              p(
                strong("Tip:"), "Try different prior parameters to see how 'strong' vs 'weak' priors affect the posterior."
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            h4("Prior Distribution"),
            plotOutput(ns("prior_plot"))
          ),
          column(
            width = 6,
            h4("Posterior Prior Plot"),
            plotOutput(ns("post_plot"))
          )
        ),
        fluidRow( # this row outputs 4 or 3 more squares for the estimates and simulations
          column(width = 6,
                 conditionalPanel(
                   condition = "input$prior_type == 'Continuous (Beta)'",
                   fluidRow(
                     
                     column(width = 12,
                            h4("Data table"),
                            tableOutput(ns("summary_table")))
                     #column(width = 9,
                     #      h4("Point Estimates")
                     #     )
                   ),
                   fluidRow( # container for the 3rd quadrant with simulated data and inference from the simulations
                     column(width=12,
                            conditionalPanel(
                              condition = "input$prior_type == 'Continuous (Beta)'",
                              fluidRow(
                                #condition = "input$prior_type == 'Continuous (Beta)'",
                                sliderInput(ns("n_sims"), label = "Number of simulations", min = 0,
                                            max = 100000, step = 10000 , value = 10000)
                                #textOutput("point_estimates") # single line tetx output for the point estimate for simulated data
                                
                              ),
                              fluidRow(
                                sliderInput(ns("sim_ci_level"), label = "level",
                                            min = 0.5, max = 0.99, step = 0.01, value=0.95),
                                textOutput(ns("sim_ci_level_text"))#single line out put for confidence interval from the simulated data
                              ),
                              fluidRow()
                            )
                     )
                   )# end of container for the subdivided quadrant with inference for simulated data 
                 )),
          column(width = 6,
                 conditionalPanel(
                   condition = "input$prior_type == 'Continuous (Beta)'",
                   h4("Credible Interval"),
                   sliderInput(ns("c_level"), "Confidence Interval", min=0.5, max = 0.99,
                               value=0.95, step=0.01),
                   plotOutput(ns("conf_int"))
                   
                 )
          )
        )# end if Beta main panel
    )
   )
  )
}