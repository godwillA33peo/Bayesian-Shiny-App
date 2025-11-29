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
                 h4("Credible Interval"),
                 conditionalPanel(
                   condition = "input$prior_type == 'Continuous (Beta)'",
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