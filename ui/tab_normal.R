normalUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Normal Distribution",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Observed Data"),
        numericInput(ns("normal_sample_mean"), "Sample Mean (x̄):", value = 50),
        numericInput(ns("normal_sample_size"), "Sample Size (n):", value = 30, min = 1),
        numericInput(ns("normal_known_sd"), "Known Standard Deviation (σ):", value = 10, min = 0.1),
        hr(),
        h4("Normal Prior Parameters"),
        numericInput(ns("normal_prior_mean"), "Prior Mean (μ₀):", value = 40),
        numericInput(ns("normal_prior_sd"), "Prior Standard Deviation (τ₀):", value = 15, min = 0.1),
        helpText("Prior belief about the population mean μ")
      ),
      mainPanel(
        # Helper text section
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: #fff8f0; padding: 15px; margin-bottom: 20px; border-left: 4px solid #2c3e50; border-radius: 4px;",
              h4(style = "margin-top: 0;", "How to Use This Tab"),
              p(
                strong("Purpose:"), "Explore Bayesian inference for the population mean μ when variance is known (Normal-Normal conjugate model)."
              ),
              p(
                strong("Steps:"),
                tags$ol(
                  tags$li("Set your prior belief: Prior mean (μ₀) and prior standard deviation (τ₀)"),
                  tags$li("Enter your observed data: sample mean, sample size, and known population standard deviation"),
                  tags$li("Watch the posterior distribution emerge as a compromise between prior and data"),
                  tags$li("Adjust the credible interval slider to see different uncertainty ranges")
                )
              ),
              p(
                strong("Tip:"), "Larger sample sizes give more weight to the data; stronger priors (smaller τ₀) give more weight to prior beliefs."
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
            h4("Prior vs Posterior"),
            plotOutput(ns("post_plot"))
          )
        ),
        fluidRow(
          column(
            width = 6,
            h4("Summary Table"),
            tableOutput(ns("summary_table")),
            hr(),
            h4("Point Estimate"),
            verbatimTextOutput(ns("point_estimate"))
          ),
          column(
            width = 6,
            h4("Credible Interval"),
            sliderInput(ns("normal_ci_level"), "Confidence Level",
                        min = 0.5, max = 0.99, value = 0.95, step = 0.01),
            plotOutput(ns("credible_interval_plot")),
            textOutput(ns("credible_interval_text"))
          )
        )
      )# end of Nor main panel
    )
  )
}