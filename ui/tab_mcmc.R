mcmcUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "MCMC Simulations",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Data Upload"),
        fileInput(
          ns("mcmc_data_file"), "Upload Data File (CSV):",
          accept = c(".csv")
        ),
        helpText("Upload a CSV file containing your data"),
        hr(),
        h4("Model Selection"),
        selectInput(
          ns("mcmc_model_type"), "Select Model Type:",
          choices = c("Select Model..." = "", "Binomial", "Poisson", "Normal"),
          selected = ""
        ),
        uiOutput(ns("model_inputs")),
        hr(),
        h4("MCMC Settings"),
        actionButton(ns("run_mcmc"), "Run MCMC",
                     class = "btn-primary",
                     style = "width: 100%; margin-bottom: 15px;"),
        hr(),
        numericInput(ns("mcmc_iterations"), "Number of Iterations:",
                     value = 5000, min = 100, step = 100),
        numericInput(ns("mcmc_burnin"), "Burn-in Period:",
                     value = 1000, min = 0, step = 50),
        numericInput(ns("mcmc_thin"), "Thinning Interval:",
                     value = 2, min = 1, step = 1),
        numericInput(ns("n_chains"), "Number of Chains:",
                     value = 3, min = 1, max = 5, step = 1),
        helpText("Multiple chains help assess convergence")
      ),

      mainPanel(
        # Helper text section
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: #fff0f5; padding: 15px; margin-bottom: 20px; border-left: 4px solid #2c3e50; border-radius: 4px;",
              h4(style = "margin-top: 0;", "How to Use This Tab"),
              p(
                strong("Purpose:"), "Learn about Markov Chain Monte Carlo (MCMC) sampling for Bayesian inference when analytical solutions are difficult."
              ),
              p(
                strong("Steps:"),
                tags$ol(
                  tags$li("Upload your data as a CSV file"),
                  tags$li("Select the appropriate model type (Binomial, Poisson, or Normal)"),
                  tags$li("Set MCMC parameters: iterations (more = better approximation), burn-in (discard early samples), and thinning (reduce autocorrelation)"),
                  tags$li("Run multiple chains to check convergence"),
                  tags$li("Examine the trace plot to ensure chains have converged (no trends, good mixing)")
                )
              ),
              p(
                strong("What is MCMC?"), "MCMC is a sampling method that generates random draws from the posterior distribution. Instead of calculating the exact posterior, we approximate it by drawing many samples."
              ),
              p(
                strong("Convergence:"), "Chains have converged when they overlap well and show no trends. Look for 'hairy caterpillar' patterns in trace plots."
              )
            )
          )
        ),
        tabsetPanel(
          type = "tabs",

          # Data Preview Tab
          tabPanel(
            "Data Preview",
            h4("Uploaded Data (First 10 Rows)"),
            tableOutput(ns("data_preview"))
          ),

          # Trace Plot Tab
          tabPanel(
            "Trace Plot",
            h4("MCMC Trace Plot"),
            helpText("Visual check for convergence. Chains should mix well and show no trends."),
            plotOutput(ns("trace_plot"), height = "500px")
          ),

          # Density Plots Tab
          tabPanel(
            "Posterior Density",
            fluidRow(
              column(
                width = 12,
                h4("Density by Chain"),
                helpText("Each chain should produce similar distributions."),
                plotOutput(ns("density_plot"), height = "400px")
              )
            ),
            fluidRow(
              column(
                width = 12,
                h4("Combined Posterior Density"),
                helpText("All chains combined. Red line shows posterior mean."),
                plotOutput(ns("combined_density_plot"), height = "400px")
              )
            )
          ),

          # Diagnostics Tab
          tabPanel(
            "Diagnostics",
            h4("Autocorrelation Plot"),
            helpText("Autocorrelation should decay quickly. High autocorrelation suggests poor mixing."),
            plotOutput(ns("autocorr_plot"), height = "400px"),
            hr(),
            h4("Summary Statistics"),
            verbatimTextOutput(ns("mcmc_summary"))
          )
        )
      ) # end of MCMC main panel
    )
  )
}