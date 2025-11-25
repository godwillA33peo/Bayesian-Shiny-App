mcmcUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "MCMC Simulations",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        fileInput(
          "mcmc_data_file", "Upload Data File (CSV):",
          accept = c(".csv")
        ),
        selectInput(
          "mcmc_model_type", "Select Model Type:",
          choices = c("Selected Model ......" = "", "Binomial", "Poisson", "Normal"),
          selected = ""
        ),
        uiOutput("model_inputs"),
        "MCMC Settings",
        numericInput("mcmc_iterations", "Number of Iterations:", value = 1000, min = 100, step = 100),
        numericInput("mcmc_burnin", "Burn-in Period:", value = 200, min = 0, step = 50),
        numericInput("mcmc_thin", "Thinning Interval:", value = 5, min = 1, step = 1),
        numericInput("n_chains", "Number of Chains:", value = 3, min = 1, step = 1)
      ),
      
      mainPanel(
      ) # end of MCMC main panel
    )
  )
}