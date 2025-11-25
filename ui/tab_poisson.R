poissonUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Poison Distribtuion",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        radioButtons(inputId = "poisson_prior_type", label = "Type of prior (continuous/discrete)",
                     choices = c("Continuous", "Discrete"), 
                     selected = "Continunous (Gamma)"),
        sliderInput(inputId = "poisson_observed_counts", label = "Observed Counts (x)", 
                    min = 0, max = 100, value = 10),
        "Gamma Distribution Parameters",
        sliderInput("poisson_alpha", "Alpha", min = 1, max=100, value=4, step=1),
        sliderInput("poisson_beta", "Beta", min=1, max=100, value=4, step=1),
        "Discrte Prior Parameters",
        textInput("poisson_discrete_thetas", "Enter Theta Values (csv):", 
                  value = "0.05, 0.2, 0.35, 0.5, 0.65"),
        textInput("poisson_prior", "Enter prior Values (sum to 1):", 
                  value = "0.05, 0.05, 0.35, 0.5, 0.05")
      ),
      mainPanel(
      )# end of Poi main panel
    )
  )
}