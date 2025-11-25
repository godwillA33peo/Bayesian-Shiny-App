normalUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Normal Distribtion",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        numericInput("normal_sample_mean", "Sample Mean (x̄):", value = 50),
        numericInput("normal_sample_size", "Sample Size (n):", value = 30),
        numericInput("normal_known_sd", "Known Standard Deviation (σ):", value = 10),
        "Normal Prior Parameters",
        numericInput("normal_prior_mean", "Prior Mean (μ₀):", value = 40),
        numericInput("normal_prior_sd", "Prior Standard Deviation (σ₀):", value = 15)
      ),
      mainPanel(
      )# end of Nor main panel
    )
  )
}