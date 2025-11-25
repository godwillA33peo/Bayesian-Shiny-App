source("ui/tab_normal.R",   local = TRUE)
source("ui/tab_poisson.R",  local = TRUE)
source("ui/tab_binomial.R", local = TRUE)
source("ui/tab_mcmc.R",     local = TRUE)

ui <- fluidPage(
  titlePanel("Bayesian Explorer"),
  theme = shinytheme("readable"),
  tabsetPanel(
    binomialUI("binomial"),
    poissonUI("poisson"),
    normalUI("normal"),
    mcmcUI("mcmc")
  )
)
