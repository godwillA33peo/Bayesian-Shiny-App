source("ui/tab_normal.R",   local = TRUE)
source("ui/tab_poisson.R",  local = TRUE)
source("ui/tab_binomial.R", local = TRUE)
source("ui/tab_mcmc.R",     local = TRUE)
source("ui/tab_about.R",    local = TRUE)

ui <- navbarPage(
  title = "Bayesian Explorer",
  #header = shinythemes::themeSelector(),
  theme = shinytheme("cosmo"),

  aboutUI("about"),
  binomialUI("binomial"),
  poissonUI("poisson"),
  normalUI("normal"),
  mcmcUI("mcmc")

)
