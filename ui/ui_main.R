source("ui/tab_normal.R",   local = TRUE)
source("ui/tab_poisson.R",  local = TRUE)
source("ui/tab_binomial.R", local = TRUE)
source("ui/tab_mcmc.R",     local = TRUE)
source("ui/tab_about.R",    local = TRUE)

# Prior Lab
source("ui/tab_prior_builder.R",      local = TRUE)
source("ui/tab_prior_elicit.R",       local = TRUE)
source("ui/tab_prior_posterior.R",    local = TRUE)
source("ui/tab_prior_predictive.R",   local = TRUE)
source("ui/tab_prior_hierarchical.R", local = TRUE)
source("ui/tab_model_lab.R",          local = TRUE)
source("ui/tab_prior_advanced.R",     local = TRUE)
source("ui/tab_prior_presets.R",      local = TRUE)

ui <- navbarPage(
  title = "Bayesian Explorer",
  id = "main_nav",
  #header = shinythemes::themeSelector(),
  theme = shinytheme("cosmo"),
  header = tags$head(tags$style(HTML(PL_CSS))),

  aboutUI("about"),
  binomialUI("binomial"),
  poissonUI("poisson"),
  normalUI("normal"),
  mcmcUI("mcmc"),
  navbarMenu(
    "Prior Lab",
    "Explore priors",
    priorBuilderUI("prior_builder"),
    priorAdvancedUI("prior_advanced"),
    priorPresetsUI("prior_presets"),
    priorElicitUI("prior_elicit"),
    "----",
    "Check priors",
    priorPosteriorUI("prior_posterior"),
    priorPredictiveUI("prior_predictive"),
    priorHierUI("prior_hier"),
    "----",
    "Fit models",
    modelLabUI("model_lab")
  )
)
