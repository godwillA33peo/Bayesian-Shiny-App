source("server/mod_normal.R",   local = TRUE)
source("server/mod_poisson.R",  local = TRUE)
source("server/mod_binomial.R", local = TRUE)
source("server/mod_mcmc.R",     local = TRUE)

# Prior Lab
source("server/mod_prior_builder.R",      local = TRUE)
source("server/mod_prior_elicit.R",       local = TRUE)
source("server/mod_prior_posterior.R",    local = TRUE)
source("server/mod_prior_predictive.R",   local = TRUE)
source("server/mod_prior_hierarchical.R", local = TRUE)
source("server/mod_model_lab.R",          local = TRUE)
source("server/mod_prior_advanced.R",     local = TRUE)
source("server/mod_prior_presets.R",      local = TRUE)

server <- function(input, output, session) {
  normalServer("normal")
  poissonServer("poisson")
  binomialServer("binomial")
  mcmcServer("mcmc")

  # One prior library per user session, shared by every Prior Lab tab
  prior_library <- reactiveVal(list())
  priorBuilderServer("prior_builder", prior_library)
  priorElicitServer("prior_elicit", prior_library)
  priorPosteriorServer("prior_posterior", prior_library)
  priorPredictiveServer("prior_predictive", prior_library)
  priorHierServer("prior_hier", prior_library)
  modelLabServer("model_lab", prior_library)
  priorAdvancedServer("prior_advanced", prior_library)
  priorPresetsServer("prior_presets", prior_library)
}
