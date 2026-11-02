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
source("server/mod_home.R",               local = TRUE)
source("server/mod_basics_intro.R",       local = TRUE)

server <- function(input, output, session) {
  homeServer("home")
  normalServer("normal")
  poissonServer("poisson")
  basicsIntroServer("basics_intro")
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

  # In-app navigation: Home cards, overview cards and Back/Next links send
  # list(page, sub, model, domain) via elements with class "pl-goto"
  observeEvent(input$pl_goto, {
    g <- input$pl_goto
    if (!is.null(g$page)) bslib::nav_select("main_nav", g$page)
    if (!is.null(g$sub)) {
      sub_nav <- switch(g$page, basics = "basics_tabs", prior_lab = "prior_lab_tabs", NULL)
      if (!is.null(sub_nav)) bslib::nav_select(sub_nav, g$sub)
    }
    if (!is.null(g$model)) {
      updateRadioButtons(session, "model_lab-source", selected = "example")
      updateSelectInput(session, "model_lab-model", selected = g$model)
    }
    if (!is.null(g$domain)) updateRadioButtons(session, "prior_presets-domain", selected = g$domain)
  })
}
