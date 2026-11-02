source("ui/tab_normal.R",   local = TRUE)
source("ui/tab_poisson.R",  local = TRUE)
source("ui/tab_binomial.R", local = TRUE)
source("ui/tab_mcmc.R",     local = TRUE)

# Prior Lab tools
source("ui/tab_prior_builder.R",      local = TRUE)
source("ui/tab_prior_elicit.R",       local = TRUE)
source("ui/tab_prior_posterior.R",    local = TRUE)
source("ui/tab_prior_predictive.R",   local = TRUE)
source("ui/tab_prior_hierarchical.R", local = TRUE)
source("ui/tab_prior_advanced.R",     local = TRUE)
source("ui/tab_prior_presets.R",      local = TRUE)
source("ui/tab_model_lab.R",          local = TRUE)

# Home page and the top-level page layouts
source("ui/tab_home.R",         local = TRUE)
source("ui/tab_basics_intro.R", local = TRUE)
source("ui/pages.R",            local = TRUE)

ui <- bslib::page_navbar(
  title = "Bayesian Explorer",
  id = "main_nav",
  window_title = "Bayesian Explorer",
  theme = app_theme(),
  fillable = FALSE,
  header = tags$head(
    tags$style(HTML(PL_CSS)),
    tags$style(HTML(APP_CSS)),
    tags$script(HTML(APP_GOTO_JS))
  ),
  bslib::nav_panel("Home", value = "home", homeUI("home")),
  bslib::nav_panel("Bayesian Basics", value = "basics", basicsPage()),
  bslib::nav_panel("MCMC Simulations", value = "mcmc", mcmcPage()),
  bslib::nav_panel("Prior Lab", value = "prior_lab", priorLabPage()),
  bslib::nav_panel("Model Lab", value = "model_lab", modelLabPage()),
  bslib::nav_spacer(),
  bslib::nav_item(tags$a(href = APP_REPO_URL, target = "_blank", rel = "noopener", class = "nav-link", "GitHub"))
)
