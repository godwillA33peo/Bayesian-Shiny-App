modelLabUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Model Lab (priors \u{2192} data \u{2192} Stan)", value = "model_lab",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        selectInput(ns("model"), "Data model", choices = pl_model_choices(), selected = "ordinal"),
        radioButtons(ns("source"), "Data", inline = TRUE,
                     choices = c("Example data" = "example", "Upload CSV" = "upload")),
        conditionalPanel(
          "input.source == 'upload'", ns = ns,
          fileInput(ns("file"), NULL, accept = ".csv", buttonLabel = "Choose CSV\u{2026}"),
          uiOutput(ns("col_ui"))
        ),
        uiOutput(ns("data_note")),
        checkboxInput(ns("use_x"), "Include the predictor x", TRUE),
        hr(),
        div(style = "display:flex; justify-content:space-between; align-items:baseline;",
            h4("Priors"),
            actionLink(ns("reset"), "Reset to data-scaled defaults")),
        p(class = "pl-note", "Defaults are weakly informative and scaled to your data, in the spirit of rstanarm. Change any of them and the prior predictive check updates immediately."),
        uiOutput(ns("prior_ui")),
        hr(),
        sliderInput(ns("n_iter"), "Draws per chain (4 chains, plus 1,000 warm-up each)",
                    min = 500, max = 5000, value = 2000, step = 500, ticks = FALSE),
        checkboxInput(ns("auto"), "Refit automatically when anything changes", TRUE),
        actionButton(ns("fit"), "Fit model", class = "btn-primary", style = "width:100%;")
      ),
      mainPanel(
        width = 8,
        pl_help(
          "Model Lab: from priors to a fitted model and Stan code",
          p("Pick a data model, use the example data or upload your own, and set a prior for every parameter. The app checks the priors against your data (",
            strong("prior predictive"), "), fits the model with its own MCMC sampler (4 chains, adaptive Metropolis, R-hat and ESS as in Stan), checks the fit (",
            strong("posterior predictive"), "), and writes the ", strong("Stan program"), " so you can run the same model in cmdstanr or rstan."),
          p(class = "pl-note", "The in-app sampler is meant for exploring priors with small models (up to 3,000 rows). For final analyses run the generated Stan code, which uses Stan's more efficient NUTS sampler.")
        ),
        uiOutput(ns("status")),
        tabsetPanel(
          id = ns("tabs"), type = "tabs",
          tabPanel(
            "Data & prior predictive", value = "prior",
            br(),
            fluidRow(
              column(6, plotOutput(ns("data_plot"), height = "330px")),
              column(6, plotOutput(ns("prior_pc_plot"), height = "330px"))
            ),
            h4("Do data like yours look plausible under the priors?"),
            tableOutput(ns("prior_pc_table")),
            p(class = "pl-note", "Each row compares a statistic of your data with the same statistic in 200 data sets simulated from the priors. A tail probability near 0 or 1 means the priors find your data surprising.")
          ),
          tabPanel(
            "Posterior", value = "posterior",
            br(),
            uiOutput(ns("effect_tiles")),
            tableOutput(ns("post_table")),
            p(class = "pl-note", "Contraction = 1 \u{2212} Var(posterior)/Var(prior): near 1 means the data dominate the prior for that parameter; near 0 means the prior still dominates. R-hat should be below 1.01 and ESS above about 400 (Vehtari et al., 2021)."),
            plotOutput(ns("post_plot"), height = "420px")
          ),
          tabPanel(
            "Posterior predictive", value = "ppc",
            br(),
            plotOutput(ns("post_pc_plot"), height = "380px"),
            tableOutput(ns("post_pc_table"))
          ),
          tabPanel(
            "Diagnostics", value = "diag",
            br(),
            plotOutput(ns("trace_plot"), height = "460px"),
            tableOutput(ns("diag_table"))
          ),
          tabPanel(
            "Compare models", value = "compare",
            br(),
            p("Fit several models to the same data (e.g. Poisson vs negative binomial vs zero-inflated Poisson), add each one here, and compare their expected predictive accuracy (WAIC; higher elpd is better)."),
            fluidRow(
              column(5, actionButton(ns("add_compare"), "Add the current fit", class = "btn-primary")),
              column(4, actionButton(ns("clear_compare"), "Clear", class = "btn-default"))
            ),
            br(),
            tableOutput(ns("compare_table")),
            p(class = "pl-note", "\u{0394}elpd is relative to the best model; a difference smaller than about 2 \u{00d7} its SE is not a clear preference. For final work use PSIS-LOO via the loo package on the Stan fit.")
          ),
          tabPanel(
            "Stan code", value = "stan",
            br(),
            fluidRow(
              column(4, downloadButton(ns("dl_stan"), "Download model.stan")),
              column(4, downloadButton(ns("dl_data"), "Download data (CSV)")),
              column(4, downloadButton(ns("dl_draws"), "Download posterior draws"))
            ),
            br(),
            verbatimTextOutput(ns("stan_code")),
            h4("Run it in R"),
            verbatimTextOutput(ns("r_code"))
          )
        )
      )
    )
  )
}
