priorPredictiveUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Prior Predictive (regression)", value = "prior_predictive",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        selectInput(ns("family"), "Model",
                    choices = c("Logistic regression (binary outcome)" = "bernoulli",
                                "Poisson regression (counts, log link)" = "poisson",
                                "Linear regression (continuous outcome)" = "gaussian")),
        sliderInput(ns("K"), "Number of standardised predictors", min = 0, max = 20, value = 5, step = 1),
        div(class = "pl-section", "Intercept prior"),
        priorPickerUI(ns("int"), "Family", supports = "real", selected = "normal"),
        div(class = "pl-section", "Prior for each slope"),
        priorPickerUI(ns("slope"), "Family", supports = "real", selected = "normal"),
        conditionalPanel(
          "input.family == 'gaussian'", ns = ns,
          div(class = "pl-section", "Residual SD prior"),
          priorPickerUI(ns("sigma"), "Family", supports = "positive", selected = "exponential")
        ),
        hr(),
        div(class = "pl-section", "What is plausible? (domain knowledge)"),
        conditionalPanel(
          "input.family == 'gaussian'", ns = ns,
          fluidRow(column(6, numericInput(ns("y_min"), "Lowest plausible y", 0)),
                   column(6, numericInput(ns("y_max"), "Highest plausible y", 100)))
        ),
        conditionalPanel(
          "input.family == 'poisson'", ns = ns,
          numericInput(ns("rate_max"), "Highest plausible mean count", 50, min = 0)
        ),
        conditionalPanel(
          "input.family == 'bernoulli'", ns = ns,
          sliderInput(ns("p_edge"), "Call a probability extreme if below / above",
                      min = 0.001, max = 0.1, value = 0.01, step = 0.001)
        ),
        fluidRow(
          column(7, selectInput(ns("ndraw"), "Prior draws", choices = c(1000, 4000, 10000), selected = 4000)),
          column(5, style = "margin-top:25px;", actionButton(ns("redraw"), "Draw again"))
        )
      ),
      mainPanel(
        width = 8,
        pl_help(
          "Prior predictive simulation: check your priors on the outcome scale",
          p("Regression priors live on the coefficient scale, but what you actually believe is about ", strong("outcomes"),
            ": pass rates, counts, scores. This tab simulates from the priors alone (no data) so you can see what they imply."),
          p(strong("Key lesson:"), " priors on several coefficients add up. With standardised predictors the linear predictor has SD \u{2248} \u{221a}(SD\u{00b2}_intercept + K \u{00d7} SD\u{00b2}_slope). Each Normal(0, 2.5) prior looks mild on its own, but five of them on the log-odds scale imply that most people have probabilities near 0 or 1. Move the predictor slider and watch."),
          p(class = "pl-note", "Predictors are simulated as independent standard normals (as if you had standardised them). The same check in brms: fit with sample_prior = \"only\", then run pp_check().")
        ),
        uiOutput(ns("tiles")),
        fluidRow(
          column(6, plotOutput(ns("mu_plot"), height = "320px")),
          column(6, plotOutput(ns("lines_plot"), height = "320px"))
        ),
        fluidRow(
          column(6, plotOutput(ns("dataset_plot"), height = "300px")),
          column(6, plotOutput(ns("coef_plot"), height = "300px"))
        ),
        h4("brms code for the same priors"),
        verbatimTextOutput(ns("code"))
      )
    )
  )
}
