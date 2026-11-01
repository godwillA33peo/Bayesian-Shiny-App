priorAdvancedUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Advanced priors (shrinkage, Dirichlet, covariance)", value = "prior_advanced",
    tabsetPanel(
      type = "tabs",

      # ---- shrinkage ----------------------------------------------------------------------
      tabPanel(
        "Shrinkage priors for many predictors",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            sliderInput(ns("sh_p"), "Number of predictors p", min = 2, max = 200, value = 50, step = 1),
            sliderInput(ns("sh_p0"), "Predictors you expect to matter p\u{2080}", min = 1, max = 50, value = 5, step = 1),
            checkboxGroupInput(ns("sh_types"), "Priors to compare (up to 4)", choices = setNames(names(PL_SHRINK), PL_SHRINK),
                               selected = c("normal", "laplace", "horseshoe", "r2d2")),
            div(class = "pl-section", "Settings"),
            fluidRow(column(6, numericInput(ns("sh_normal_sd"), "Normal SD", 1, min = 0.01, step = 0.1)),
                     column(6, numericInput(ns("sh_laplace_b"), "Laplace scale", 0.7, min = 0.01, step = 0.1))),
            fluidRow(column(6, numericInput(ns("sh_n"), "Sample size n (horseshoe)", 200, min = 2, step = 10)),
                     column(6, numericInput(ns("sh_slab"), "Slab scale (horseshoe)", 2, min = 0.01, step = 0.5))),
            fluidRow(column(6, numericInput(ns("sh_r2_mean"), "R2D2: prior mean R\u{00b2}", 0.3, min = 0.01, max = 0.99, step = 0.05)),
                     column(6, numericInput(ns("sh_r2_prec"), "R2D2: precision", 2, min = 0.1, step = 0.5))),
            fluidRow(column(6, numericInput(ns("sh_slab_sd"), "Spike-slab: slab SD", 1, min = 0.01, step = 0.1)),
                     column(6, numericInput(ns("sh_spike_sd"), "Spike SD", 0.01, min = 0.0001, step = 0.01))),
            helpText("Predictors are standardised and the residual SD is 1, so coefficients are in SD units.")
          ),
          mainPanel(
            width = 8,
            pl_help(
              "When you have many predictors, the prior on all of them together matters",
              p("Independent Normal priors on many coefficients add up: with 50 standardised predictors and Normal(0, 1) on each, the prior says R\u{00b2} is almost certainly above 0.95. ",
                strong("Shrinkage priors"), " say instead that most effects are near zero and a few may be large."),
              tags$ul(
                tags$li(strong("Regularised horseshoe"), " (Piironen & Vehtari, 2017): a global scale set from how many predictors you expect to matter, with a slab that caps the largest effects."),
                tags$li(strong("R2D2"), " (Zhang et al., 2022): put the prior directly on R\u{00b2}, then share it out between predictors."),
                tags$li(strong("Spike-and-slab:"), " each coefficient is either essentially zero or drawn from a wider slab.")
              )
            ),
            uiOutput(ns("sh_tiles")),
            fluidRow(
              column(6, plotOutput(ns("sh_r2_plot"), height = "330px")),
              column(6, plotOutput(ns("sh_beta_plot"), height = "330px"))
            ),
            tableOutput(ns("sh_table")),
            h4("brms code"),
            verbatimTextOutput(ns("sh_code"))
          )
        )
      ),

      # ---- Dirichlet -------------------------------------------------------------------------
      tabPanel(
        "Dirichlet (shares that sum to 1)",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            textInput(ns("dir_alpha"), "Concentration parameters \u{03b1} (comma-separated)", value = "4, 3, 2, 1"),
            textInput(ns("dir_names"), "Category names (optional)", value = "Never, Monthly, Weekly, Daily"),
            div(class = "pl-section", "Quick settings for the current number of categories"),
            div(style = "display:flex; flex-wrap:wrap; gap:6px;",
                actionButton(ns("dir_flat"), "Flat (all 1)", class = "btn-default btn-sm"),
                actionButton(ns("dir_jeff"), "Jeffreys (all \u{00bd})", class = "btn-default btn-sm"),
                actionButton(ns("dir_sparse"), "Sparse (all 0.2)", class = "btn-default btn-sm"),
                actionButton(ns("dir_x10"), "10\u{00d7} more confident", class = "btn-default btn-sm")),
            helpText("The sum \u{03b1}\u{2080} = \u{03a3}\u{03b1} acts like a prior sample size; the expected share of category k is \u{03b1}\u{2096}/\u{03b1}\u{2080}.")
          ),
          mainPanel(
            width = 8,
            pl_help(
              "Dirichlet: a prior for several proportions at once",
              p("Use it for the shares of a categorical outcome (e.g. how often students use GenAI), mixture weights, or brms monotonic effects (", code("mo()"), "). Each share on its own has a Beta(\u{03b1}\u{2096}, \u{03b1}\u{2080} \u{2212} \u{03b1}\u{2096}) prior.")
            ),
            plotOutput(ns("dir_marg_plot"), height = "300px"),
            plotOutput(ns("dir_draws_plot"), height = "300px"),
            tableOutput(ns("dir_table")),
            verbatimTextOutput(ns("dir_code"))
          )
        )
      ),

      # ---- covariance ------------------------------------------------------------------------
      tabPanel(
        "Covariance: Inverse-Wishart vs LKJ",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            sliderInput(ns("cv_K"), "Dimension K (e.g. random intercept + slopes)", min = 2, max = 6, value = 2),
            div(class = "pl-section", "Inverse-Wishart(\u{03bd}, s\u{00b2} I)"),
            fluidRow(column(6, numericInput(ns("cv_nu"), "df \u{03bd} (\u{2265} K)", 3, min = 2, step = 1)),
                     column(6, numericInput(ns("cv_s"), "Scale s", 1, min = 0.01, step = 0.1))),
            div(class = "pl-section", "LKJ(\u{03b7}) + independent SD prior"),
            numericInput(ns("cv_eta"), "LKJ shape \u{03b7}", 2, min = 0.1, step = 0.5),
            priorPickerUI(ns("cv_sd"), "SD prior", supports = "positive", selected = "half_normal")
          ),
          mainPanel(
            width = 8,
            pl_help(
              "Why modern software prefers LKJ to the Inverse-Wishart",
              p("The Inverse-Wishart ties variances and correlations together: draws with large standard deviations tend to come with strong correlations, and the prior for each SD is hard to control. LKJ separates them: a prior on the correlation matrix plus a prior on each SD, chosen independently (see Alvarez, Niemi & Simpson, 2014).")
            ),
            uiOutput(ns("cv_tiles")),
            plotOutput(ns("cv_scatter"), height = "360px"),
            fluidRow(
              column(6, plotOutput(ns("cv_r_plot"), height = "280px")),
              column(6, plotOutput(ns("cv_sd_plot"), height = "280px"))
            ),
            verbatimTextOutput(ns("cv_code"))
          )
        )
      )
    )
  )
}
