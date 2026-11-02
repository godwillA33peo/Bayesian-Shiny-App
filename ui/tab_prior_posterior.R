priorPosteriorUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Prior \u{2192} Posterior", value = "prior_posterior",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        selectInput(ns("lik"), "Data model",
                    choices = c("Binomial: k successes in n trials" = "binomial",
                                "Poisson: y events over exposure E" = "poisson",
                                "Normal: mean with known \u{03c3}" = "normal")),
        conditionalPanel(
          "input.lik == 'binomial'", ns = ns,
          fluidRow(column(6, numericInput(ns("b_n"), "Trials n", 20, min = 0, step = 1)),
                   column(6, numericInput(ns("b_k"), "Successes k", 14, min = 0, step = 1))),
          radioButtons(ns("b_scale"), "Put the prior on", inline = TRUE,
                       choices = c("\u{03b8} (probability)" = "natural", "logit(\u{03b8})" = "link"))
        ),
        conditionalPanel(
          "input.lik == 'poisson'", ns = ns,
          fluidRow(column(6, numericInput(ns("p_y"), "Events y", 12, min = 0, step = 1)),
                   column(6, numericInput(ns("p_E"), "Exposure E", 4, min = 0.001, step = 1))),
          helpText("E = units observed (days, students, person-years)."),
          radioButtons(ns("p_scale"), "Put the prior on", inline = TRUE,
                       choices = c("\u{03bb} (rate)" = "natural", "log(\u{03bb})" = "link"))
        ),
        conditionalPanel(
          "input.lik == 'normal'", ns = ns,
          fluidRow(column(4, numericInput(ns("n_n"), "n", 20, min = 0, step = 1)),
                   column(4, numericInput(ns("n_ybar"), "Mean \u{0233}", 52)),
                   column(4, numericInput(ns("n_sigma"), "\u{03c3}", 10, min = 0.001)))
        ),
        hr(),
        div(class = "pl-section", "Prior"),
        priorPickerUI(ns("prior"), "Family", supports = "unit", selected = "beta"),
        checkboxInput(ns("robust"), "Robust prior: mix with a vague component", FALSE),
        conditionalPanel(
          "input.robust", ns = ns,
          sliderInput(ns("w_inf"), "Prior weight on the main (informative) component",
                      min = 0.05, max = 0.99, value = 0.8, step = 0.01, ticks = FALSE),
          div(class = "pl-section", "Vague component"),
          priorPickerUI(ns("vague"), "Family", supports = "unit", selected = "beta")
        ),
        hr(),
        sliderInput(ns("level"), "Credible level", min = 0.5, max = 0.99, value = 0.95, step = 0.01),
        radioButtons(ns("interval"), "Interval", inline = TRUE,
                     choices = c("Equal-tailed" = "equal", "Highest density (HDI)" = "hdi")),
        numericInput(ns("threshold"), "Threshold c for P(\u{2009}natural parameter > c\u{2009})", 0.5),
        checkboxInput(ns("show_ref"), "Show the reference-prior posterior (dotted)", TRUE)
      ),
      mainPanel(
        width = 8,
        pl_help(
          "Prior \u{2192} Posterior: any prior, exact answers",
          p("Unlike the conjugate tabs, this one accepts ", strong("any prior family", .noWS = "after"), ", on the natural scale or on the link scale (log-odds, log-rate), including a two-part ",
            strong("robust mixture", .noWS = "after"), ". The posterior is computed exactly on a fine grid, so there is no MCMC error."),
          p(strong("Sensitivity"), " re-weights the prior and the likelihood (power-scaling) to show how much your conclusion depends on each. ",
            strong("Prior predictive"), " asks whether data like yours were plausible under the prior, which is a check for prior\u{2013}data conflict.")
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Prior \u{2192} Posterior",
            br(),
            uiOutput(ns("tiles")),
            plotOutput(ns("main_plot"), height = "400px"),
            tableOutput(ns("summary_table")),
            uiOutput(ns("ess_note"))
          ),
          tabPanel(
            "Sensitivity",
            br(),
            uiOutput(ns("sens_diag")),
            plotOutput(ns("sens_plot"), height = "380px"),
            tableOutput(ns("sens_table")),
            p(class = "pl-note",
              "Power-scaling raises the prior (or likelihood) to a power \u{03b1}: \u{03b1} < 1 weakens it, \u{03b1} > 1 strengthens it. The index is how far the posterior moves per doubling of \u{03b1}, in posterior SDs (Wasserstein distance). ",
              "Above 0.05 is flagged; in a normal model that is roughly when the component supplies more than ~20% of the information. ",
              "Idea from Kallioinen et al. (2023), ", em("Statistics and Computing"), ", implemented in the R package priorsense. Uniform priors are unchanged by power-scaling, so their index is 0.")
          ),
          tabPanel(
            "Prior predictive",
            br(),
            uiOutput(ns("ppc_diag")),
            plotOutput(ns("ppc_plot"), height = "360px"),
            p(class = "pl-note", "20,000 data sets were simulated from the prior and the model. The tail probability is two-sided: 2 \u{00d7} min(P(Y \u{2264} y), P(Y \u{2265} y)).")
          ),
          tabPanel(
            "Code",
            br(),
            h4("Stan"),
            verbatimTextOutput(ns("code_stan")),
            h4("brms"),
            verbatimTextOutput(ns("code_brms"))
          )
        )
      )
    )
  )
}
