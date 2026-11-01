pl_quantile_row <- function(ns, i, prob, value, first = FALSE) {
  div(class = "pl-qrow",
      fluidRow(
        column(5, numericInput(ns(paste0("qp_", i)), if (first) "Probability" else NULL,
                               value = prob, min = 0, max = 1, step = 0.05)),
        column(7, numericInput(ns(paste0("qv_", i)), if (first) "Value" else NULL,
                               value = value, step = 0.1))
      ))
}

priorElicitUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Elicitation", value = "prior_elicit",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons(ns("support"), "The quantity can take",
                     choices = c("Any real value" = "real",
                                 "Positive values (an SD, a rate)" = "positive",
                                 "Values between 0 and 1 (a proportion)" = "unit"),
                     selected = "real"),
        textInput(ns("quantity"), "What are you eliciting?",
                  value = "Effect of weekly GenAI use on exam score (SD units)"),
        radioButtons(ns("method"), "Your judgements", inline = FALSE,
                     choices = c("Quantiles (recommended)" = "quantiles",
                                 "Mean and SD" = "moments")),
        conditionalPanel(
          "input.method == 'quantiles'", ns = ns,
          p(class = "pl-note", "\u{201c}I am p sure the value is below v.\u{201d} Give 2\u{2013}5 pairs; leave rows blank to skip them."),
          pl_quantile_row(ns, 1, 0.05, -0.2, first = TRUE),
          pl_quantile_row(ns, 2, 0.50, 0.1),
          pl_quantile_row(ns, 3, 0.95, 0.5),
          pl_quantile_row(ns, 4, "", ""),
          pl_quantile_row(ns, 5, "", "")
        ),
        conditionalPanel(
          "input.method == 'moments'", ns = ns,
          fluidRow(column(6, numericInput(ns("m_mean"), "Mean", 0.1, step = 0.1)),
                   column(6, numericInput(ns("m_sd"), "SD", 0.2, step = 0.05)))
        ),
        checkboxGroupInput(ns("families"), "Families to fit",
                           choices = pl_family_choices("real")[pl_family_choices("real") %in% PL_ELICIT_FAMILIES$real],
                           selected = PL_ELICIT_FAMILIES$real),
        numericInput(ns("nu"), "Degrees of freedom for Student-t / Half-t fits", value = 5, min = 1, step = 1)
      ),
      mainPanel(
        width = 8,
        pl_help(
          "Elicitation: turn expert judgement into a prior",
          p("Write down what you (or a domain expert) believe ", em("before"), " seeing the data, as a few quantiles: for example \u{201c}there is a 5% chance the effect is below \u{2212}0.2, a 50% chance below 0.1, and a 95% chance below 0.5\u{201d}. The app fits each candidate family by least squares on the cumulative probabilities, the approach used by the SHELF elicitation framework."),
          p(strong("Then check the feedback:"), " read the fitted prior's other quantiles below. If they do not match your beliefs, adjust your judgements and refit. Eliciting the median first and then the outer quantiles tends to reduce anchoring.")
        ),
        uiOutput(ns("input_messages")),
        fluidRow(
          column(6, plotOutput(ns("cdf_plot"), height = "340px")),
          column(6, plotOutput(ns("density_plot"), height = "340px"))
        ),
        h4("Fitted priors (best fit first)"),
        tableOutput(ns("fit_table")),
        p(class = "pl-note", "SSE: sum of squared differences between your probabilities and the fitted CDF at your values. Near 0 means the family can match your judgements exactly."),
        hr(),
        fluidRow(
          column(5, uiOutput(ns("pick_ui"))),
          column(7, uiOutput(ns("feedback")))
        ),
        fluidRow(
          column(5, textInput(ns("save_name"), "Save as", placeholder = "e.g. GenAI effect (elicited)")),
          column(3, style = "margin-top:25px;", actionButton(ns("save"), "Save to library", class = "btn-primary"))
        ),
        verbatimTextOutput(ns("code"))
      )
    )
  )
}
