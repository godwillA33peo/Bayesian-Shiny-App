# Bayesian Basics > "What is Bayes?": the teaching tab

BAYES_WHY <- list(
  list(t = "Answers the question you actually asked",
       b = "A posterior gives direct statements such as “there is a 93% probability the new teaching method raises pass rates”, rather than a p-value about hypothetical repeated samples."),
  list(t = "Uses what is already known, openly",
       b = "Earlier studies, pilot data or expert judgement enter as a prior that is written down and can be criticised, instead of being left out or used informally."),
  list(t = "Copes with small samples and many groups",
       b = "Hierarchical models share information across schools, modules or hospitals (partial pooling), so small groups borrow strength from larger ones."),
  list(t = "Carries uncertainty all the way through",
       b = "Predictions, costs and decisions inherit the full uncertainty of the parameters, not just a point estimate plugged in."),
  list(t = "Learns sequentially",
       b = "Today’s posterior is tomorrow’s prior, so evidence can be updated as each cohort, trial phase or survey wave arrives."),
  list(t = "Flexible models, one logic",
       b = "Counts, proportions, Likert scales, survival times, measurement error: every model is fitted and interpreted the same way, through the posterior.")
)

BAYES_COMPARE <- list(
  c("What probability means", "Long-run frequency of events over repeated samples", "Degree of belief (plausibility), given the information available"),
  c("Unknown parameters", "Fixed constants; not given probabilities", "Uncertain quantities, described by probability distributions"),
  c("What goes in", "Data and a model", "Data, a model and a prior"),
  c("What comes out", "Point estimate, confidence interval, p-value", "The whole posterior distribution, and any summary of it"),
  c("A 95% interval means", "The method captures the true value in 95% of repeated samples", "Given the model, prior and data, a 95% probability that the value lies in the interval"),
  c("Testing an idea", "p-value: how surprising the data would be if the null hypothesis were true", "Posterior probability of the hypothesis itself, e.g. P(effect > 0 | data)")
)

BAYES_GLOSSARY <- list(
  c("Prior", "The distribution describing what you believe about a parameter before seeing the data."),
  c("Likelihood", "How probable the observed data are for each possible value of the parameter."),
  c("Posterior", "The updated distribution after combining prior and likelihood: posterior ∝ likelihood × prior."),
  c("Credible interval", "A range containing the parameter with a stated posterior probability (e.g. 95%)."),
  c("Posterior predictive distribution", "What new data should look like, given everything learned; used to check a model and to predict."),
  c("Conjugate prior", "A prior that gives a posterior in the same family, so the update has an exact formula (Beta–Binomial, Gamma–Poisson, Normal–Normal)."),
  c("Weakly informative prior", "A prior that rules out implausible values while letting the data dominate."),
  c("MCMC", "Algorithms that draw samples from the posterior when there is no exact formula."),
  c("Prior sensitivity analysis", "Refitting with reasonable alternative priors to see whether the conclusions change.")
)

basicsIntroUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "What is Bayes?", value = "intro",
    div(style = "max-width: 1180px;",

        # ---- the idea -------------------------------------------------------------------
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
          div(
            h3("Bayesian statistics in one sentence"),
            p(style = "font-size:1.15rem;",
              "Bayesian statistics treats anything you are unsure about as a probability distribution, and uses Bayes’ theorem to ",
              strong("update that distribution when new data arrive", .noWS = "after"), "."),
            p("You start with a ", strong("prior"), " (what you believe before the data), the data supply a ", strong("likelihood"),
              " (how well each possible value explains what you saw), and the result is the ", strong("posterior", .noWS = "after"),
              ": your updated belief, which you can summarise, plot or use to make predictions and decisions."),
            tags$pre(class = "pl-code", style = "font-size:1rem;",
                     "posterior  ∝  likelihood  ×  prior\nP(θ | data) ∝ P(data | θ) × P(θ)")
          ),
          bslib::card(bslib::card_body(
            h4("A familiar example"),
            p("A new revision programme is trialled with 12 students and 9 pass. Was it better than the usual 60% pass rate?"),
            tags$ul(
              tags$li(strong("Frequentist:"), " a binomial test against 60% gives p ≈ 0.38 (two-sided): data like these would not be unusual if the true rate were 60%, so the result is “not significant”."),
              tags$li(strong("Bayesian:"), " with a flat prior, the posterior probability that the programme's pass rate exceeds 60% is about 83%, and a 95% credible interval runs from about 46% to 91%.")),
            p(class = "pl-note", "Same data, two kinds of answer. The Bayesian one is a direct probability about the quantity you care about, together with a range of plausible values.")
          ))
        ),

        # ---- why it helps ------------------------------------------------------------------
        h3(style = "margin-top:1.8rem;", "Why it is useful"),
        do.call(bslib::layout_column_wrap, c(
          list(width = "320px", fill = FALSE),
          lapply(BAYES_WHY, function(w) bslib::card(bslib::card_body(
            h5(style = "font-weight:700;", w$t), p(style = "margin:0; color:#3d434b;", w$b))))
        )),
        div(class = "pl-diag warn", style = "margin-top:1rem;",
            span(class = "tag", "What it does not do for free:"),
            "priors have to be justified and their influence checked (see the ",
            pl_goto("Prior Lab", "prior_lab", "overview", .noWS = "outside"), "); models without exact formulas need computation (",
            pl_goto("MCMC", "mcmc", .noWS = "outside"), "); and every model still needs checking against the data (",
            pl_goto("Model Lab", "model_lab", .noWS = "outside"), ")."),

        # ---- demo 1: Bayes' rule -----------------------------------------------------------
        h3(style = "margin-top:1.8rem;", "Bayes’ rule with a screening test"),
        p("The classic example. A condition affects some of the population; a test is good but not perfect. If someone tests positive, how likely is it that they have the condition? Move the sliders."),
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
          div(
            sliderInput(ns("prev"), "How common is the condition? (prior)", min = 0.1, max = 50, value = 1, step = 0.1, post = "%"),
            sliderInput(ns("sens"), "Sensitivity: P(positive | has it)", min = 50, max = 99.9, value = 90, step = 0.1, post = "%"),
            sliderInput(ns("spec"), "Specificity: P(negative | doesn't have it)", min = 50, max = 99.9, value = 95, step = 0.1, post = "%")
          ),
          div(
            uiOutput(ns("test_tiles")),
            plotOutput(ns("test_plot"), height = "200px"),
            uiOutput(ns("test_text"))
          )
        ),

        # ---- demo 2: learning from data ----------------------------------------------------------
        h3(style = "margin-top:1.8rem;", "Watch beliefs update, and priors wash out"),
        p("Three people start with different priors about a module’s pass rate. Students’ results arrive one at a time. Press play to watch each posterior update; with enough data they end up agreeing."),
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(4, 8)),
          div(
            sliderInput(ns("n"), "Students observed so far", min = 0, max = 100, value = 0, step = 1,
                        animate = animationOptions(interval = 450)),
            sliderInput(ns("truth"), "True pass rate used to simulate the results", min = 0.1, max = 0.9, value = 0.7, step = 0.05),
            uiOutput(ns("seq_text"))
          ),
          div(
            plotOutput(ns("seq_plot"), height = "300px"),
            tableOutput(ns("seq_table"))
          )
        ),

        # ---- frequentist vs Bayesian ---------------------------------------------------------------
        h3(style = "margin-top:1.8rem;", "Frequentist and Bayesian, side by side"),
        tags$table(class = "table table-striped pl-table", style = "max-width: 1100px;",
                   tags$thead(tags$tr(tags$th(""), tags$th("Frequentist"), tags$th("Bayesian"))),
                   tags$tbody(lapply(BAYES_COMPARE, function(r) tags$tr(tags$td(strong(r[1])), tags$td(r[2]), tags$td(r[3]))))),
        p(class = "pl-note", "Both are legitimate frameworks. With plenty of data and weak priors their numbers are often close; they differ most with small samples, strong prior information, complex hierarchical structure, and in how results are interpreted."),

        # ---- glossary -------------------------------------------------------------------------------
        h3(style = "margin-top:1.8rem;", "Key words"),
        do.call(bslib::accordion, c(
          list(open = FALSE, multiple = TRUE),
          lapply(BAYES_GLOSSARY, function(g) bslib::accordion_panel(g[1], p(g[2])))
        )),

        div(style = "margin-top:1.6rem;",
            pl_goto("Next: three models with exact answers →", "basics", "start", class = "btn btn-primary"))
    )
  )
}
