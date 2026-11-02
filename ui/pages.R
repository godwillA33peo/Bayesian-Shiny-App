# ============================================================================
# Top-level pages: each wraps existing module UIs with a page header,
# sub-navigation where needed, and Back / Next links.
# ============================================================================

# ---- 1. Bayesian Basics -----------------------------------------------------------
BASICS_MODELS <- list(
  list(key = "binomial", kind = "Proportions", name = "Binomial", example = "e.g. how many of 40 students passed",
       unknown = "success probability θ", prior = "Beta(α, β)", post = "Beta(α + x, β + n − x)"),
  list(key = "poisson", kind = "Counts", name = "Poisson", example = "e.g. hospital admissions per day",
       unknown = "rate λ", prior = "Gamma(α, β)", post = "Gamma(α + Σy, β + n)"),
  list(key = "normal", kind = "Means", name = "Normal", example = "e.g. the average exam score",
       unknown = "mean μ (σ known)", prior = "Normal(μ₀, τ₀)", post = "Normal, precision-weighted")
)

basicsStartUI <- function() {
  tabPanel(
    "Three models", value = "start",
    div(style = "max-width: 1100px;",
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
          div(
            h3("Three models, one idea"),
            p("Each tab that follows applies Bayes’ theorem to one common kind of data. In every case you choose a prior, enter or simulate data, and see the posterior:"),
            tags$pre(class = "pl-code", "P(θ | data) ∝ P(data | θ) × P(θ)"),
            p(class = "pl-note", "New to Bayesian statistics? Start with ",
              pl_goto("What is Bayes?", "basics", "intro", .noWS = "outside"), ".")
          ),
          bslib::card(bslib::card_body(
            h4("Why start with these three models?"),
            p("Each uses a ", strong("conjugate prior", .noWS = "after"), ": combined with its likelihood, the posterior is in the same family as the prior. That gives exact answers, so you can see precisely how much the data move your belief, and how a strong or weak prior changes that."),
            p(class = "pl-note", "When there is no exact formula, the next step, MCMC, approximates the posterior by sampling.")
          ))
        ),
        div(style = "margin-top: 1.2rem;"),
        do.call(bslib::layout_column_wrap, c(
          list(width = "290px", fill = FALSE),
          lapply(BASICS_MODELS, function(m) {
            bslib::card(bslib::card_body(
              div(class = "home-kicker", style = "color:#52514e;", m$kind),
              h3(style = "font-size:1.4rem; font-weight:700; margin:0.2rem 0;", m$name),
              p(class = "pl-note", m$example),
              tags$table(class = "table table-sm", style = "margin-bottom:0.6rem;",
                         tags$tbody(
                           tags$tr(tags$td("Unknown"), tags$td(m$unknown)),
                           tags$tr(tags$td("Prior"), tags$td(m$prior)),
                           tags$tr(tags$td("Posterior"), tags$td(m$post)))),
              pl_goto(paste("Open", m$name, "→"), "basics", m$key, class = "btn btn-primary align-self-start")))
          })))
    )
  )
}

basicsPage <- function() {
  tagList(
    pl_page_header("Step 1 of 4", "Bayesian Basics",
                   "What Bayesian statistics is and why it helps, then three models where the posterior has an exact formula: the best place to build intuition."),
    div(class = "app-wrap",
        div(class = "app-subnav",
            bslib::navset_pill(
              id = "basics_tabs",
              basicsIntroUI("basics_intro"),
              basicsStartUI(),
              binomialUI("binomial"),
              poissonUI("poisson"),
              normalUI("normal"))),
        pl_page_nav(back = list(label = "Home", page = "home"),
                    nxt = list(label = "Next: MCMC Simulations", page = "mcmc")))
  )
}

# ---- 2. MCMC ----------------------------------------------------------------------------
mcmcPage <- function() {
  tagList(
    pl_page_header("Step 2 of 4", "MCMC Simulations",
                   "When the posterior has no formula, draw samples from it instead, and learn to check that the samples can be trusted."),
    div(class = "app-wrap",
    bslib::accordion(
      open = FALSE, id = "mcmc_about",
      bslib::accordion_panel(
        "What is MCMC, and how do I know it worked?",
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
          div(
            p(strong("Markov chain Monte Carlo (MCMC)"), " is a family of algorithms for sampling from a probability distribution when computing it directly is difficult or impossible. Many real Bayesian models have no analytical solution, so we approximate the posterior with samples and summarise those (mean, median, credible intervals)."),
            tags$ul(
              tags$li(strong("Markov chain:"), " a sequence of values where each depends only on the previous one"),
              tags$li(strong("Burn-in:"), " early samples discarded because the chain has not reached the target yet"),
              tags$li(strong("Thinning:"), " keeping every nth sample to reduce autocorrelation"),
              tags$li(strong("Multiple chains:"), " several independent runs, to check they agree"))),
          div(
            p(strong("Diagnosing convergence")),
            tags$ul(
              tags$li("Trace plots should look like “hairy caterpillars” with no trends"),
              tags$li("Different chains should overlap and give similar distributions"),
              tags$li("Autocorrelation should die away quickly")),
            p(class = "pl-note", "The Model Lab adds R-hat and effective sample size, the diagnostics Stan reports.")))
      )
    ),
    br(),
    pl_pane_content(mcmcUI("mcmc")),
    pl_page_nav(back = list(label = "Back: Bayesian Basics", page = "basics"),
                nxt = list(label = "Next: Prior Lab", page = "prior_lab", sub = "overview")))
  )
}

# ---- 3. Prior Lab ---------------------------------------------------------------------------
PRIOR_LAB_GROUPS <- list(
  list(name = "Explore priors", blurb = "See what a prior says, build one from judgements, or start from a documented preset.",
       tools = list(
         list(value = "prior_builder", name = "Prior Builder", desc = sprintf("Compare up to four priors from %d families, on the parameter scale and after the link function, with code export.", length(PL_DISTS))),
         list(value = "prior_advanced", name = "Advanced priors", desc = "Shrinkage priors for many predictors, Dirichlet priors for shares, and Inverse-Wishart vs LKJ for covariance matrices."),
         list(value = "prior_presets", name = "Presets", desc = sprintf("%d documented starting points for health, education and general models, plus quick builders.", length(PL_PRESETS))),
         list(value = "prior_elicit", name = "Elicitation", desc = "Turn an expert’s quantiles, or a mean and SD, into a fitted prior, with feedback to check it."))),
  list(name = "Check priors", blurb = "Confront priors with data and with what they imply, before you rely on them.",
       tools = list(
         list(value = "prior_posterior", name = "Prior → Posterior & Sensitivity", desc = "Any prior with a Binomial, Poisson or Normal likelihood: exact posteriors, sensitivity analysis and prior–data conflict checks."),
         list(value = "prior_predictive", name = "Prior predictive (regression)", desc = "What intercept, slope and SD priors imply for outcomes in linear, logistic and Poisson regression."),
         list(value = "prior_hier", name = "Hierarchical priors", desc = "What a prior on the between-group SD implies, exact partial pooling for grouped data, and LKJ correlations.")))
)

priorLabOverviewUI <- function() {
  tabPanel(
    "Overview", value = "overview",
    div(style = "max-width: 1200px;",
        lapply(PRIOR_LAB_GROUPS, function(g) {
          div(style = "margin-bottom: 1.6rem;",
              h3(style = "font-weight:700; margin-bottom:0.1rem;", g$name),
              p(class = "pl-note", style = "font-size:1rem;", g$blurb),
              do.call(bslib::layout_column_wrap, c(
                list(width = "260px", fill = FALSE),
                lapply(g$tools, function(t) {
                  bslib::card(class = "app-overview-card", bslib::card_body(
                    h3(t$name), p(t$desc),
                    div(style = "margin-top:auto;",
                        pl_goto(paste("Open", "→"), "prior_lab", t$value, class = "btn btn-outline-primary btn-sm"))))
                }))))
        }),
        div(class = "pl-help",
            h4("Your prior library"),
            p("Save a prior in any tool (or tick presets) and it appears under “Load a saved prior” in every other tool and in the Model Lab. Download the library as a CSV from the Prior Builder to keep it between visits."))
    )
  )
}

priorLabPage <- function() {
  tagList(
    pl_page_header("Step 3 of 4", "Prior Lab",
                   "Build, elicit and stress-test priors. Priors you save here appear in every other tool and in the Model Lab."),
    div(class = "app-wrap",
    div(class = "app-subnav",
        bslib::navset_pill(
          id = "prior_lab_tabs",
          priorLabOverviewUI(),
          bslib::nav_item(span(class = "app-navgroup", "Explore")),
          priorBuilderUI("prior_builder"),
          priorAdvancedUI("prior_advanced"),
          priorPresetsUI("prior_presets"),
          priorElicitUI("prior_elicit"),
          bslib::nav_item(span(class = "app-navgroup", "Check")),
          priorPosteriorUI("prior_posterior"),
          priorPredictiveUI("prior_predictive"),
          priorHierUI("prior_hier"))),
    pl_page_nav(back = list(label = "Back: MCMC Simulations", page = "mcmc"),
                nxt = list(label = "Next: Model Lab", page = "model_lab")))
  )
}

# ---- 4. Model Lab ---------------------------------------------------------------------------------
modelLabPage <- function() {
  tagList(
    pl_page_header("Step 4 of 4", "Model Lab",
                   "Pick a data model, bring your data, set a prior for every parameter, check the fit, and take the Stan program with you."),
    div(class = "app-wrap",
        pl_pane_content(modelLabUI("model_lab")),
        pl_page_nav(back = list(label = "Back: Prior Lab", page = "prior_lab", sub = "overview"),
                    nxt = list(label = "Back to Home", page = "home")))
  )
}
