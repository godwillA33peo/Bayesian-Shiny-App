# Home page (replaces the old About tab)

HOME_STEPS <- list(
  list(n = "1", title = "Bayesian Basics", page = "basics", sub = "intro", cta = "Start here",
       body = "What Bayesian statistics is and why it helps, then prior, likelihood and posterior for proportions, counts and means, where the maths has exact answers.",
       inside = "What is Bayes? · Binomial · Poisson · Normal"),
  list(n = "2", title = "MCMC Simulations", page = "mcmc", sub = NULL, cta = "See sampling",
       body = "When there is no formula, sample the posterior instead: chains, burn-in, and how to tell they converged.",
       inside = "Trace plots · densities · autocorrelation"),
  list(n = "3", title = "Prior Lab", page = "prior_lab", sub = "overview", cta = "Explore priors",
       body = "Build, elicit and stress-test priors: implied scales, sensitivity, predictive checks and hierarchical priors.",
       inside = "7 tools · presets for health and education"),
  list(n = "4", title = "Model Lab", page = "model_lab", sub = NULL, cta = "Fit a model",
       body = "Bring your data, set a prior for every parameter, check the fit, and take the Stan program with you.",
       inside = "Data models · diagnostics · Stan code")
)

# Example questions by field; each points to a page (and optionally a sub-tab,
# a Model Lab model or a preset domain)
HOME_FIELDS <- list(
  health = list(
    list(q = "Is a new treatment better than usual care?",
         how = "Compare a sceptical and an enthusiastic prior for the odds ratio.",
         where = "Prior Lab · Presets", page = "prior_lab", sub = "prior_presets", domain = "Health"),
    list(q = "GP visits per patient vary far more than a Poisson model allows.",
         how = "Fit a negative binomial model and compare it with Poisson.",
         where = "Model Lab · Negative binomial", page = "model_lab", model = "negbinomial"),
    list(q = "How long until an event happens, when some people are still being followed?",
         how = "Weibull survival with censoring, checked against the Kaplan–Meier curve.",
         where = "Model Lab · Weibull survival", page = "model_lab", model = "weibull")
  ),
  education = list(
    list(q = "Do Likert answers about GenAI shift with weekly use?",
         how = "An ordinal (cumulative logit) model with a predictor.",
         where = "Model Lab · Ordinal", page = "model_lab", model = "ordinal"),
    list(q = "How different can pass rates be between modules?",
         how = "See what a prior on the between-module SD implies about real groups.",
         where = "Prior Lab · Hierarchical priors", page = "prior_lab", sub = "prior_hier"),
    list(q = "How big could an intervention effect plausibly be?",
         how = "Start from documented education effect-size priors.",
         where = "Prior Lab · Presets", page = "prior_lab", sub = "prior_presets", domain = "Education")
  ),
  any = list(
    list(q = "What does my prior actually say?",
         how = "Plot it, and see it on the probability or ratio scale.",
         where = "Prior Lab · Prior Builder", page = "prior_lab", sub = "prior_builder"),
    list(q = "An expert gave me a range, not a distribution.",
         how = "Fit a prior to their quantiles and read back what it implies.",
         where = "Prior Lab · Elicitation", page = "prior_lab", sub = "prior_elicit"),
    list(q = "How does a proportion get updated by data?",
         how = "Beta–binomial updating, step by step.",
         where = "Bayesian Basics · Binomial", page = "basics", sub = "binomial")
  )
)

home_curve_svg <- function(d, colour, dash = NULL, width = 3) {
  HTML(sprintf('<svg viewBox="0 0 200 70" aria-hidden="true"><path d="%s" fill="none" stroke="%s" stroke-width="%s"%s></path></svg>',
               d, colour, width, if (is.null(dash)) "" else sprintf(' stroke-dasharray="%s"', dash)))
}

homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    # ---- hero with live demo ---------------------------------------------------------
    div(class = "home-hero",
        div(class = "home-wrap",
            bslib::layout_columns(
              col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)), gap = "3rem",
              div(style = "align-self: center;",
                  div(class = "home-kicker", "Interactive Bayesian statistics"),
                  h1("Learn Bayesian inference by doing"),
                  p(class = "home-lead",
                    "Start with coin flips and conjugate priors, see how samplers explore a posterior, then build, check and defend priors for real models in health, education and beyond. Every model comes with its Stan code."),
                  div(class = "home-cta",
                      pl_goto("Start with the basics", "basics", "intro", class = "btn btn-primary"),
                      pl_goto("Open the Prior Lab", "prior_lab", "overview", class = "btn btn-outline-primary"))),
              div(class = "home-demo",
                  div(style = "display:flex; justify-content:space-between; align-items:baseline; flex-wrap:wrap; gap:8px;",
                      h2("Bayes in ten seconds"),
                      span(class = "pl-note", "Is this module's pass rate above 50%?")),
                  div(class = "pl-chips", style = "margin-top:0.6rem;",
                      radioButtons(ns("prior"), "Your prior", inline = TRUE,
                                   choices = c("No idea (flat)" = "flat", "Sceptical (around 30%)" = "sceptical",
                                               "Hopeful (around 70%)" = "hopeful"))),
                  plotOutput(ns("demo_plot"), height = "210px"),
                  div(style = "display:flex; flex-wrap:wrap; gap:10px; align-items:center;",
                      actionButton(ns("pass"), "+1 pass", class = "btn-primary"),
                      actionButton(ns("fail"), "+1 fail", class = "btn-outline-secondary"),
                      actionLink(ns("reset"), "Reset"),
                      span(class = "pl-note", textOutput(ns("demo_data"), inline = TRUE))),
                  uiOutput(ns("demo_tiles"))))
        )),

    # ---- Bayes' theorem in pictures ----------------------------------------------------
    div(class = "home-section",
        div(class = "home-wrap",
            h2("One idea behind everything here"),
            p(class = "sub", "What you believed before, combined with what the data say, gives what you believe now."),
            div(class = "home-theorem",
                bslib::layout_columns(
                  col_widths = bslib::breakpoints(sm = 12, lg = c(3, 1, 4, 1, 3)),
                  bslib::card(bslib::card_body(
                    home_curve_svg("M5 65 C 50 65, 70 15, 100 15 S 150 65, 195 65", "#2a78d6"),
                    h3("Prior"), p("What you believe before seeing the data."))),
                  div(class = "home-op", "×"),
                  bslib::card(bslib::card_body(
                    home_curve_svg("M5 65 C 100 65, 115 8, 135 8 S 165 65, 195 65", "#0f9b7a", dash = "7 4"),
                    h3("Likelihood"), p("How well each possible value explains the data you saw."))),
                  div(class = "home-op", "∝"),
                  bslib::card(bslib::card_body(
                    home_curve_svg("M5 65 C 90 65, 105 5, 122 5 S 150 65, 195 65", "#eb6834", width = 3.5),
                    h3("Posterior"), p("Your updated belief: a compromise weighted by how much each side knows."))))))),

    # ---- learning path ------------------------------------------------------------------
    div(class = "home-section",
        div(class = "home-wrap",
            h2("Your learning path"),
            p(class = "sub", "Follow the steps in order, or jump straight to what you need."),
            do.call(bslib::layout_column_wrap, c(
              list(width = "250px", fill = FALSE),
              lapply(HOME_STEPS, function(s) {
                bslib::card(class = "home-step", bslib::card_body(
                  span(class = "badge-num", s$n),
                  h3(s$title),
                  p(s$body),
                  div(class = "inside", s$inside),
                  div(style = "margin-top:auto; padding-top:0.6rem;",
                      pl_goto(paste(s$cta, "→"), s$page, s$sub, class = "fw-semibold"))))
              }))))),

    # ---- start from your field ------------------------------------------------------------
    div(class = "home-band",
        div(class = "home-section", style = "padding-top:2rem;",
            div(class = "home-wrap",
                h2("Start from your field"),
                p(class = "sub", "Pick a field to see questions the app can help with, and where to go for each."),
                div(class = "pl-chips",
                    radioButtons(ns("field"), NULL, inline = TRUE, selected = "education",
                                 choices = c("Health" = "health", "Education" = "education", "Any field" = "any"))),
                uiOutput(ns("field_cards"))))),

    # ---- what's inside --------------------------------------------------------------------
    div(class = "home-section",
        div(class = "home-wrap",
            bslib::layout_column_wrap(
              width = "220px", fill = FALSE,
              div(class = "home-fact", div(class = "num", length(PL_DISTS)), div(class = "lab", "prior families to explore")),
              div(class = "home-fact", div(class = "num", length(PL_MODELS)), div(class = "lab", "data models in the Model Lab")),
              div(class = "home-fact", div(class = "num", length(PL_PRESETS)), div(class = "lab", "documented presets for health, education and general use")),
              div(class = "home-fact", div(class = "num", "Stan"), div(class = "lab", "code for every model, ready for cmdstanr or rstan"))))),

    # ---- footer -------------------------------------------------------------------------------
    div(class = "home-footer",
        div(class = "home-wrap",
            div(style = "display:flex; flex-wrap:wrap; justify-content:space-between; gap:16px;",
                div(div(style = "color:#ffffff; font-weight:700;", "Bayesian Explorer"),
                    div("Built with R Shiny by Godwill Zulu. Started in the MSc in Health Data Science, University of Galway.")),
                div(style = "display:flex; flex-wrap:wrap; gap:18px; align-items:center;",
                    tags$a(href = APP_LINKEDIN_URL, target = "_blank", rel = "noopener", "LinkedIn"),
                    tags$a(href = APP_REPO_URL, target = "_blank", rel = "noopener", "GitHub repository"),
                    actionLink(ns("cite"), "How to cite", style = "color:#ffffff;")))))
  )
}
