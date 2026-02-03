aboutUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "About",
    fluidRow(
      column(
        width = 10, offset = 1,

        # App Title and Purpose
        div(
          style = "margin-top: 30px; margin-bottom: 30px;",
          h2("Welcome to Bayesian Explorer"),
          p(
            style = "font-size: 16px;",
            "An interactive web application for learning fundamental concepts in Bayesian statistics through visualization and hands-on exploration."
          )
        ),

        # What is Bayesian Statistics?
        div(
          style = "background-color: #f8f9fa; padding: 20px; margin-bottom: 20px; border-radius: 5px;",
          h3("What is Bayesian Statistics?"),
          p(
            "Bayesian statistics is an approach to statistical inference that combines:",
            tags$ul(
              tags$li(strong("Prior beliefs"), " - What we know (or assume) before seeing data"),
              tags$li(strong("Observed data"), " - New evidence we collect"),
              tags$li(strong("Posterior distribution"), " - Updated beliefs after combining prior and data")
            )
          ),
          p(
            "The fundamental principle is ", strong("Bayes' Theorem:"),
            br(),
            tags$code("P(θ|data) ∝ P(data|θ) × P(θ)"),
            br(),
            "where θ represents the parameter(s) we want to learn about."
          )
        ),

        # About the App
        div(
          style = "background-color: #fff; padding: 20px; margin-bottom: 20px; border: 1px solid #dee2e6; border-radius: 5px;",
          h3("About This App"),
          p(
            "This app was developed as part of the ", em("Introduction to Bayesian Modelling"),
            " module for the MSc in Health Data Science at University of Galway."
          ),
          p(
            strong("Features:"),
            tags$ul(
              tags$li("Interactive exploration of conjugate Bayesian models (Binomial, Poisson, Normal)"),
              tags$li("Visual comparison of prior and posterior distributions"),
              tags$li("Credible interval calculations"),
              tags$li("MCMC sampling demonstrations for complex inference")
            )
          )
        ),

        # Conjugate Priors
        div(
          style = "background-color: #e7f3ff; padding: 20px; margin-bottom: 20px; border-radius: 5px;",
          h3("Conjugate Priors"),
          p(
            "A ", strong("conjugate prior"), " is a prior distribution that, when combined with a particular likelihood, ",
            "produces a posterior distribution in the same family as the prior."
          ),
          p(
            "This app demonstrates three conjugate pairs:",
            tags$ul(
              tags$li(strong("Beta-Binomial:"), " Beta prior for binomial success probability"),
              tags$li(strong("Gamma-Poisson:"), " Gamma prior for Poisson rate parameter"),
              tags$li(strong("Normal-Normal:"), " Normal prior for normal mean (known variance)")
            )
          ),
          p(
            "Conjugate priors allow us to calculate posteriors analytically, making them excellent for learning and intuition building."
          )
        ),

        # MCMC Section
        div(
          style = "background-color: #fff5f5; padding: 20px; margin-bottom: 20px; border-radius: 5px;",
          h3("What is MCMC?"),
          p(
            strong("Markov Chain Monte Carlo (MCMC)"), " is a family of algorithms for sampling from probability distributions ",
            "when direct computation is difficult or impossible."
          ),
          p(
            strong("Why use MCMC?"),
            tags$ul(
              tags$li("Many real-world Bayesian models don't have analytical solutions"),
              tags$li("MCMC allows us to approximate complex posterior distributions through sampling"),
              tags$li("We can make inferences from the samples (mean, median, credible intervals)")
            )
          ),
          p(
            strong("Key MCMC Concepts:"),
            tags$ul(
              tags$li(strong("Markov Chain:"), " A sequence of random values where each depends only on the previous one"),
              tags$li(strong("Burn-in:"), " Initial samples discarded because the chain hasn't reached the target distribution yet"),
              tags$li(strong("Thinning:"), " Keeping every nth sample to reduce autocorrelation"),
              tags$li(strong("Convergence:"), " The chain has stabilized and is sampling from the target posterior"),
              tags$li(strong("Multiple Chains:"), " Running several chains helps verify convergence")
            )
          ),
          p(
            strong("Diagnosing Convergence:"),
            tags$ul(
              tags$li("Trace plots should look like 'hairy caterpillars' with no trends"),
              tags$li("Multiple chains should overlap and produce similar distributions"),
              tags$li("Autocorrelation should decay quickly")
            )
          )
        ),

        # How to Use
        div(
          style = "background-color: #f0fff0; padding: 20px; margin-bottom: 30px; border-radius: 5px;",
          h3("How to Use This App"),
          p(
            tags$ol(
              tags$li(strong("Start Simple:"), " Begin with the Binomial tab to understand how priors and data interact"),
              tags$li(strong("Experiment:"), " Try different prior parameters - weak vs strong priors, different shapes"),
              tags$li(strong("Compare Models:"), " Explore Poisson and Normal tabs to see how different likelihoods work"),
              tags$li(strong("Advance to MCMC:"), " Once comfortable with conjugate models, try MCMC for sampling-based inference")
            )
          )
        ),

        # Footer
        hr(),
        div(
          style = "text-align: center; color: #6c757d; margin-bottom: 30px;",
          p(
            "Built with R Shiny | ",
            tags$a(href = "https://www.linkedin.com/in/godwill-zulu/", target = "_blank", "Godwill Zulu"),
            br(),
            "MSc Health Data Science - Bayesian Modelling Module"
          )
        )
      )
    )
  )
}
