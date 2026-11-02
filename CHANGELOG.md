# Changelog

## Teaching tab: "What is Bayes?"

- New first sub-tab under **Bayesian Basics** (`ui/tab_basics_intro.R`, `server/mod_basics_intro.R`) for people new to Bayesian statistics:
  - the idea in one sentence, and posterior ∝ likelihood × prior
  - a worked example (9 of 12 students pass vs a 60% baseline) answered both ways: a p-value of about 0.38, against a posterior probability of about 83% that the rate is above 60%, with a 95% credible interval of about 46% to 91%
  - six reasons Bayesian methods are useful, and what they do not do for free (with links to the Prior Lab, MCMC and Model Lab)
  - **Bayes' rule with a screening test:** sliders for prevalence, sensitivity and specificity, with results shown as natural frequencies out of 10,000 people
  - **Priors wash out:** three people with different Beta priors watch the same students' results arrive one at a time (press play); their posteriors converge
  - a frequentist vs Bayesian comparison table and a glossary of key words
- The old "Start here" sub-tab is now **Three models**, which focuses on the Binomial, Poisson and Normal models. "Start with the basics" on Home now opens What is Bayes?.
- On phones, wide tables now scroll inside themselves, and the page no longer scrolls sideways.

## New layout: Home page, learning path and Bootstrap 5

- **Theme:** the app now uses `bslib::page_navbar()` with a Bootstrap 5 theme (`app_theme()` in `R/app_layout.R`) instead of `shinythemes::shinytheme("cosmo")`. The font is Source Sans 3, with a system-font fallback. `shinythemes` is no longer loaded.
- **Five top-level pages,** in learning-path order: Home · Bayesian Basics · MCMC Simulations · Prior Lab · Model Lab.
  - **Home** (`ui/tab_home.R`, `server/mod_home.R`) replaces the About tab. It has a live "Bayes in ten seconds" demo, the prior × likelihood ∝ posterior idea, learning-path cards, a "start from your field" section with examples that jump straight to the right tool, counts of what's inside, and a footer with LinkedIn, GitHub and a "How to cite" pop-up.
  - **Bayesian Basics** groups the Binomial, Poisson and Normal tabs as sub-tabs behind an introductory tab (now "Three models").
  - **MCMC Simulations** stays on its own, with the old About text on MCMC and convergence in a collapsible panel.
  - **Prior Lab** opens on an overview of its seven tools, grouped as Explore and Check, with sub-tabs for each.
  - **Model Lab** moves to the top bar.
- Every page has a header band ("Step n of 4") and Back / Next links at the bottom.
- **One navigation mechanism:** any link with class `pl-goto` and `data-page`, `data-sub`, `data-model` or `data-domain` attributes jumps there. See `pl_goto()` in `R/app_layout.R` and the observer in `server/server_main.R`.
- `ui/tab_about.R` was removed; its content moved to Home, Bayesian Basics and MCMC.

## Prior Lab, round 2: Model Lab, advanced priors, presets

New tabs in the Prior Lab menu, which is now grouped into "Explore priors", "Check priors" and "Fit models":

- **Model Lab** (`ui/tab_model_lab.R`, `server/mod_model_lab.R`, `R/prior_lab_models.R`, `R/prior_lab_mcmc.R`):
  - 11 data models with example data or CSV upload, and a prior picker for every parameter that appears with the model
  - live prior predictive checks, posterior summaries with contraction, posterior predictive checks (Kaplan-Meier based for survival), trace plots, R-hat and ESS, and WAIC comparison
  - complete Stan programs with cmdstanr code, plus downloads of the `.stan` file, the data and the posterior draws
- **Advanced priors** (`R/prior_lab_advanced.R`): shrinkage priors with the R² they imply, Dirichlet, and Inverse-Wishart vs LKJ.
- **Presets** (`R/prior_lab_presets.R`): 24 documented presets for health, education and general use, plus four quick builders.
- **New prior families** (`R/prior_lab_families.R`): skew-normal, logit-normal, Beta(mean, sample size), Weibull and scaled inverse-χ². They are available everywhere, including elicitation fitting.
- Stan code everywhere now handles priors that need a custom log-density (e.g. logit-normal on a probability).
- The prior picker remembers edited values when its inputs are re-drawn.

The in-app sampler and its diagnostics are checked against exact posteriors, frequentist fits, the `posterior` package, and real Stan fits; see `tests/test_prior_lab.R`. There are still no new package dependencies.

## Prior Lab (new "Prior Lab" menu)

New files:

| File | What it does |
|---|---|
| `R/prior_lab_dists.R` | 14 prior families with density, CDF, quantiles, draws, moments, and brms / Stan / R code |
| `R/prior_lab_engine.R` | Elicitation fitting, adaptive grid posteriors, power-scaling sensitivity, prior predictive simulation, exact 8-schools hierarchical model, LKJ marginal (no Shiny code) |
| `R/prior_lab_shared.R` | Plot theme and colours, help boxes and stat tiles, the reusable `priorPicker` module, prior library helpers |
| `ui/tab_prior_*.R`, `server/mod_prior_*.R` | The five Prior Lab tabs |
| `tests/test_prior_lab.R` | 144 checks against closed-form conjugate results and brute-force integration (`Rscript tests/test_prior_lab.R`) |

No new package dependencies: the Prior Lab uses only base R, shiny and ggplot2.

## Fixes to existing tabs

- **Binomial tab:** the data table, simulation sliders and credible-interval plot never appeared. Their `conditionalPanel` conditions used R syntax (`input$prior_type`) instead of JavaScript (`input.prior_type`) and lacked `ns = ns`, which a module needs. The data table now shows for both prior types.
- **Poisson tab:** the credible-interval panel's condition lacked `ns = ns`, so it showed for the discrete prior too.
- **Poisson tab, discrete prior:** the likelihood ignored the number of observations. It now uses `dpois(total_counts, n_obs * theta)`.
- **Poisson tab, Gamma prior:** the plots were fixed to the range 0 to 5, which cut off the default posterior (mean about 4, with about 13% of its mass above 5) and about a quarter of the default prior. The range now adapts to the prior and the posterior. The prior plot now uses ggplot like the other plots.
- **`R/bayes_utils.R`:** `size =` changed to `linewidth =` for lines (removes the ggplot2 3.4+ deprecation warning).
- **`global.R`:** dropped `library(learnr)` and `library(rsconnect)`. Neither is used by the running app.
