# Changelog

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
