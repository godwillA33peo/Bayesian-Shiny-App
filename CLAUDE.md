# Bayesian Learning Shiny App

## Project Overview
An interactive Shiny web application for exploring fundamental concepts in Bayesian statistics. Built as part of the 'Introduction to Bayesian Modelling' module for MSc in Health Data Science at University of Galway. This is a portfolio piece demonstrating skills in R, Shiny, and applied Bayesian methods.

## Purpose
Build intuition for how prior beliefs combine with observed data to form posterior distributions through interactive visualisation.

## Tech Stack
- **Language:** R
- **Framework:** Shiny
- **Bayesian Core:** Custom utility functions (extracted from TeachBayes) in R/bayes_utils.R
- **Plotting:** ggplot2 and plotly
- **Data Manipulation:** dplyr, tidyr

## Current Implementation Status

### ✅ Completed
- **Binomial Model (Discrete Prior)**
  - User inputs: trials (n), successes (x)
  - Discrete prior specification (theta values and probabilities)
  - Table showing Prior, Likelihood, Posterior
  - Visualisation of prior and posterior using TeachBayes

- **Binomial Model (Conjugate Prior)** - Partial
  - Beta-Binomial model implemented
  - Beta(α, β) prior parameter inputs
  - Continuous prior, likelihood, posterior on single plot
  - **TODO:** Point estimates and simulations

### 🚧 To Be Implemented
- **Poisson Model (Conjugate Prior)**
  - Gamma-Poisson conjugate model
  - Gamma(α, β) prior for the rate parameter λ
  - Posterior: Gamma(α + Σx, β + n)

- **Normal Model (Conjugate Priors)**
  - Normal-Normal (known variance): posterior for mean μ
  - Normal-Inverse-Gamma (unknown variance): posterior for μ and σ²
  - Visualise prior, likelihood, posterior densities
  - Show credible intervals

- **MCMC Integration**
  - Demonstrate sampling-based inference
  - Trace plots, density plots
  - Potentially using rstan or rjags for simple models

## App Structure (Expected)
```
/app.R or /ui.R + /server.R    # Main Shiny app
/R/                            # Helper functions
/www/                          # CSS, images
/data/                         # Example datasets (if any)
```

## Coding Preferences
- Use tidyverse style with pipes (`%>%` or `|>`)
- Keep UI and server logic modular (use Shiny modules if appropriate)
- Comment code to explain Bayesian concepts for learning purposes
- Use consistent naming: `prior_*`, `likelihood_*`, `posterior_*`
- Plots should clearly label Prior, Likelihood, Posterior with a legend

## Key Bayesian Formulas to Implement

### Beta-Binomial (done, needs point estimates)
- Prior: Beta(α, β)
- Likelihood: Binomial(n, θ)
- Posterior: Beta(α + x, β + n - x)
- Point estimates: posterior mean = (α + x) / (α + β + n)

### Gamma-Poisson (to do)
- Prior: Gamma(α, β)
- Likelihood: Poisson(λ)
- Posterior: Gamma(α + Σx, β + n)

### Normal-Normal (to do, known variance σ²)
- Prior: μ ~ Normal(μ₀, τ₀²)
- Likelihood: x̄ | μ ~ Normal(μ, σ²/n)
- Posterior: μ | data ~ Normal(μₙ, τₙ²)
  - τₙ² = 1 / (1/τ₀² + n/σ²)
  - μₙ = τₙ² × (μ₀/τ₀² + n×x̄/σ²)

## When Helping
- Prioritise educational clarity — this app is for learning Bayesian concepts
- Match the existing style when adding new model tabs
- Include clear axis labels, legends, and titles on all plots
- Add tooltips or explanatory text in the UI where helpful
- Suggest improvements for interactivity and user experience
- When implementing formulas, add comments explaining each step

## Git Workflow
Project uses Git version control. Commit frequently with descriptive messages:
- `git add -A`
- `git commit -m "Added Normal-Normal posterior calculation and plot"`
- `git push`
