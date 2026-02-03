# Sample Data Files for MCMC Testing

This directory contains sample CSV files for testing the MCMC Simulations tab.

## Files

### 1. binomial_sample.csv
- **Model Type:** Binomial
- **Columns:** `successes`, `trials`
- **Description:** Data from 15 trials measuring success rates
- **Suggested Prior:** Beta(1, 1) - uniform prior

### 2. poisson_sample.csv
- **Model Type:** Poisson
- **Columns:** `counts`
- **Description:** Event counts from 20 observations (e.g., customer arrivals per hour)
- **Suggested Prior:** Gamma(2, 1) - weakly informative prior

### 3. normal_sample.csv
- **Model Type:** Normal
- **Columns:** `measurement`
- **Description:** 20 measurements from a process with known variance
- **Suggested Settings:**
  - Known Variance: 1
  - Prior Mean: 50
  - Prior Variance: 10

## How to Use

1. Navigate to the **MCMC Simulations** tab in the app
2. Upload one of these CSV files
3. Select the corresponding model type
4. Choose the appropriate column(s) for your data
5. Set prior parameters (or use defaults)
6. Adjust MCMC settings if needed
7. View trace plots, posterior densities, and diagnostics

## MCMC Settings Guidance

- **Iterations:** 5000-10000 for stable results
- **Burn-in:** 1000-2000 (discard initial samples)
- **Thinning:** 2-5 (reduces autocorrelation)
- **Chains:** 3-4 (for convergence assessment)
