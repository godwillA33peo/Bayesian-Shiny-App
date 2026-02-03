# Testing Guide for Bayesian Explorer App

## Quick Start

### Option 1: RStudio (Recommended)
1. Open RStudio
2. Navigate to: `C:\Users\Godwill\Documents\Bayesian Explorer\bayeshiny`
3. Open `app.R`
4. Click the **"Run App"** button in RStudio
5. Or run in console: `shiny::runApp()`

### Option 2: R Console
```r
setwd("C:/Users/Godwill/Documents/Bayesian Explorer/bayeshiny")
shiny::runApp()
```

---

## What to Test

### ✅ 1. Binomial Distribution Tab

**Test Discrete Prior:**
- Select "Discrete" prior type
- Use default values
- Adjust trials and successes
- Verify table and plots update

**Test Continuous (Beta) Prior:**
- Select "Continuous (Beta)" prior type
- Adjust alpha and beta sliders
- Modify trials and successes
- Check:
  - Prior plot (blue)
  - Posterior vs Prior plot (blue vs red)
  - Summary table
  - Credible interval plot
  - Simulations with adjustable CI level

### ✅ 2. Poisson Distribution Tab

**Test Discrete Prior:**
- Select "Discrete" prior type
- Enter theta values and priors
- Adjust observations and counts
- Verify discrete plots

**Test Continuous (Gamma) Prior:**
- Select "Continuous (Gamma)" prior type
- Adjust mean and SD
- Modify observations and total counts
- Check:
  - Prior plot
  - Posterior plot (prior vs posterior overlay)
  - Summary table
  - Credible interval plot

### ✅ 3. Normal Distribution Tab (NEW!)

**Test Normal-Normal Conjugate:**
- Set sample mean (e.g., 50)
- Set sample size (e.g., 30)
- Set known SD (e.g., 10)
- Adjust prior mean (e.g., 40)
- Adjust prior SD (e.g., 15)
- Check:
  - Prior distribution plot
  - Prior vs Posterior overlay
  - Summary table showing all distributions
  - Point estimate output
  - Credible interval with adjustable level (50%-99%)

**Expected Behavior:**
- Posterior should be between prior and data
- Larger sample size → posterior closer to data
- Smaller prior SD → stronger prior influence

### ✅ 4. MCMC Simulations Tab (NEW!)

#### Test Binomial Model:
1. Upload: `data/binomial_sample.csv`
2. Select: "Binomial" model
3. Choose columns:
   - Success Column: `successes`
   - Trials Column: `trials`
4. Set priors: Beta(1, 1) - uniform
5. MCMC settings: 5000 iterations, 1000 burn-in, thin=2, 3 chains
6. Check tabs:
   - **Data Preview**: Shows uploaded data
   - **Trace Plot**: Chains should mix well, no trends
   - **Posterior Density**:
     - By chain: All chains similar
     - Combined: Single smooth distribution
   - **Diagnostics**:
     - Autocorrelation decays quickly
     - Summary statistics show mean ~0.75-0.80

#### Test Poisson Model:
1. Upload: `data/poisson_sample.csv`
2. Select: "Poisson" model
3. Choose: Count Column = `counts`
4. Set priors: Gamma(2, 1)
5. Same MCMC settings
6. Expected posterior mean: ~4.5-5.0

#### Test Normal Model:
1. Upload: `data/normal_sample.csv`
2. Select: "Normal" model
3. Choose: Data Column = `measurement`
4. Settings:
   - Known Variance: 1
   - Prior Mean: 50
   - Prior Variance: 10
5. Same MCMC settings
6. Expected posterior mean: ~50.0-51.0

---

## Convergence Checks (MCMC)

### Good Signs:
- ✅ Trace plots show "fuzzy caterpillar" pattern
- ✅ All chains overlap and mix well
- ✅ No trends or drift in trace plots
- ✅ Density plots from different chains overlap
- ✅ Autocorrelation drops to ~0 within 10-20 lags

### Bad Signs:
- ❌ Trace plots show trends or cycles
- ❌ Chains don't overlap
- ❌ Autocorrelation remains high (>0.5) at high lags
- ❌ Very different means across chains

### Fixes if Convergence Issues:
- Increase iterations (10,000+)
- Increase burn-in (2,000+)
- Increase thinning (5-10)
- Try different starting values

---

## Bug Fixes Applied

1. ✅ Fixed typo: "Poison" → "Poisson"
2. ✅ Fixed typo: "Distribtion" → "Distribution"
3. ✅ Fixed calculation bug in Binomial point estimate
4. ✅ Fixed conditional panel variable name in Poisson UI

---

## Known Limitations

1. **MCMC Implementation**: Uses direct sampling from conjugate posteriors (Beta, Gamma, Normal) rather than true Metropolis-Hastings. This is educationally sound and computationally efficient for these simple models.

2. **File Upload**: CSV files only. Ensure:
   - No missing headers
   - Numeric data in relevant columns
   - No special characters in column names

3. **MCMC Diagnostics**: Basic diagnostics provided. For production use, consider:
   - Gelman-Rubin diagnostic (R-hat)
   - Effective sample size
   - More sophisticated convergence tests

---

## Troubleshooting

### App won't start:
```r
# Check if required packages are installed
required_packages <- c("shiny", "bslib", "TeachBayes", "ggplot2",
                       "plotly", "tidyr", "dplyr", "shinythemes")
missing <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing)) install.packages(missing)
```

### Plots not showing:
- Check browser console for JavaScript errors
- Try a different browser
- Refresh the page

### Data upload fails:
- Verify CSV format
- Check column names match expectations
- Ensure numeric data types

---

## Success Criteria

- [ ] All four tabs load without errors
- [ ] Binomial tab: Both discrete and continuous priors work
- [ ] Poisson tab: Both discrete and continuous priors work
- [ ] Normal tab: Calculations update correctly
- [ ] MCMC tab: All three sample files work
- [ ] All plots render correctly
- [ ] No console errors in R or browser

---

## Next Steps After Testing

If all tests pass:
1. Consider adding more model types (e.g., Normal-Inverse-Gamma)
2. Add export functionality for results
3. Add more convergence diagnostics
4. Create interactive tutorials/tooltips
5. Deploy to shinyapps.io

Good luck with testing! 🚀
