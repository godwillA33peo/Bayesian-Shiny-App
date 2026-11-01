# ============================================================================
# Prior Lab: preset priors for health, education and general modelling
# ============================================================================
# Each preset: domain, name, parameter it is meant for, the prior, a short
# rationale (numbers computed from the prior itself) and a reference key.
# These are starting points to adapt and justify, not universal defaults.
# ============================================================================

pl_preset <- function(domain, name, param, prior, why, ref = "") {
  list(domain = domain, name = name, param = param, prior = prior, why = why, ref = ref)
}

PL_PRESETS <- list(
  # ---- general --------------------------------------------------------------
  pl_preset("General", "rstanarm-style coefficient prior", "Regression coefficient (standardised predictor)",
            pl_prior("normal", 0, 2.5),
            "Default weakly informative prior in rstanarm for coefficients (scaled to the data). On the log-odds scale it still puts ~7% of mass beyond odds ratios of 100 per SD, so check it with the prior predictive tab.",
            "rstanarm"),
  pl_preset("General", "Gelman et al. logistic default", "Logistic coefficient (predictor scaled to SD 0.5)",
            pl_prior("cauchy", 0, 2.5),
            "Proposed by Gelman et al. (2008) for logistic regression with predictors scaled to SD 0.5. Heavy tails: 16% of mass beyond |\u{03b2}| = 10.",
            "gelman2008"),
  pl_preset("General", "Near-flat probability on the log-odds scale", "Intercept of a logistic model",
            pl_prior("normal", 0, 1.5),
            "Normal(0, 1.5) on log-odds gives a roughly flat prior on the probability (McElreath, 2020), unlike Normal(0, 10), which piles mass at 0 and 1.",
            "mcelreath"),
  pl_preset("General", "brms default for SDs (half-t)", "Residual SD or group-level SD",
            pl_prior("half_t", 3, 2.5),
            "brms uses Student-t(3, 0, 2.5) restricted to positive values (scaled up when the outcome's MAD exceeds 2.5). Median 1.9.",
            "brms"),
  pl_preset("General", "brms default for Student-t df", "Degrees of freedom \u{03bd} of a Student-t likelihood",
            pl_prior("gamma", 2, 0.1),
            "Gamma(2, 0.1) (Ju\u{00e1}rez & Steel, 2010): median \u{03bd} \u{2248} 17, with 6% below 4, which leaves room for heavy tails.",
            "juarez"),
  pl_preset("General", "PC prior for a random-effect SD", "Group-level SD \u{03c4}",
            pl_prior("exponential", 4.605),
            "Penalised-complexity prior (Simpson et al., 2017) stated as P(\u{03c4} > U) = \u{03b1}; here U = 1 and \u{03b1} = 0.01, giving Exponential(rate = \u{2212}log(0.01)/1). Build your own in the quick builders.",
            "simpson2017"),
  pl_preset("General", "Gelman (2006) half-Cauchy for \u{03c4}", "Group-level SD \u{03c4} (8 schools scale)",
            pl_prior("half_cauchy", 25),
            "Recommended by Gelman (2006) for the 8 schools example as a weakly informative alternative to Inverse-Gamma(\u{03b5}, \u{03b5}); rescale it to your outcome.",
            "gelman2006"),
  pl_preset("General", "Jeffreys prior for a proportion", "Probability \u{03b8}",
            pl_prior("beta", 0.5, 0.5),
            "Invariant to reparameterisation; worth one observation in total.", "jeffreys"),
  pl_preset("General", "Uniform prior for a proportion", "Probability \u{03b8}",
            pl_prior("beta", 1, 1),
            "Every probability equally likely; worth two observations (one success, one failure).", ""),

  # ---- health ---------------------------------------------------------------
  pl_preset("Health", "Sceptical prior for a trial effect", "Log odds ratio (or log hazard ratio)",
            pl_prior("normal", 0, 0.1749),
            "Centred on no effect, with only a 5% chance that the odds ratio is better than the clinically hoped-for 0.75 (Spiegelhalter, Freedman & Parmar, 1994). Make your own in the quick builders.",
            "spiegelhalter"),
  pl_preset("Health", "Enthusiastic prior for a trial effect", "Log odds ratio (or log hazard ratio)",
            pl_prior("normal", -0.2877, 0.1749),
            "Centred on the hoped-for odds ratio 0.75, with only a 5% chance of no benefit. Used alongside the sceptical prior to show the range of conclusions.",
            "spiegelhalter"),
  pl_preset("Health", "Weakly informative log hazard ratio", "Log hazard ratio",
            pl_prior("normal", 0, 1),
            "95% of prior mass on hazard ratios between 0.14 and 7.1, which rules out implausibly huge effects while letting the data speak.", ""),
  pl_preset("Health", "Meta-analysis heterogeneity (log OR)", "Between-study SD \u{03c4} of log odds ratios",
            pl_prior("half_normal", 0.5),
            "Recommended by R\u{00f6}ver et al. (2021) as a reasonably conservative weakly informative choice: P(\u{03c4} > 1) = 4.6%. For outcome-specific empirical priors on \u{03c4}\u{00b2} see Turner et al. (2015).",
            "rover2021"),
  pl_preset("Health", "Weibull shape (survival)", "Weibull shape k",
            pl_prior("lognormal", 0, 0.5),
            "Centred on a constant hazard (k = 1); 95% between 0.38 (falling hazard) and 2.7 (rising hazard).", ""),
  pl_preset("Health", "Diagnostic sensitivity from a pilot (example)", "Sensitivity or specificity",
            pl_prior("beta_proportion", 0.85, 20),
            "Example of an informative prior: a pilot suggesting about 85%, given the weight of 20 patients. 90% interval 0.70 to 0.96. Replace with your own numbers.", ""),
  pl_preset("Health", "Prevalence with little prior knowledge", "Prevalence",
            pl_prior("beta", 1, 1),
            "Uniform on [0, 1]. For rare conditions, a Beta with small mean (e.g. Beta(1, 20)) is usually more honest.", ""),

  # ---- education ------------------------------------------------------------
  pl_preset("Education", "Sceptical effect of an education intervention", "Standardised effect size (SD units)",
            pl_prior("normal", 0, 0.15),
            "Kraft (2020) finds most effects of education interventions on achievement are below 0.20 SD; this prior puts 82% of its mass within \u{00b1}0.2.",
            "kraft2020"),
  pl_preset("Education", "Weakly informative effect size", "Standardised effect size (SD units)",
            pl_prior("normal", 0, 0.5),
            "95% of mass within \u{00b1}0.98 SD: allows large effects but not absurd ones.", ""),
  pl_preset("Education", "Between-module spread of pass rates", "Group-level SD \u{03c4} on the log-odds scale",
            pl_prior("half_normal", 0.5),
            "Median \u{03c4} = 0.34, which implies the top-10% module has about 2.4 times the odds of passing of the bottom-10% module. Check it in Hierarchical priors.", ""),
  pl_preset("Education", "Likert cutpoints", "Each cutpoint of an ordinal (cumulative logit) model",
            pl_prior("normal", 0, 1.5),
            "Keeps thresholds within about \u{00b1}2.9 on the log-odds scale, so no response category is ruled out in advance.", ""),
  pl_preset("Education", "IRT person ability", "Ability \u{03b8} of each student",
            pl_prior("normal", 0, 1),
            "Standard choice that also fixes the scale of the latent trait, which identifies the model.", ""),
  pl_preset("Education", "IRT item discrimination", "Discrimination a of each item (2PL)",
            pl_prior("lognormal", 0, 0.5),
            "Positive by construction, median 1, 90% between 0.44 and 2.3.", ""),
  pl_preset("Education", "IRT item difficulty", "Difficulty b of each item",
            pl_prior("normal", 0, 2),
            "On the same scale as ability; 95% of items between \u{2212}3.9 and 3.9.", ""),
  pl_preset("Education", "Share of students using a tool", "Proportion",
            pl_prior("beta", 2, 2),
            "Gently favours middle values; 90% between 0.14 and 0.86.", "")
)

PL_REFERENCES <- c(
  rstanarm = "Gabry J, Goodrich B. Prior distributions for rstanarm models (rstanarm vignette).",
  gelman2008 = "Gelman A, Jakulin A, Pittau MG, Su Y-S (2008). A weakly informative default prior distribution for logistic and other regression models. Annals of Applied Statistics 2(4):1360\u{2013}1383.",
  mcelreath = "McElreath R (2020). Statistical Rethinking, 2nd ed. CRC Press.",
  brms = "B\u{00fc}rkner P-C (2017). brms: an R package for Bayesian multilevel models using Stan. Journal of Statistical Software 80(1).",
  juarez = "Ju\u{00e1}rez MA, Steel MFJ (2010). Model-based clustering of non-Gaussian panel data based on skew-t distributions. Journal of Business & Economic Statistics 28(1):52\u{2013}66.",
  simpson2017 = "Simpson D, Rue H, Riebler A, Martins TG, S\u{00f8}rbye SH (2017). Penalising model component complexity: a principled, practical approach to constructing priors. Statistical Science 32(1):1\u{2013}28.",
  gelman2006 = "Gelman A (2006). Prior distributions for variance parameters in hierarchical models. Bayesian Analysis 1(3):515\u{2013}534.",
  jeffreys = "Jeffreys H (1946). An invariant form for the prior probability in estimation problems. Proc. Royal Society A 186:453\u{2013}461.",
  spiegelhalter = "Spiegelhalter DJ, Freedman LS, Parmar MKB (1994). Bayesian approaches to randomized trials. JRSS Series A 157(3):357\u{2013}416.",
  rover2021 = "R\u{00f6}ver C, Bender R, Dias S, Schmid CH, Schmidli H, Sturtz S, Weber S, Friede T (2021). On weakly informative prior distributions for the heterogeneity parameter in Bayesian random-effects meta-analysis. Research Synthesis Methods 12(4):448\u{2013}474.",
  turner2015 = "Turner RM, Jackson D, Wei Y, Thompson SG, Higgins JPT (2015). Predictive distributions for between-study heterogeneity and simple methods for their application in Bayesian meta-analysis. Statistics in Medicine 34(6):984\u{2013}998.",
  kraft2020 = "Kraft MA (2020). Interpreting effect sizes of education interventions. Educational Researcher 49(4):241\u{2013}253."
)

# Quick builders --------------------------------------------------------------------

#' Sceptical / enthusiastic priors (Spiegelhalter et al. 1994) for a ratio effect
pl_build_trial_priors <- function(ratio, tail = 0.05) {
  m <- log(ratio); s <- abs(m) / qnorm(1 - tail)
  list(sceptical = pl_prior("normal", 0, s), enthusiastic = pl_prior("normal", m, s))
}

#' PC prior for an SD: P(sigma > U) = alpha  ->  Exponential(-log(alpha) / U)
pl_build_pc_sd <- function(U, alpha) pl_prior("exponential", -log(alpha) / U)

#' Effect size prior: mass `p` inside +/- d  ->  Normal(0, d / qnorm((1 + p)/2))
pl_build_effect <- function(d, p) pl_prior("normal", 0, d / qnorm((1 + p) / 2))
