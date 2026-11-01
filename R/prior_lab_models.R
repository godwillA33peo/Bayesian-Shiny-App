# ============================================================================
# Prior Lab: Model Lab data models
# ============================================================================
# Each model knows
#   - its parameter blocks and data-scaled default priors (rstanarm-style
#     "weakly informative, scaled to your data")
#   - its pointwise log-likelihood (R), and how to simulate replicate data
#   - how to write itself as a complete Stan program
#   - an example data set with a health or education story
#
# Linear predictor: eta = alpha + b * x, where x is the (centred) predictor.
# Slope priors are scaled by sd(x) so "per unit of x" stays sensible.
# ============================================================================

pl_round <- function(x, digits = 2) signif(x, digits)

pl_eta <- function(p, d) {
  base <- if (is.null(p$alpha)) 0 else p$alpha
  if (is.null(d$x)) rep(base, d$N) else base + p$b * d$x
}

pl_log_add <- function(a, b) {
  m <- pmax(a, b)
  ifelse(is.infinite(m) & m < 0, -Inf, m + log(exp(a - m) + exp(b - m)))
}

# Blocks shared by most models
pl_blk_alpha <- function(desc) pl_par_block("alpha", "real", 1, "Intercept \u{03b1}", desc)
pl_blk_b <- function(desc) pl_par_block("b", "real", 1, "Slope \u{03b2}", desc)

# ---- model definitions ------------------------------------------------------------------

PL_MODELS <- list()

PL_MODELS$normal <- list(
  label = "Normal (continuous)", group = "Continuous", y_type = "real",
  stan_y = "vector[N] y;",
  effect = list(f = identity, label = "\u{03b2}: change in the mean of y per unit of x"),
  blocks = function(has_x, K) c(
    list(pl_blk_alpha("Mean of y at the average x")),
    if (has_x) list(pl_blk_b("Change in the mean of y per unit of x")),
    list(pl_par_block("sigma", "positive", 1, "Residual SD \u{03c3}", "Spread of y around its mean"))),
  defaults = function(d) {
    sy <- sd(d$y)
    list(alpha = pl_prior("normal", pl_round(mean(d$y), 3), pl_round(2.5 * sy)),
         b = pl_prior("normal", 0, pl_round(2.5 * sy / max(d$sdx, 1e-8))),
         sigma = pl_prior("exponential", pl_round(1 / sy)))
  },
  loglik = function(p, d) dnorm(d$y, pl_eta(p, d), p$sigma, log = TRUE),
  rng = function(p, d) rnorm(d$N, pl_eta(p, d), p$sigma),
  stan_lik = "y ~ normal(eta, sigma);",
  stan_gq = "array[N] real y_rep = normal_rng(eta, sigma);",
  stan_ll = "normal_lpdf(y[n] | eta[n], sigma)"
)

PL_MODELS$student_t <- list(
  label = "Student-t (continuous, robust to outliers)", group = "Continuous", y_type = "real",
  stan_y = "vector[N] y;",
  effect = list(f = identity, label = "\u{03b2}: change in the location of y per unit of x"),
  blocks = function(has_x, K) c(
    list(pl_blk_alpha("Location of y at the average x")),
    if (has_x) list(pl_blk_b("Change in location per unit of x")),
    list(pl_par_block("sigma", "positive", 1, "Scale \u{03c3}", "Spread of y around its location"),
         pl_par_block("nu", "positive", 1, "Degrees of freedom \u{03bd}", "Small \u{03bd} = heavy tails (outliers); large \u{03bd} = Normal"))),
  defaults = function(d) {
    sy <- mad(d$y); if (sy <= 0) sy <- sd(d$y)
    list(alpha = pl_prior("normal", pl_round(median(d$y), 3), pl_round(2.5 * sy)),
         b = pl_prior("normal", 0, pl_round(2.5 * sy / max(d$sdx, 1e-8))),
         sigma = pl_prior("exponential", pl_round(1 / sy)),
         nu = pl_prior("gamma", 2, 0.1))
  },
  loglik = function(p, d) dt((d$y - pl_eta(p, d)) / p$sigma, p$nu, log = TRUE) - log(p$sigma),
  rng = function(p, d) pl_eta(p, d) + p$sigma * rt(d$N, p$nu),
  stan_lik = "y ~ student_t(nu, eta, sigma);",
  stan_gq = "array[N] real y_rep = student_t_rng(nu, eta, sigma);",
  stan_ll = "student_t_lpdf(y[n] | nu, eta[n], sigma)"
)

PL_MODELS$lognormal <- list(
  label = "Log-normal (positive, skewed)", group = "Positive", y_type = "positive",
  stan_y = "vector<lower=0>[N] y;",
  effect = list(f = exp, label = "exp(\u{03b2}): multiplicative change in y per unit of x"),
  blocks = function(has_x, K) c(
    list(pl_blk_alpha("Mean of log(y) at the average x")),
    if (has_x) list(pl_blk_b("Change in log(y) per unit of x")),
    list(pl_par_block("sigma", "positive", 1, "SD of log(y) \u{03c3}", "Spread on the log scale"))),
  defaults = function(d) {
    ly <- log(d$y); s <- sd(ly)
    list(alpha = pl_prior("normal", pl_round(mean(ly), 3), pl_round(2.5 * s)),
         b = pl_prior("normal", 0, pl_round(1 / max(d$sdx, 1e-8))),
         sigma = pl_prior("exponential", pl_round(1 / s)))
  },
  loglik = function(p, d) dlnorm(d$y, pl_eta(p, d), p$sigma, log = TRUE),
  rng = function(p, d) rlnorm(d$N, pl_eta(p, d), p$sigma),
  stan_lik = "y ~ lognormal(eta, sigma);",
  stan_gq = "array[N] real y_rep = lognormal_rng(eta, sigma);",
  stan_ll = "lognormal_lpdf(y[n] | eta[n], sigma)"
)

PL_MODELS$gamma <- list(
  label = "Gamma (positive, skewed; log link)", group = "Positive", y_type = "positive",
  stan_y = "vector<lower=0>[N] y;",
  effect = list(f = exp, label = "exp(\u{03b2}): multiplicative change in the mean of y per unit of x"),
  blocks = function(has_x, K) c(
    list(pl_blk_alpha("log of the mean of y at the average x")),
    if (has_x) list(pl_blk_b("Change in log-mean per unit of x")),
    list(pl_par_block("shape", "positive", 1, "Shape", "Larger shape = less skew (CV = 1/\u{221a}shape)"))),
  defaults = function(d) {
    list(alpha = pl_prior("normal", pl_round(log(mean(d$y)), 3), 2),
         b = pl_prior("normal", 0, pl_round(1 / max(d$sdx, 1e-8))),
         shape = pl_prior("lognormal", 1, 1))
  },
  loglik = function(p, d) dgamma(d$y, shape = p$shape, rate = p$shape / exp(pl_eta(p, d)), log = TRUE),
  rng = function(p, d) rgamma(d$N, shape = p$shape, rate = p$shape / exp(pl_eta(p, d))),
  stan_lik = "y ~ gamma(shape, shape ./ exp(eta));",
  stan_gq = "array[N] real y_rep = gamma_rng(shape, shape ./ exp(eta));",
  stan_ll = "gamma_lpdf(y[n] | shape, shape / exp(eta[n]))"
)

PL_MODELS$bernoulli <- list(
  label = "Bernoulli / logistic (yes-no)", group = "Binary & counts", y_type = "binary",
  stan_y = "array[N] int<lower=0, upper=1> y;",
  effect = list(f = exp, label = "exp(\u{03b2}): odds ratio per unit of x"),
  blocks = function(has_x, K) c(
    list(pl_blk_alpha("Log-odds of y = 1 at the average x")),
    if (has_x) list(pl_blk_b("Change in log-odds per unit of x"))),
  defaults = function(d) {
    list(alpha = pl_prior("normal", 0, 1.5),
         b = pl_prior("normal", 0, pl_round(1 / max(d$sdx, 1e-8))))
  },
  loglik = function(p, d) {
    eta <- pl_eta(p, d)
    d$y * plogis(eta, log.p = TRUE) + (1 - d$y) * plogis(-eta, log.p = TRUE)
  },
  rng = function(p, d) rbinom(d$N, 1, plogis(pl_eta(p, d))),
  stan_lik = "y ~ bernoulli_logit(eta);",
  stan_gq = "array[N] int y_rep = bernoulli_logit_rng(eta);",
  stan_ll = "bernoulli_logit_lpmf(y[n] | eta[n])"
)

PL_MODELS$poisson <- list(
  label = "Poisson (counts)", group = "Binary & counts", y_type = "count",
  stan_y = "array[N] int<lower=0> y;",
  effect = list(f = exp, label = "exp(\u{03b2}): rate ratio per unit of x"),
  blocks = function(has_x, K) c(
    list(pl_blk_alpha("log of the mean count at the average x")),
    if (has_x) list(pl_blk_b("Change in log-rate per unit of x"))),
  defaults = function(d) {
    list(alpha = pl_prior("normal", pl_round(log(mean(d$y) + 0.1), 3), 1.5),
         b = pl_prior("normal", 0, pl_round(1 / max(d$sdx, 1e-8))))
  },
  loglik = function(p, d) dpois(d$y, exp(pl_eta(p, d)), log = TRUE),
  rng = function(p, d) {
    lam <- pmin(exp(pl_eta(p, d)), 1e9)
    suppressWarnings(as.numeric(rpois(d$N, lam)))
  },
  stan_lik = "y ~ poisson_log(eta);",
  stan_gq = "array[N] int y_rep = poisson_log_rng(eta);",
  stan_ll = "poisson_log_lpmf(y[n] | eta[n])"
)

PL_MODELS$negbinomial <- list(
  label = "Negative binomial (over-dispersed counts)", group = "Binary & counts", y_type = "count",
  stan_y = "array[N] int<lower=0> y;",
  effect = list(f = exp, label = "exp(\u{03b2}): rate ratio per unit of x"),
  blocks = function(has_x, K) c(
    list(pl_blk_alpha("log of the mean count at the average x")),
    if (has_x) list(pl_blk_b("Change in log-rate per unit of x")),
    list(pl_par_block("phi", "positive", 1, "Shape \u{03c6}", "Var = \u{03bc} + \u{03bc}\u{00b2}/\u{03c6}: small \u{03c6} = strong over-dispersion"))),
  defaults = function(d) {
    list(alpha = pl_prior("normal", pl_round(log(mean(d$y) + 0.1), 3), 1.5),
         b = pl_prior("normal", 0, pl_round(1 / max(d$sdx, 1e-8))),
         phi = pl_prior("exponential", 0.1))
  },
  loglik = function(p, d) dnbinom(d$y, size = p$phi, mu = exp(pl_eta(p, d)), log = TRUE),
  rng = function(p, d) {
    mu <- pmin(exp(pl_eta(p, d)), 1e9)
    suppressWarnings(as.numeric(rnbinom(d$N, size = p$phi, mu = mu)))
  },
  stan_lik = "y ~ neg_binomial_2_log(eta, phi);",
  stan_gq = "array[N] int y_rep = neg_binomial_2_log_rng(eta, phi);",
  stan_ll = "neg_binomial_2_log_lpmf(y[n] | eta[n], phi)"
)

PL_MODELS$zip <- list(
  label = "Zero-inflated Poisson (counts with extra zeros)", group = "Binary & counts", y_type = "count",
  stan_y = "array[N] int<lower=0> y;",
  effect = list(f = exp, label = "exp(\u{03b2}): rate ratio per unit of x (among non-structural zeros)"),
  blocks = function(has_x, K) c(
    list(pl_blk_alpha("log of the mean count for the 'at risk' group at the average x")),
    if (has_x) list(pl_blk_b("Change in log-rate per unit of x")),
    list(pl_par_block("zi", "unit", 1, "Zero-inflation \u{03c0}", "Share of structural zeros (e.g. people who never use it)"))),
  defaults = function(d) {
    pos <- d$y[d$y > 0]
    m <- if (length(pos)) mean(pos) else 1
    list(alpha = pl_prior("normal", pl_round(log(m), 3), 1.5),
         b = pl_prior("normal", 0, pl_round(1 / max(d$sdx, 1e-8))),
         zi = pl_prior("beta", 1, 1))
  },
  loglik = function(p, d) {
    eta <- pl_eta(p, d)
    lam <- exp(eta)
    ifelse(d$y == 0,
           pl_log_add(log(p$zi), log1p(-p$zi) - lam),
           log1p(-p$zi) + dpois(d$y, lam, log = TRUE))
  },
  rng = function(p, d) {
    lam <- pmin(exp(pl_eta(p, d)), 1e9)
    y <- suppressWarnings(as.numeric(rpois(d$N, lam)))
    y[runif(d$N) < p$zi] <- 0
    y
  },
  stan_lik = paste(
    "for (n in 1:N) {",
    "    if (y[n] == 0)",
    "      target += log_sum_exp(log(zi), log1m(zi) + poisson_log_lpmf(0 | eta[n]));",
    "    else",
    "      target += log1m(zi) + poisson_log_lpmf(y[n] | eta[n]);",
    "  }", sep = "\n"),
  stan_gq = paste(
    "array[N] int y_rep;",
    "  for (n in 1:N) y_rep[n] = bernoulli_rng(zi) ? 0 : poisson_log_rng(eta[n]);", sep = "\n"),
  stan_ll = "y[n] == 0 ? log_sum_exp(log(zi), log1m(zi) + poisson_log_lpmf(0 | eta[n])) : log1m(zi) + poisson_log_lpmf(y[n] | eta[n])"
)

PL_MODELS$beta <- list(
  label = "Beta (proportions strictly between 0 and 1)", group = "Proportions & ordinal", y_type = "unit",
  stan_y = "vector<lower=0, upper=1>[N] y;",
  effect = list(f = exp, label = "exp(\u{03b2}): odds ratio for the mean proportion per unit of x"),
  blocks = function(has_x, K) c(
    list(pl_blk_alpha("Log-odds of the mean proportion at the average x")),
    if (has_x) list(pl_blk_b("Change in log-odds per unit of x")),
    list(pl_par_block("phi", "positive", 1, "Precision \u{03c6}", "Larger \u{03c6} = proportions cluster tightly around the mean"))),
  defaults = function(d) {
    list(alpha = pl_prior("normal", pl_round(qlogis(mean(d$y)), 3), 1.5),
         b = pl_prior("normal", 0, pl_round(1 / max(d$sdx, 1e-8))),
         phi = pl_prior("gamma", 2, 0.1))
  },
  loglik = function(p, d) {
    mu <- plogis(pl_eta(p, d))
    dbeta(d$y, mu * p$phi, (1 - mu) * p$phi, log = TRUE)
  },
  rng = function(p, d) {
    mu <- plogis(pl_eta(p, d))
    rbeta(d$N, mu * p$phi, (1 - mu) * p$phi)
  },
  stan_lik = "y ~ beta_proportion(inv_logit(eta), phi);",
  stan_gq = "array[N] real y_rep = beta_proportion_rng(inv_logit(eta), phi);",
  stan_ll = "beta_proportion_lpdf(y[n] | inv_logit(eta[n]), phi)"
)

PL_MODELS$ordinal <- list(
  label = "Ordinal / cumulative logit (Likert items)", group = "Proportions & ordinal", y_type = "ordinal",
  stan_y = "int<lower=2> K;\n  array[N] int<lower=1, upper=K> y;",
  effect = list(f = exp, label = "exp(\u{03b2}): cumulative odds ratio of a higher category per unit of x"),
  blocks = function(has_x, K) c(
    list(pl_par_block("cut", "ordered", K - 1, "Cutpoints c\u{2081} < \u{2026} < c\u{2096}\u{208b}\u{2081}",
                      "Log-odds thresholds between categories (same prior on each)")),
    if (has_x) list(pl_blk_b("Shift towards higher categories per unit of x (log-odds)"))),
  defaults = function(d) {
    list(cut = pl_prior("normal", 0, 2),
         b = pl_prior("normal", 0, pl_round(1 / max(d$sdx, 1e-8))))
  },
  loglik = function(p, d) {
    eta <- if (is.null(d$x)) rep(0, d$N) else p$b * d$x
    cc <- c(-Inf, p$cut, Inf)
    up <- cc[d$y + 1]; lo <- cc[d$y]
    pr <- plogis(up - eta) - plogis(lo - eta)
    ifelse(pr > 0, log(pr), -Inf)
  },
  rng = function(p, d) {
    eta <- if (is.null(d$x)) rep(0, d$N) else p$b * d$x
    u <- runif(d$N)
    cum <- vapply(p$cut, function(ck) plogis(ck - eta), numeric(d$N))
    if (is.null(dim(cum))) cum <- matrix(cum, nrow = d$N)
    1 + rowSums(u > cum)
  },
  stan_lik = "y ~ ordered_logistic(eta, cut);",
  stan_gq = "array[N] int y_rep;\n  for (n in 1:N) y_rep[n] = ordered_logistic_rng(eta[n], cut);",
  stan_ll = "ordered_logistic_lpmf(y[n] | eta[n], cut)"
)

PL_MODELS$weibull <- list(
  label = "Weibull survival (time to event, with censoring)", group = "Time to event", y_type = "positive",
  needs_event = TRUE,
  stan_y = "vector<lower=0>[N] y;                  // follow-up time\n  array[N] int<lower=0, upper=1> event;   // 1 = event, 0 = censored",
  effect = list(f = exp, label = "exp(\u{03b2}): time ratio per unit of x (> 1 = longer time to event)"),
  blocks = function(has_x, K) c(
    list(pl_blk_alpha("log of the Weibull scale (typical time to event) at the average x")),
    if (has_x) list(pl_blk_b("Change in log-time per unit of x (accelerated failure time)")),
    list(pl_par_block("shape", "positive", 1, "Shape k", "k = 1: constant hazard; k > 1: rising; k < 1: falling"))),
  defaults = function(d) {
    list(alpha = pl_prior("normal", pl_round(log(median(d$y)), 3), 1.5),
         b = pl_prior("normal", 0, pl_round(1 / max(d$sdx, 1e-8))),
         shape = pl_prior("lognormal", 0, 0.5))
  },
  loglik = function(p, d) {
    sc <- exp(pl_eta(p, d))
    ifelse(d$event == 1,
           dweibull(d$y, p$shape, sc, log = TRUE),
           pweibull(d$y, p$shape, sc, lower.tail = FALSE, log.p = TRUE))
  },
  rng = function(p, d) rweibull(d$N, p$shape, exp(pl_eta(p, d))),
  stan_lik = paste(
    "for (n in 1:N) {",
    "    if (event[n] == 1) target += weibull_lpdf(y[n] | shape, exp(eta[n]));",
    "    else target += weibull_lccdf(y[n] | shape, exp(eta[n]));",
    "  }", sep = "\n"),
  stan_gq = "array[N] real y_rep = weibull_rng(shape, exp(eta));  // uncensored event times",
  stan_ll = "event[n] == 1 ? weibull_lpdf(y[n] | shape, exp(eta[n])) : weibull_lccdf(y[n] | shape, exp(eta[n]))"
)

pl_model_choices <- function() {
  groups <- unique(vapply(PL_MODELS, `[[`, "", "group"))
  setNames(lapply(groups, function(g) {
    keys <- names(PL_MODELS)[vapply(PL_MODELS, `[[`, "", "group") == g]
    setNames(keys, vapply(PL_MODELS[keys], `[[`, "", "label"))
  }), groups)
}

# ---- example data sets -------------------------------------------------------------------

PL_EXAMPLES <- list(
  normal = list(
    title = "Exam score vs weekly study hours (education)", y_label = "Exam score (0\u{2013}100)", x_label = "Study hours per week",
    make = function() {
      set.seed(101); n <- 120; x <- round(runif(n, 0, 15), 1)
      y <- round(pmin(100, pmax(0, 55 + 1.8 * (x - 7.5) + rnorm(n, 0, 10))))
      data.frame(y = y, x = x)
    }),
  student_t = list(
    title = "Exam score vs study hours, with a few outliers (education)", y_label = "Exam score (0\u{2013}100)", x_label = "Study hours per week",
    make = function() {
      set.seed(102); n <- 120; x <- round(runif(n, 0, 15), 1)
      y <- 55 + 1.8 * (x - 7.5) + rnorm(n, 0, 8)
      out <- sample(n, 6); y[out] <- y[out] - runif(6, 30, 45)
      data.frame(y = round(pmin(100, pmax(0, y))), x = x)
    }),
  lognormal = list(
    title = "Hospital length of stay vs age (health)", y_label = "Length of stay (days)", x_label = "Age (decades)",
    make = function() {
      set.seed(103); n <- 150; x <- round(runif(n, 2, 9), 1)
      data.frame(y = round(rlnorm(n, 1.2 + 0.15 * (x - 5.5), 0.6), 1) + 0.1, x = x)
    }),
  gamma = list(
    title = "Treatment cost vs number of comorbidities (health)", y_label = "Cost (\u{20ac}1000s)", x_label = "Comorbidities",
    make = function() {
      set.seed(104); n <- 150; x <- rpois(n, 1.5)
      mu <- exp(0.8 + 0.25 * (x - 1.5))
      data.frame(y = round(rgamma(n, shape = 2.5, rate = 2.5 / mu), 2) + 0.01, x = x)
    }),
  bernoulli = list(
    title = "Pass (1) / fail (0) vs attendance (education)", y_label = "Passed", x_label = "Attendance (%)",
    make = function() {
      set.seed(105); n <- 200; x <- round(runif(n, 30, 100))
      data.frame(y = rbinom(n, 1, plogis(0.8 + 0.05 * (x - 65))), x = x)
    }),
  poisson = list(
    title = "Weekly logins to the learning platform vs year of study (education)", y_label = "Logins per week", x_label = "Year of study",
    make = function() {
      set.seed(106); n <- 160; x <- sample(1:4, n, replace = TRUE)
      data.frame(y = rpois(n, exp(1.6 + 0.15 * (x - 2.5))), x = x)
    }),
  negbinomial = list(
    title = "GP visits in a year vs chronic conditions (health)", y_label = "GP visits", x_label = "Chronic conditions",
    make = function() {
      set.seed(107); n <- 200; x <- rpois(n, 1.2)
      data.frame(y = rnbinom(n, size = 1.5, mu = exp(0.9 + 0.35 * (x - 1.2))), x = x)
    }),
  zip = list(
    title = "Weekly GenAI uses vs year of study, many non-users (education)", y_label = "GenAI uses per week", x_label = "Year of study",
    make = function() {
      set.seed(108); n <- 200; x <- sample(1:4, n, replace = TRUE)
      y <- rpois(n, exp(1.3 + 0.2 * (x - 2.5))); y[runif(n) < 0.4] <- 0
      data.frame(y = y, x = x)
    }),
  beta = list(
    title = "Share of module content completed vs weeks enrolled (education)", y_label = "Proportion completed", x_label = "Weeks enrolled",
    make = function() {
      set.seed(109); n <- 150; x <- sample(1:12, n, replace = TRUE)
      mu <- plogis(0.3 + 0.18 * (x - 6.5))
      y <- rbeta(n, mu * 8, (1 - mu) * 8)
      data.frame(y = round(pmin(pmax(y, 0.005), 0.995), 3), x = x)
    }),
  ordinal = list(
    title = "\u{201c}GenAI helps me learn\u{201d} (1\u{2013}5 Likert) vs weekly GenAI hours (education)", y_label = "Agreement (1 = strongly disagree, 5 = strongly agree)", x_label = "GenAI hours per week",
    make = function() {
      set.seed(110); n <- 220; x <- round(rgamma(n, 2, 0.5), 1)
      cuts <- c(-2, -0.7, 0.6, 2)
      eta <- 0.25 * (x - mean(x))
      u <- runif(n)
      y <- 1 + rowSums(outer(u, rep(1, 4)) > plogis(outer(-eta, cuts, `+`)))
      data.frame(y = y, x = x)
    }),
  weibull = list(
    title = "Weeks to dropout vs engagement score, censored at 30 weeks (education)", y_label = "Weeks to dropout", x_label = "Engagement score",
    make = function() {
      set.seed(111); n <- 180; x <- round(rnorm(n, 0, 1), 2)
      t <- rweibull(n, 1.3, exp(3.4 + 0.3 * x))
      cens <- pmin(30, runif(n, 10, 60))
      data.frame(y = round(pmin(t, cens), 2), x = x, event = as.integer(t <= cens))
    })
)

# ---- data preparation ------------------------------------------------------------------------

#' Validate an outcome for a model; returns NULL if fine, otherwise a message
pl_check_y <- function(model, y, event = NULL) {
  m <- PL_MODELS[[model]]
  if (any(!is.finite(y))) return("The outcome has missing or non-numeric values.")
  if (length(y) < 5) return("Need at least 5 observations.")
  switch(m$y_type,
    positive = if (any(y <= 0)) "This model needs a strictly positive outcome." else NULL,
    unit = if (any(y <= 0 | y >= 1)) "The Beta model needs proportions strictly between 0 and 1 (no exact 0s or 1s)." else NULL,
    binary = if (!all(y %in% c(0, 1))) "The Bernoulli model needs an outcome coded 0/1." else NULL,
    count = if (any(y < 0 | y != round(y))) "This model needs whole-number counts \u{2265} 0." else NULL,
    ordinal = if (any(y < 1 | y != round(y)) || length(unique(y)) < 2) "The ordinal model needs categories coded 1, 2, \u{2026}, K." else NULL,
    NULL) %||%
    (if (isTRUE(m$needs_event) && (is.null(event) || !all(event %in% c(0, 1)))) "Survival needs an event column coded 1 = event, 0 = censored." else NULL)
}

#' Build the data list used by the likelihood: y, centred x (or NULL), event, K
pl_model_data <- function(model, y, x = NULL, event = NULL, K = NULL) {
  d <- list(y = as.numeric(y), N = length(y))
  if (!is.null(x)) {
    d$x_mean <- mean(x)
    d$x <- as.numeric(x) - d$x_mean
    d$sdx <- sd(x)
    if (!is.finite(d$sdx) || d$sdx == 0) { d$x <- NULL; d$sdx <- 1 }
  } else d$sdx <- 1
  if (!is.null(event)) d$event <- as.numeric(event)
  if (model == "ordinal") d$K <- if (is.null(K)) max(y) else max(K, max(y))
  d
}

# ---- log posterior ---------------------------------------------------------------------------------

#' Build the unconstrained log-posterior for a model, data and priors (named list)
pl_model_logpost <- function(model, d, priors) {
  m <- PL_MODELS[[model]]
  blocks <- m$blocks(!is.null(d$x), d$K)
  idx <- pl_block_index(blocks)
  function(z) {
    p <- pl_constrain(z, blocks, idx)
    lp <- attr(p, "logJ")
    for (b in blocks) {
      lp <- lp + sum(pl_logd(priors[[b$name]], p[[b$name]]))
      if (!is.finite(lp)) return(-Inf)
    }
    ll <- suppressWarnings(m$loglik(p, d))
    lp + sum(ll)
  }
}

#' A data-based starting point on the unconstrained scale
pl_model_init <- function(model, d, priors) {
  m <- PL_MODELS[[model]]
  blocks <- m$blocks(!is.null(d$x), d$K)
  par <- list()
  for (b in blocks) {
    pr <- priors[[b$name]]
    med <- tryCatch(pl_q(pr, 0.5), error = function(e) NA)
    if (b$type == "ordered") {
      # cutpoints at the empirical cumulative log-odds
      cp <- cumsum(tabulate(d$y, d$K))[-d$K] / d$N
      cp <- pmin(pmax(cp, 0.02), 0.98)
      v <- qlogis(cp); v <- v + cumsum(c(0, pmax(0, 0.05 - diff(v))))
      par[[b$name]] <- v
    } else if (b$name == "alpha") {
      par$alpha <- switch(m$y_type,
                          real = mean(d$y), positive = if (model == "lognormal") mean(log(d$y)) else log(mean(d$y)),
                          binary = qlogis(min(max(mean(d$y), 0.02), 0.98)), count = log(mean(d$y) + 0.1),
                          unit = qlogis(mean(d$y)), med)
      if (model == "weibull") par$alpha <- log(mean(d$y))
    } else if (b$name == "b") {
      par$b <- 0
    } else if (b$name == "sigma") {
      par$sigma <- if (model == "lognormal") sd(log(d$y)) else sd(d$y)
    } else {
      v <- if (is.finite(med)) med else 1
      if (b$type == "unit") v <- min(max(v, 0.05), 0.95)
      if (b$type == "positive") v <- max(v, 1e-3)
      par[[b$name]] <- v
    }
  }
  pl_unconstrain(par, blocks)
}

#' Fit a model with the adaptive Metropolis sampler. Returns constrained draws etc.
pl_model_fit <- function(model, d, priors, n_warmup = 1000, n_iter = 1000, n_chains = 4) {
  m <- PL_MODELS[[model]]
  blocks <- m$blocks(!is.null(d$x), d$K)
  idx <- pl_block_index(blocks)
  lp <- pl_model_logpost(model, d, priors)
  z0 <- pl_model_init(model, d, priors)
  if (!is.finite(lp(z0))) {
    # fall back to prior medians if the data-based start is impossible under the prior
    z0 <- pl_unconstrain(setNames(lapply(blocks, function(b) {
      v <- pl_q(priors[[b$name]], 0.5)
      if (b$type == "ordered") sort(pl_q(priors[[b$name]], (seq_len(b$size)) / (b$size + 1))) else v
    }), vapply(blocks, `[[`, "", "name")), blocks)
  }
  t0 <- Sys.time()
  res <- pl_mcmc(lp, z0, n_warmup, n_iter, n_chains)
  # constrain every draw
  dims <- dim(res$draws)
  flat <- matrix(res$draws, ncol = dims[3])
  cons <- t(apply(flat, 1, function(z) unlist(pl_constrain(z, blocks, idx), use.names = FALSE)))
  if (dims[3] == 1) cons <- matrix(cons, ncol = 1)
  arr <- array(cons, dim = c(dims[1], dims[2], ncol(cons)))
  names_flat <- pl_flat_names(blocks)
  list(model = model, blocks = blocks, idx = idx, draws = arr, names = names_flat,
       accept = res$accept, seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
       data = d, priors = priors)
}

#' Turn row i of the flattened draws into a named parameter list
pl_draw_par <- function(fit, flat, i) {
  p <- list()
  for (b in fit$blocks) p[[b$name]] <- flat[i, fit$idx[[b$name]]]
  p
}

pl_fit_flat <- function(fit) matrix(fit$draws, ncol = dim(fit$draws)[3])

#' Simulate parameter draws from the priors (ordered blocks: sorted, which is
#' exactly the prior implied by independent priors restricted to ordering)
pl_prior_draws <- function(model, d, priors, S = 400) {
  m <- PL_MODELS[[model]]
  blocks <- m$blocks(!is.null(d$x), d$K)
  lapply(seq_len(S), function(i) {
    p <- list()
    for (b in blocks) {
      v <- pl_r(priors[[b$name]], b$size)
      p[[b$name]] <- if (b$type == "ordered") sort(v) else v
    }
    p
  })
}

#' Replicated data sets from a list of parameter draws
pl_yrep <- function(model, d, par_list) {
  m <- PL_MODELS[[model]]
  t(vapply(par_list, function(p) {
    y <- tryCatch(m$rng(p, d), error = function(e) rep(NA_real_, d$N))
    as.numeric(y)
  }, numeric(d$N)))
}

#' Pointwise log-likelihood matrix [draws, obs] for WAIC
pl_loglik_matrix <- function(fit, max_draws = 2000) {
  flat <- pl_fit_flat(fit)
  S <- nrow(flat)
  use <- if (S > max_draws) round(seq(1, S, length.out = max_draws)) else seq_len(S)
  m <- PL_MODELS[[fit$model]]
  t(vapply(use, function(i) m$loglik(pl_draw_par(fit, flat, i), fit$data), numeric(fit$data$N)))
}

# ---- Stan program -----------------------------------------------------------------------------------

pl_stan_decl <- function(b, prior) {
  if (b$type == "ordered") return(sprintf("ordered[K - 1] %s;", b$name))
  lo <- NULL; hi <- NULL
  if (b$type == "positive") lo <- 0
  if (b$type == "unit") { lo <- 0; hi <- 1 }
  if (!is.null(prior) && prior$family == "uniform") {
    lo <- max(c(lo, prior$params[["a"]])); hi <- min(c(hi, prior$params[["b"]]))
  }
  bounds <- c(if (!is.null(lo)) paste0("lower=", pl_fmt(lo)), if (!is.null(hi)) paste0("upper=", pl_fmt(hi)))
  sprintf("real%s %s;", if (length(bounds)) paste0("<", paste(bounds, collapse = ", "), ">") else "", b$name)
}

pl_stan_model_program <- function(model, has_x, priors, K = NULL) {
  m <- PL_MODELS[[model]]
  blocks <- m$blocks(has_x, if (is.null(K)) 5 else K)
  data_lines <- c("int<lower=1> N;", m$stan_y, if (has_x) "vector[N] x;                  // predictor, centred at its mean")
  par_lines <- vapply(blocks, function(b) {
    paste0(pl_stan_decl(b, priors[[b$name]]), "  // ", b$label)
  }, "")
  prior_lines <- vapply(blocks, function(b) pl_stan_prior_stmt(priors[[b$name]], b$name), "")
  has_alpha <- any(vapply(blocks, `[[`, "", "name") == "alpha")
  eta <- if (has_alpha) {
    if (has_x) "alpha + b * x" else "rep_vector(alpha, N)"
  } else {
    if (has_x) "b * x" else "rep_vector(0, N)"
  }
  ind <- function(txt) paste0("  ", txt)   # continuation lines carry their own indent
  paste0(
    "// ", m$label, "\n",
    "// Generated by Bayesian Explorer \u{2013} Prior Lab\n",
    "data {\n", paste0("  ", data_lines, collapse = "\n"), "\n}\n",
    "parameters {\n", paste0("  ", par_lines, collapse = "\n"), "\n}\n",
    "model {\n",
    "  vector[N] eta = ", eta, ";\n",
    "  // priors\n", paste0("  ", prior_lines, collapse = "\n"), "\n",
    "  // likelihood\n", ind(m$stan_lik), "\n}\n",
    "generated quantities {\n",
    "  vector[N] eta = ", eta, ";\n",
    ind(m$stan_gq), "\n",
    "  vector[N] log_lik;\n",
    "  for (n in 1:N) log_lik[n] = ", m$stan_ll, ";\n",
    "}\n"
  )
}

#' R code that fits the generated program with cmdstanr
pl_stan_r_code <- function(model, has_x, file = "model.stan") {
  m <- PL_MODELS[[model]]
  dl <- c("N = nrow(d)", "y = d$y")
  if (model == "ordinal") dl <- c(dl, "K = max(d$y)")
  if (isTRUE(m$needs_event)) dl <- c(dl, "event = d$event")
  if (has_x) dl <- c(dl, "x = d$x - mean(d$x)")
  paste0(
    "library(cmdstanr)   # or rstan::stan(file = \"", file, "\", data = stan_data)\n",
    "# d: a data frame with column y", if (has_x) " and x" else "", if (isTRUE(m$needs_event)) " and event" else "", "\n",
    "stan_data <- list(", paste(dl, collapse = ", "), ")\n",
    "mod <- cmdstan_model(\"", file, "\")\n",
    "fit <- mod$sample(data = stan_data, chains = 4, parallel_chains = 4, seed = 2026)\n",
    "fit$summary()\n",
    "# Model comparison: loo::loo(fit$draws(\"log_lik\", format = \"matrix\"))\n",
    "# Posterior predictive check: bayesplot::ppc_dens_overlay(d$y, fit$draws(\"y_rep\", format = \"matrix\")[1:50, ])"
  )
}
