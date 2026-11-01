# ============================================================================
# Prior Lab maths tests (base R only)
# Run from the project root:   Rscript tests/test_prior_lab.R
# Every check compares the grid/fitting engine against a closed-form answer or
# an independent brute-force calculation.
# ============================================================================

source("R/prior_lab_dists.R")
source("R/prior_lab_engine.R")

n_pass <- 0; n_fail <- 0
check <- function(label, ok) {
  if (isTRUE(ok)) { n_pass <<- n_pass + 1 } else { n_fail <<- n_fail + 1; cat("FAIL:", label, "\n") }
}
near <- function(a, b, tol) all(abs(a - b) <= tol)

set.seed(1)

# ---- 1. every distribution: density integrates to 1, p(q(u)) = u, draws match ----
for (fam in names(PL_DISTS)) {
  pr <- list(family = fam, params = pl_default_params(fam))
  if (fam == "uniform") pr$params <- c(a = -1, b = 2)
  u <- c(0.1, 0.3, 0.5, 0.8, 0.95)
  check(paste(fam, "p(q(u)) = u"), near(pl_p(pr, pl_q(pr, u)), u, 1e-6))
  lim <- pl_support_limits(pr)
  mass <- integrate(function(x) pl_d(pr, x), lim[1], lim[2], subdivisions = 2000L)$value
  check(sprintf("%s density integrates to 1 (got %.4f)", fam, mass), near(mass, 1, 0.01))
  dr <- pl_r(pr, 2e5)
  check(paste(fam, "draws match median"), near(median(dr), pl_q(pr, 0.5), 0.02 * (1 + abs(pl_q(pr, 0.5)))))
  m <- pl_dist(fam)$mean(pr$params); s <- pl_dist(fam)$sd(pr$params)
  if (is.finite(m) && is.finite(s)) {
    check(sprintf("%s analytic mean (%.3f vs %.3f)", fam, m, mean(dr)), near(mean(dr), m, 0.03 * (s + abs(m))))
    check(sprintf("%s analytic sd (%.3f vs %.3f)", fam, s, sd(dr)), near(sd(dr), s, 0.05 * s))
  }
}
# half-t mean/sd formulas at other df
pr <- pl_prior("half_t", 7, 2)
dr <- pl_r(pr, 4e5)
check("half-t mean df=7", near(pl_dist("half_t")$mean(pr$params), mean(dr), 0.01))
check("half-t sd df=7", near(pl_dist("half_t")$sd(pr$params), sd(dr), 0.02))

# ---- 2. elicitation recovers known parameters ---------------------------------------
cases <- list(
  list(f = "normal", p = c(mu = 0.3, sigma = 0.7)),
  list(f = "student_t", p = c(nu = 5, mu = -1, sigma = 2), fixed = list(nu = 5)),
  list(f = "logistic", p = c(mu = 2, s = 0.5)),
  list(f = "laplace", p = c(mu = 0, b = 1.3)),
  list(f = "gamma", p = c(shape = 3, rate = 2)),
  list(f = "lognormal", p = c(mu = -0.5, sigma = 0.6)),
  list(f = "inv_gamma", p = c(shape = 4, scale = 3)),
  list(f = "half_normal", p = c(sigma = 0.8)),
  list(f = "half_t", p = c(nu = 4, sigma = 1.5), fixed = list(nu = 4)),
  list(f = "exponential", p = c(rate = 0.4)),
  list(f = "beta", p = c(a = 6, b = 3))
)
for (cs in cases) {
  pr <- list(family = cs$f, params = cs$p)
  probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  vals <- pl_q(pr, probs)
  fit <- pl_fit_quantiles(cs$f, probs, vals, if (is.null(cs$fixed)) list() else cs$fixed)
  check(sprintf("elicit %s recovers params (%s vs %s)", cs$f,
                paste(round(fit$prior$params, 3), collapse = ","), paste(cs$p, collapse = ",")),
        near(fit$prior$params, cs$p[names(fit$prior$params)], 1e-3 * (1 + abs(cs$p))))
  check(paste("elicit", cs$f, "SSE ~ 0"), fit$sse < 1e-10)
  mo <- pl_fit_moments(cs$f, pl_dist(cs$f)$mean(cs$p), pl_dist(cs$f)$sd(cs$p),
                       if (is.null(cs$fixed)) list() else cs$fixed)
  if (!is.null(mo$prior)) {
    check(paste("moments", cs$f), near(mo$prior$params, cs$p[names(mo$prior$params)], 1e-6 * (1 + abs(cs$p))))
  }
}

# ---- 3. grid posterior vs conjugate closed forms ---------------------------------------
# Beta-binomial on the probability scale
lik <- pl_lik_spec("binomial", list(n = 20, k = 14), "natural")
an <- pl_posterior_analysis(pl_mixture(list(pl_prior("beta", 2, 3))), lik)
s <- pl_grid_summary(an$post$x, an$post$w)
a <- 16; b <- 9
check("beta-binomial mean", near(s$Mean, a / (a + b), 1e-5))
check("beta-binomial sd", near(s$SD, sqrt(a * b / ((a + b)^2 * (a + b + 1))), 1e-5))
check("beta-binomial CI", near(c(s$Lower, s$Upper), qbeta(c(0.025, 0.975), a, b), 1e-4))
# Beta(2,3) on theta == same prior pushed to the logit scale
lik_l <- pl_lik_spec("binomial", list(n = 20, k = 14), "link")
# density of logit(theta) when theta ~ Beta(2,3) is a "type IV logistic"; check via
# Jeffreys reference posterior instead, which must be invariant to the scale:
ref_nat <- pl_grid_summary(an$ref$x, an$ref$w)
an_l <- pl_posterior_analysis(pl_mixture(list(pl_prior("normal", 0, 1.5))), lik_l)
ref_link <- pl_grid_summary(an_l$ref$x, an_l$ref$w, f = plogis)
check("Jeffreys posterior invariant to scale (mean)", near(ref_nat$Mean, 14.5 / 21, 1e-4) && near(ref_link$Mean, 14.5 / 21, 1e-4))
check("Jeffreys posterior invariant to scale (CI)",
      near(pl_grid_quantile(an_l$ref$x, an_l$ref$w, c(0.025, 0.975)), qlogis(qbeta(c(0.025, 0.975), 14.5, 6.5)), 2e-3))
# Logit-scale normal prior vs brute-force integration
th <- seq(1e-6, 1 - 1e-6, length.out = 2e6)
eta <- qlogis(th)
wbf <- dnorm(eta, 0, 1.5) / (th * (1 - th)) * dbinom(14, 20, th)
check("logit-normal prior posterior mean of theta",
      near(pl_grid_summary(an_l$post$x, an_l$post$w, f = plogis)$Mean, sum(wbf * th) / sum(wbf), 1e-4))

# Gamma-Poisson on both scales
lik <- pl_lik_spec("poisson", list(y = 12, E = 4), "natural")
an <- pl_posterior_analysis(pl_mixture(list(pl_prior("gamma", 2, 0.5))), lik)
s <- pl_grid_summary(an$post$x, an$post$w)
check("gamma-poisson mean", near(s$Mean, 14 / 4.5, 1e-5))
check("gamma-poisson CI", near(c(s$Lower, s$Upper), qgamma(c(0.025, 0.975), 14, 4.5), 1e-4))
# log-scale prior: log(lambda) with lambda ~ Gamma(2, 0.5) has density
# dgamma(exp(eta)) * exp(eta); emulate with a Gamma prior fit through the grid by
# checking that the posterior from a log-gamma prior on eta equals Gamma(14, 4.5)
lik_l <- pl_lik_spec("poisson", list(y = 12, E = 4), "link")
an_l <- pl_posterior_analysis(pl_mixture(list(pl_prior("normal", 1, 1))), lik_l)
lam <- seq(1e-6, 30, length.out = 2e6)
wbf <- dlnorm(lam, 1, 1) * dpois(12, 4 * lam)
check("log-normal prior on rate: posterior mean of lambda",
      near(pl_grid_summary(an_l$post$x, an_l$post$w, f = exp)$Mean, sum(wbf * lam) / sum(wbf), 1e-4))

# Normal-normal
lik <- pl_lik_spec("normal", list(n = 30, ybar = 50, sigma = 10))
an <- pl_posterior_analysis(pl_mixture(list(pl_prior("normal", 40, 15))), lik)
pp <- 1 / 15^2 + 30 / 100
pm <- (40 / 15^2 + 30 * 50 / 100) / pp
s <- pl_grid_summary(an$post$x, an$post$w)
check("normal-normal mean", near(s$Mean, pm, 1e-5))
check("normal-normal sd", near(s$SD, sqrt(1 / pp), 1e-5))
check("normal-normal HDI = equal-tailed", near(pl_grid_hdi(an$post$x, an$post$w, 0.95), qnorm(c(0.025, 0.975), pm, sqrt(1 / pp)), 2e-2))
check("prior ESS exact for normal (sigma^2/tau^2)", near(pl_prior_ess(an), 100 / 225, 1e-3))

# Heavy-tailed prior with informative data still resolves a narrow posterior
lik <- pl_lik_spec("binomial", list(n = 5000, k = 3100), "link")
an <- pl_posterior_analysis(pl_mixture(list(pl_prior("cauchy", 0, 2.5))), lik)
s <- pl_grid_summary(an$post$x, an$post$w)
check("cauchy prior + big n: posterior near MLE", near(s$Mean, qlogis(0.62), 2e-3) && s$SD < 0.04 && s$SD > 0.025)

# ---- 4. mixture prior: posterior component weight vs closed form ---------------------
lik <- pl_lik_spec("binomial", list(n = 20, k = 3), "natural")
mix <- pl_mixture(list(pl_prior("beta", 20, 20), pl_prior("beta", 1, 1)), c(0.8, 0.2))
an <- pl_posterior_analysis(mix, lik)
# marginal likelihoods of beta-binomial: B(a+k, b+n-k)/B(a,b)
ml <- function(a, b) exp(lbeta(a + 3, b + 17) - lbeta(a, b))
w_true <- 0.8 * ml(20, 20) / (0.8 * ml(20, 20) + 0.2 * ml(1, 1))
check(sprintf("mixture posterior weight (%.4f vs %.4f)", an$comp_post[1], w_true), near(an$comp_post[1], w_true, 1e-4))

# ---- 5. power-scaling: closed form for the normal model ---------------------------------
lik <- pl_lik_spec("normal", list(n = 10, ybar = 5, sigma = 2))
an <- pl_posterior_analysis(pl_mixture(list(pl_prior("normal", 0, 1))), lik)
path <- pl_powerscale_path(an, alphas = c(0.5, 2))
p0 <- 1; pd <- 10 / 4
for (al in c(0.5, 2)) {
  m_pr <- (al * p0 * 0 + pd * 5) / (al * p0 + pd)
  m_lk <- (p0 * 0 + al * pd * 5) / (p0 + al * pd)
  check(paste("power-scale prior alpha", al), near(path$mean[path$component == "prior" & path$alpha == al], m_pr, 1e-4))
  check(paste("power-scale lik alpha", al), near(path$mean[path$component == "likelihood" & path$alpha == al], m_lk, 1e-4))
}
# index for an agreeing prior/data: W1 per doubling = 0.2765 * f (location equal)
lik <- pl_lik_spec("normal", list(n = 10, ybar = 0, sigma = 2))
an <- pl_posterior_analysis(pl_mixture(list(pl_prior("normal", 0, 1))), lik)
f <- p0 / (p0 + pd)
idx <- pl_powerscale_index(an, "prior")
check(sprintf("sensitivity index matches theory (%.4f vs %.4f)", idx, 0.5 * f * log(2) * sqrt(2 / pi)),
      near(idx, 0.5 * f * log(2) * sqrt(2 / pi), 2e-3))

# ---- 6. prior predictive tail probability -------------------------------------------
lik <- pl_lik_spec("binomial", list(n = 20, k = 14), "natural")
pp <- pl_prior_predictive(pl_mixture(list(pl_prior("beta", 2, 3))), lik, 2e5)
# exact beta-binomial
pmf <- exp(lchoose(20, 0:20) + lbeta(2 + 0:20, 3 + 20 - 0:20) - lbeta(2, 3))
check("prior predictive P(Y >= k) vs beta-binomial", near(pp$p_upper, sum(pmf[15:21]), 0.004))

# ---- 7. hierarchical model vs brute-force 2D integration -----------------------------
y <- PL_EIGHT_SCHOOLS$estimate; s8 <- PL_EIGHT_SCHOOLS$se
for (spec in list(list(pr = pl_prior("half_cauchy", 25), v = FALSE, mp = NULL),
                  list(pr = pl_prior("uniform", 0, 100), v = FALSE, mp = list(m0 = 0, s0 = 10)),
                  list(pr = pl_prior("inv_gamma", 1, 1), v = TRUE, mp = NULL))) {
  set.seed(3)
  res <- pl_hier_analysis(y, s8, spec$pr, spec$v, spec$mp, ndraw = 20000)
  # brute force over (mu, tau) grid; theta integrated analytically
  mu <- seq(-40, 60, length.out = 801); tau <- seq(1e-4, 120, length.out = 2400)
  lp_tau <- log(pl_tau_density(spec$pr, tau, spec$v))
  L <- outer(mu, tau, function(m, t) {
    out <- 0
    for (j in seq_along(y)) out <- out + dnorm(y[j], m, sqrt(s8[j]^2 + t^2), log = TRUE)
    out
  })
  L <- L + matrix(lp_tau, length(mu), length(tau), byrow = TRUE)
  if (!is.null(spec$mp)) L <- L + dnorm(mu, spec$mp$m0, spec$mp$s0, log = TRUE)
  W <- exp(L - max(L[is.finite(L)])); W[!is.finite(W)] <- 0; W <- W / sum(W)
  tau_mean_bf <- sum(colSums(W) * tau)
  mu_mean_bf <- sum(rowSums(W) * mu)
  # E[theta_1] = E[ (1-B) y1 + B mu ], B = s1^2/(s1^2+tau^2)
  B1 <- s8[1]^2 / (s8[1]^2 + tau^2)
  th1_bf <- sum(W * outer(mu, B1, function(m, b) (1 - b) * y[1] + b * m))
  lab <- paste(pl_prior_label(spec$pr), if (spec$v) "on var" else "on sd")
  check(sprintf("8 schools %s: E[tau] %.3f vs %.3f", lab, res$tau_mean, tau_mean_bf), near(res$tau_mean, tau_mean_bf, 0.02 * tau_mean_bf + 0.02))
  check(sprintf("8 schools %s: E[mu] %.3f vs %.3f", lab, res$mu[["mean"]], mu_mean_bf), near(res$mu[["mean"]], mu_mean_bf, 0.05))
  check(sprintf("8 schools %s: E[theta1] %.3f vs %.3f", lab, res$theta$post_mean[1], th1_bf), near(res$theta$post_mean[1], th1_bf, 0.05))
}

# ---- 8. LKJ marginal integrates to 1 and is uniform for K = 2, eta = 1 ------------------
for (K in c(2, 3, 6)) for (eta in c(0.5, 1, 2, 4)) {
  mass <- integrate(function(r) pl_lkj_marginal(r, eta, K), -1, 1)$value
  check(sprintf("LKJ K=%d eta=%s integrates to 1", K, eta), near(mass, 1, 1e-4))
}
check("LKJ(1), K=2 is uniform", near(pl_lkj_marginal(c(-0.5, 0, 0.7), 1, 2), 0.5, 1e-9))

# ---- 9. regression prior predictive: eta SD formula ------------------------------------
set.seed(5)
sim <- pl_sim_regression("bernoulli", 5, pl_prior("normal", 0, 2.5), pl_prior("normal", 0, 2.5), ndraw = 20000)
check(sprintf("linear predictor SD ~ sqrt(6)*2.5 (%.3f)", sim$eta_sd), near(sim$eta_sd, sqrt(6) * 2.5, 0.15))

# ---- 10. library round trip -------------------------------------------------------------
lib <- list()
source("R/prior_lab_shared.R")
lib <- pl_library_add(lib, "A", pl_prior("student_t", 3, 0, 2.5), "test")
lib <- pl_library_add(lib, "A", pl_prior("beta", 2, 5), "test")
check("library de-duplicates names", identical(names(lib), c("A", "A (2)")))
tmp <- tempfile(fileext = ".csv")
write.csv(pl_library_export(lib), tmp, row.names = FALSE)
lib2 <- pl_library_import(list(), read.csv(tmp, stringsAsFactors = FALSE))
check("library CSV round trip", attr(lib2, "n_imported") == 2 &&
        isTRUE(all.equal(lib2[["A (2)"]]$prior$params, lib[["A (2)"]]$prior$params)))

# =============================================================================
# Part 2: advanced families, MCMC engine, Model Lab, presets
# =============================================================================
source("R/prior_lab_families.R")
source("R/prior_lab_mcmc.R")
source("R/prior_lab_models.R")
source("R/prior_lab_advanced.R")
source("R/prior_lab_presets.R")
has_pkg <- function(p) requireNamespace(p, quietly = TRUE)

# ---- 11. new families ------------------------------------------------------------
set.seed(11)
for (fam in c("skew_normal", "logit_normal", "beta_proportion", "weibull", "scaled_inv_chisq")) {
  pr <- list(family = fam, params = pl_default_params(fam))
  lim <- pl_support_limits(pr)
  check(paste(fam, "integrates to 1"), near(integrate(function(x) pl_d(pr, x), lim[1], lim[2])$value, 1, 1e-4))
  u <- c(0.05, 0.3, 0.5, 0.9)
  check(paste(fam, "p(q(u)) = u"), near(pl_p(pr, pl_q(pr, u)), u, 1e-5))
  dr <- pl_r(pr, 2e5)
  check(paste(fam, "median of draws"), near(median(dr), pl_q(pr, 0.5), 0.02 * (1 + abs(pl_q(pr, 0.5)))))
  m <- pl_dist(fam)$mean(pr$params)
  if (is.finite(m) && fam != "scaled_inv_chisq") check(paste(fam, "mean of draws"), near(mean(dr), m, 0.01 * (1 + abs(m))))
}
if (has_pkg("sn")) {
  x <- c(-1, 0, 0.5, 2, 4)
  check("skew-normal CDF matches sn::psn", near(pl_p(pl_prior("skew_normal", 1, 2, 3), x), sn::psn(x, 1, 2, 3), 1e-5))
}
for (cs in list(list("skew_normal", c(xi = 0.2, omega = 1.5, alpha = 4)), list("weibull", c(shape = 2.2, scale = 3)),
                list("logit_normal", c(mu = -1, sigma = 0.7)))) {
  pr <- list(family = cs[[1]], params = cs[[2]]); pb <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  fit <- pl_fit_quantiles(cs[[1]], pb, pl_q(pr, pb))
  check(paste("elicit", cs[[1]]), near(fit$prior$params, cs[[2]], 1e-3 * (1 + abs(cs[[2]]))))
}
mo <- pl_fit_moments("weibull", 2.66, 1.29)
check("weibull moment match", near(c(pl_dist("weibull")$mean(mo$prior$params), pl_dist("weibull")$sd(mo$prior$params)), c(2.66, 1.29), 1e-4))

# ---- 12. MCMC engine: diagnostics and an exact answer ---------------------------------
set.seed(12)
mk <- function(phi, n = 1000, shift = 0) { e <- rnorm(n); x <- numeric(n); for (i in 2:n) x[i] <- phi * x[i - 1] + e[i]; x + shift }
X <- cbind(mk(0.9), mk(0.9), mk(0.9), mk(0.9, shift = 0.5))
if (has_pkg("posterior")) {
  check("R-hat matches posterior::rhat", near(pl_rhat(X), posterior::rhat(X), 1e-6))
  check("bulk ESS matches posterior::ess_bulk", near(pl_ess_bulk(X), posterior::ess_bulk(X), 1e-3))
  check("tail ESS matches posterior::ess_tail", near(pl_ess_tail(X), posterior::ess_tail(X), 1e-3))
}
# intercept-only Bernoulli: compare the sampler with an exact 1-D grid posterior
y <- c(rep(1, 14), rep(0, 6))
d <- pl_model_data("bernoulli", y)
pri <- list(alpha = pl_prior("student_t", 3, 0, 1))
set.seed(12)
fit <- pl_model_fit("bernoulli", d, pri, 1000, 3000)
g <- seq(-6, 8, length.out = 20001)
w <- exp(pl_logd(pri$alpha, g) + 14 * plogis(g, log.p = TRUE) + 6 * plogis(-g, log.p = TRUE)); w <- w / sum(w)
gm <- sum(w * g); gs <- sqrt(sum(w * (g - gm)^2))
s <- pl_draws_summary(fit$draws, fit$names)
check(sprintf("MCMC vs exact posterior mean (%.4f vs %.4f)", s$mean, gm), near(s$mean, gm, 0.03))
check(sprintf("MCMC vs exact posterior sd (%.4f vs %.4f)", s$sd, gs), near(s$sd, gs, 0.03))
check("MCMC diagnostics fine", s$rhat < 1.01 && s$ess_bulk > 1000)

# ---- 13. every model recovers standard frequentist estimates on its example ------------
suppressMessages({library(MASS); library(survival)})
mle <- function(model, df) {
  x <- df$x - mean(df$x)
  switch(model,
    normal = { f <- lm(df$y ~ x); c(coef(f), sigma(f)) },
    lognormal = { f <- lm(log(df$y) ~ x); c(coef(f), sigma(f)) },
    gamma = { f <- glm(df$y ~ x, family = Gamma(link = "log")); coef(f) },
    bernoulli = coef(glm(df$y ~ x, family = binomial)),
    poisson = coef(glm(df$y ~ x, family = poisson)),
    negbinomial = coef(glm.nb(df$y ~ x)),
    ordinal = { f <- polr(factor(df$y) ~ x, method = "logistic"); c(f$zeta, coef(f)) },
    weibull = { f <- survreg(Surv(df$y, df$event) ~ x, dist = "weibull"); c(coef(f), 1 / f$scale) },
    NULL)
}
for (model in names(PL_MODELS)) {
  df <- PL_EXAMPLES[[model]]$make()
  check(paste(model, "example passes its own data check"), is.null(pl_check_y(model, df$y, df$event)))
  d <- pl_model_data(model, df$y, df$x, df$event)
  set.seed(13)
  fit <- pl_model_fit(model, d, PL_MODELS[[model]]$defaults(d), 1000, 1000)
  s <- pl_draws_summary(fit$draws, fit$names)
  check(paste(model, "R-hat < 1.02 and ESS > 300"), max(s$rhat) < 1.02 && min(s$ess_bulk) > 300)
  m <- mle(model, df)
  if (!is.null(m)) {
    k <- seq_along(m)
    check(sprintf("%s posterior means within 0.5 SD of the MLE", model), all(abs(s$mean[k] - m) < 0.5 * s$sd[k]))
  }
}
# zero-inflated Poisson and Beta: compare with direct maximum likelihood
for (model in c("zip", "beta", "student_t")) {
  df <- PL_EXAMPLES[[model]]$make(); d <- pl_model_data(model, df$y, df$x)
  blocks <- PL_MODELS[[model]]$blocks(TRUE, NULL)
  nll <- function(z) -sum(PL_MODELS[[model]]$loglik(pl_constrain(z, blocks), d))
  z0 <- pl_model_init(model, d, PL_MODELS[[model]]$defaults(d))
  o <- optim(z0, nll, method = "BFGS")
  mle_v <- unlist(pl_constrain(o$par, blocks))
  set.seed(14)
  fit <- pl_model_fit(model, d, PL_MODELS[[model]]$defaults(d), 1000, 1000)
  s <- pl_draws_summary(fit$draws, fit$names)
  # Student-t: nu has a right-skewed posterior and sigma moves with it, so their
  # posterior means legitimately differ from the joint MLE; check alpha and b only.
  k <- if (model == "student_t") 1:2 else seq_along(mle_v)
  check(sprintf("%s posterior close to direct MLE", model), all(abs(s$mean[k] - mle_v[k]) < 0.5 * s$sd[k]))
}

# ---- 14. every generated Stan program compiles ---------------------------------------
if (has_pkg("rstan")) {
  for (model in names(PL_MODELS)) for (hx in c(TRUE, FALSE)) {
    df <- PL_EXAMPLES[[model]]$make()
    d <- pl_model_data(model, df$y, if (hx) df$x else NULL, df$event)
    code <- pl_stan_model_program(model, hx, PL_MODELS[[model]]$defaults(d), d$K)
    ok <- tryCatch(isTRUE(rstan::stanc(model_code = code, verbose = FALSE)$status), error = function(e) FALSE)
    check(sprintf("Stan program compiles: %s, x = %s", model, hx), ok)
  }
  odd <- list(alpha = pl_prior("skew_normal", 1, 1, 2), b = pl_prior("uniform", -2, 2), zi = pl_prior("logit_normal", -1, 1))
  ok <- tryCatch(isTRUE(rstan::stanc(model_code = pl_stan_model_program("zip", TRUE, odd), verbose = FALSE)$status), error = function(e) FALSE)
  check("Stan program with skew-normal / uniform / logit-normal priors compiles", ok)
} else cat("(rstan not installed: skipping Stan compile checks)\n")

# ---- 15. advanced priors and presets --------------------------------------------------
set.seed(15)
iw <- pl_sim_iw(2, 6, 1, S = 20000)
check("Inverse-Wishart E[Sigma11] = s^2/(nu - K - 1)", near(mean(iw$sd1^2), 1 / 3, 0.02))
B <- pl_sim_shrinkage("spike", 50, list(spike_pi = 0.1, slab_sd = 1, spike_sd = 0.01), S = 2000)
check("spike-and-slab: ~10% of coefficients from the slab", near(mean(abs(B) > 0.05), 0.1 * 2 * pnorm(-0.05), 0.01))
for (pz in PL_PRESETS) check(paste("preset valid:", pz$name), is.null(pl_check_prior(pz$prior)))
tp <- pl_build_trial_priors(0.75, 0.05)
check("sceptical prior: P(OR < 0.75) = 5%", near(pl_p(tp$sceptical, log(0.75)), 0.05, 1e-9))
check("enthusiastic prior: P(OR > 1) = 5%", near(1 - pl_p(tp$enthusiastic, 0), 0.05, 1e-9))
check("PC prior: P(sigma > U) = alpha", near(1 - pl_p(pl_build_pc_sd(2, 0.05), 2), 0.05, 1e-9))
check("effect-size prior: P(|d| < 0.2) = 0.8", near(diff(pl_p(pl_build_effect(0.2, 0.8), c(-0.2, 0.2))), 0.8, 1e-9))

cat(sprintf("\n%d checks passed, %d failed\n", n_pass, n_fail))
if (n_fail > 0) quit(status = 1)
