# ============================================================================
# Prior Lab: advanced prior families (extends PL_DISTS)
# ============================================================================
#   skew_normal       asymmetric beliefs about an effect
#   logit_normal      probability whose log-odds are Normal
#   beta_proportion   Beta written as mean mu and "prior sample size" kappa
#   weibull           positive, flexible shape (survival scale/shape)
#   scaled_inv_chisq  classic conjugate prior for a variance (BDA)
#
# Also: Stan statement helpers that handle families needing a Jacobian or a
# custom log-density (e.g. logit-normal written on the probability scale).
# ============================================================================

# ---- numeric CDF / quantile for families without a closed form ---------------
# A standardised density is tabulated once per shape value and cached.
PL_NUM_CACHE <- new.env(parent = emptyenv())

pl_numeric_table <- function(key, dfun, lo, hi, n = 6001) {
  if (!is.null(PL_NUM_CACHE[[key]])) return(PL_NUM_CACHE[[key]])
  x <- seq(lo, hi, length.out = n)
  d <- dfun(x)
  d[!is.finite(d)] <- 0
  cdf <- c(0, cumsum((d[-1] + d[-n]) / 2 * diff(x)))
  cdf <- cdf / cdf[n]
  # keep strictly increasing points for inverse interpolation
  keep <- c(TRUE, diff(cdf) > 0)
  tab <- list(x = x, cdf = cdf, xi = x[keep], cdfi = cdf[keep])
  if (length(ls(PL_NUM_CACHE)) > 200) rm(list = ls(PL_NUM_CACHE), envir = PL_NUM_CACHE)
  assign(key, tab, envir = PL_NUM_CACHE)
  tab
}

# Skew-normal (Azzalini): density 2/omega * phi(z) * Phi(alpha z), z = (x - xi)/omega
pl_sn_table <- function(alpha) {
  pl_numeric_table(sprintf("sn_%.12g", alpha),
                   function(z) 2 * dnorm(z) * pnorm(alpha * z), -12, 12)
}
pl_sn_delta <- function(alpha) alpha / sqrt(1 + alpha^2)

PL_DISTS$skew_normal <- list(
  label = "Skew-Normal(\u{03be}, \u{03c9}, \u{03b1})", short = "Skew-Normal", support = "real",
  pars = list(pl_par("xi", "Location \u{03be}", 0), pl_par("omega", "Scale \u{03c9}", 1, min = 0),
              pl_par("alpha", "Shape \u{03b1} (0 = Normal)", 3, step = 0.5)),
  d = function(x, p) 2 / p[["omega"]] * dnorm((x - p[["xi"]]) / p[["omega"]]) *
    pnorm(p[["alpha"]] * (x - p[["xi"]]) / p[["omega"]]),
  p = function(x, p) {
    tab <- pl_sn_table(p[["alpha"]])
    approx(tab$x, tab$cdf, (x - p[["xi"]]) / p[["omega"]], rule = 2)$y
  },
  q = function(u, p) {
    tab <- pl_sn_table(p[["alpha"]])
    p[["xi"]] + p[["omega"]] * approx(tab$cdfi, tab$xi, u, rule = 2, ties = "ordered")$y
  },
  r = function(n, p) {
    dl <- pl_sn_delta(p[["alpha"]])
    p[["xi"]] + p[["omega"]] * (dl * abs(rnorm(n)) + sqrt(1 - dl^2) * rnorm(n))
  },
  mean = function(p) p[["xi"]] + p[["omega"]] * pl_sn_delta(p[["alpha"]]) * sqrt(2 / pi),
  sd = function(p) p[["omega"]] * sqrt(1 - 2 * pl_sn_delta(p[["alpha"]])^2 / pi),
  stan = function(p) sprintf("skew_normal(%s, %s, %s)", pl_fmt(p[["xi"]]), pl_fmt(p[["omega"]]), pl_fmt(p[["alpha"]])),
  rcode = function(p) {
    dl <- pl_sn_delta(p[["alpha"]])
    sprintf("%s + %s * (%s * abs(rnorm(n)) + %s * rnorm(n))  # or sn::rsn(n, %s, %s, %s)",
            pl_fmt(p[["xi"]]), pl_fmt(p[["omega"]]), pl_fmt(dl, 6), pl_fmt(sqrt(1 - dl^2), 6),
            pl_fmt(p[["xi"]]), pl_fmt(p[["omega"]]), pl_fmt(p[["alpha"]]))
  }
)

PL_DISTS$logit_normal <- list(
  label = "Logit-Normal(\u{03bc}, \u{03c3})", short = "Logit-Normal", support = "unit",
  pars = list(pl_par("mu", "Log-odds mean \u{03bc}", 0), pl_par("sigma", "Log-odds SD \u{03c3}", 1, min = 0)),
  d = function(x, p) {
    out <- numeric(length(x))
    ok <- x > 0 & x < 1
    out[ok] <- dnorm(qlogis(x[ok]), p[["mu"]], p[["sigma"]]) / (x[ok] * (1 - x[ok]))
    out
  },
  p = function(x, p) ifelse(x <= 0, 0, ifelse(x >= 1, 1, pnorm(qlogis(pmin(pmax(x, 1e-300), 1 - 1e-16)), p[["mu"]], p[["sigma"]]))),
  q = function(u, p) plogis(qnorm(u, p[["mu"]], p[["sigma"]])),
  r = function(n, p) plogis(rnorm(n, p[["mu"]], p[["sigma"]])),
  mean = function(p) integrate(function(z) plogis(z) * dnorm(z, p[["mu"]], p[["sigma"]]),
                               -Inf, Inf)$value,
  sd = function(p) {
    m <- integrate(function(z) plogis(z) * dnorm(z, p[["mu"]], p[["sigma"]]), -Inf, Inf)$value
    m2 <- integrate(function(z) plogis(z)^2 * dnorm(z, p[["mu"]], p[["sigma"]]), -Inf, Inf)$value
    sqrt(max(m2 - m^2, 0))
  },
  stan = function(p) sprintf("normal(%s, %s)", pl_fmt(p[["mu"]]), pl_fmt(p[["sigma"]])),
  stan_lpdf = function(p, par) sprintf("normal_lpdf(logit(%s) | %s, %s) - log(%s) - log1m(%s)",
                                       par, pl_fmt(p[["mu"]]), pl_fmt(p[["sigma"]]), par, par),
  brms_note = "logit-normal on a probability = this Normal on the log-odds (e.g. the Intercept of a logistic model)",
  rcode = function(p) sprintf("plogis(rnorm(n, %s, %s))", pl_fmt(p[["mu"]]), pl_fmt(p[["sigma"]]))
)

PL_DISTS$beta_proportion <- list(
  label = "Beta(mean \u{03bc}, sample size \u{03ba})", short = "Beta(\u{03bc}, \u{03ba})", support = "unit",
  pars = list(pl_par("mu", "Mean \u{03bc}", 0.5, min = 0, step = 0.05),
              pl_par("kappa", "Prior sample size \u{03ba}", 10, min = 0, step = 1)),
  d = function(x, p) dbeta(x, p[["mu"]] * p[["kappa"]], (1 - p[["mu"]]) * p[["kappa"]]),
  p = function(x, p) pbeta(x, p[["mu"]] * p[["kappa"]], (1 - p[["mu"]]) * p[["kappa"]]),
  q = function(u, p) qbeta(u, p[["mu"]] * p[["kappa"]], (1 - p[["mu"]]) * p[["kappa"]]),
  r = function(n, p) rbeta(n, p[["mu"]] * p[["kappa"]], (1 - p[["mu"]]) * p[["kappa"]]),
  mean = function(p) p[["mu"]],
  sd = function(p) sqrt(p[["mu"]] * (1 - p[["mu"]]) / (p[["kappa"]] + 1)),
  stan = function(p) sprintf("beta_proportion(%s, %s)", pl_fmt(p[["mu"]]), pl_fmt(p[["kappa"]])),
  rcode = function(p) sprintf("rbeta(n, %s, %s)", pl_fmt(p[["mu"]] * p[["kappa"]]), pl_fmt((1 - p[["mu"]]) * p[["kappa"]])),
  check = function(p) if (p[["mu"]] >= 1) "Beta(\u{03bc}, \u{03ba}): the mean \u{03bc} must be below 1." else NULL
)

PL_DISTS$weibull <- list(
  label = "Weibull(shape k, scale \u{03bb})", short = "Weibull", support = "positive",
  pars = list(pl_par("shape", "Shape k", 1.5, min = 0), pl_par("scale", "Scale \u{03bb}", 1, min = 0)),
  d = function(x, p) dweibull(x, p[["shape"]], p[["scale"]]),
  p = function(x, p) pweibull(x, p[["shape"]], p[["scale"]]),
  q = function(u, p) qweibull(u, p[["shape"]], p[["scale"]]),
  r = function(n, p) rweibull(n, p[["shape"]], p[["scale"]]),
  mean = function(p) p[["scale"]] * gamma(1 + 1 / p[["shape"]]),
  sd = function(p) p[["scale"]] * sqrt(gamma(1 + 2 / p[["shape"]]) - gamma(1 + 1 / p[["shape"]])^2),
  stan = function(p) sprintf("weibull(%s, %s)", pl_fmt(p[["shape"]]), pl_fmt(p[["scale"]])),
  rcode = function(p) sprintf("rweibull(n, %s, %s)", pl_fmt(p[["shape"]]), pl_fmt(p[["scale"]]))
)

# Scaled-Inv-chi^2(nu, s^2) = Inverse-Gamma(nu/2, nu s^2 / 2); Stan takes s, not s^2
PL_DISTS$scaled_inv_chisq <- list(
  label = "Scaled-Inv-\u{03c7}\u{00b2}(\u{03bd}, s)", short = "Scaled-Inv-\u{03c7}\u{00b2}", support = "positive",
  pars = list(pl_par("nu", "df \u{03bd}", 5, min = 0, step = 1), pl_par("s", "Scale s (a guess at the SD)", 1, min = 0)),
  d = function(x, p) PL_DISTS$inv_gamma$d(x, c(shape = p[["nu"]] / 2, scale = p[["nu"]] * p[["s"]]^2 / 2)),
  p = function(x, p) PL_DISTS$inv_gamma$p(x, c(shape = p[["nu"]] / 2, scale = p[["nu"]] * p[["s"]]^2 / 2)),
  q = function(u, p) PL_DISTS$inv_gamma$q(u, c(shape = p[["nu"]] / 2, scale = p[["nu"]] * p[["s"]]^2 / 2)),
  r = function(n, p) PL_DISTS$inv_gamma$r(n, c(shape = p[["nu"]] / 2, scale = p[["nu"]] * p[["s"]]^2 / 2)),
  mean = function(p) if (p[["nu"]] > 2) p[["nu"]] * p[["s"]]^2 / (p[["nu"]] - 2) else NA_real_,
  sd = function(p) {
    nu <- p[["nu"]]
    if (nu > 4) sqrt(2 * nu^2 * p[["s"]]^4 / ((nu - 2)^2 * (nu - 4))) else NA_real_
  },
  stan = function(p) sprintf("scaled_inv_chi_square(%s, %s)", pl_fmt(p[["nu"]]), pl_fmt(p[["s"]])),
  rcode = function(p) sprintf("%s * %s^2 / rchisq(n, %s)", pl_fmt(p[["nu"]]), pl_fmt(p[["s"]]), pl_fmt(p[["nu"]]))
)

# Families offered to the elicitation fitter
PL_ELICIT_FAMILIES$real <- c(PL_ELICIT_FAMILIES$real, "skew_normal")
PL_ELICIT_FAMILIES$positive <- c(PL_ELICIT_FAMILIES$positive, "weibull")
PL_ELICIT_FAMILIES$unit <- c(PL_ELICIT_FAMILIES$unit, "logit_normal")

# Starting values for the new families (wraps the original function)
pl_elicit_starts_base <- pl_elicit_starts
pl_elicit_starts <- function(family, probs, vals, fixed = list()) {
  if (family == "skew_normal") {
    g <- pl_elicit_guess(probs, vals)
    m <- g[["m"]]; s <- g[["s"]]
    return(lapply(c(0, 2, -2, 6, -6), function(a) {
      dl <- pl_sn_delta(a); om <- s / sqrt(1 - 2 * dl^2 / pi)
      c(xi = m - om * dl * sqrt(2 / pi) * 0.8, omega = om, alpha = a)
    }))
  }
  if (family == "weibull") {
    lg <- pl_elicit_guess(probs, log(pmax(vals, 1e-12)))
    k <- pi / (sqrt(6) * lg[["s"]])
    med <- exp(lg[["m"]])
    base <- c(shape = k, scale = med / log(2)^(1 / k))
    return(list(base, base * c(0.5, 1), base * c(2, 1)))
  }
  if (family == "logit_normal") {
    lg <- pl_elicit_guess(probs, qlogis(pmin(pmax(vals, 1e-9), 1 - 1e-9)))
    base <- c(mu = lg[["m"]], sigma = lg[["s"]])
    return(list(base, base * c(1, 0.5), base * c(1, 2)))
  }
  pl_elicit_starts_base(family, probs, vals, fixed)
}

# Moment matching for the new families (wraps the original function)
pl_fit_moments_base <- pl_fit_moments
pl_fit_moments <- function(family, mean, sd, fixed = list()) {
  if (family == "weibull") {
    if (!is.finite(mean) || !is.finite(sd) || mean <= 0 || sd <= 0) {
      return(list(prior = NULL, sse = NA_real_, why = "Weibull needs a positive mean and SD."))
    }
    cv2 <- (sd / mean)^2
    f <- function(lk) { k <- exp(lk); gamma(1 + 2 / k) / gamma(1 + 1 / k)^2 - 1 - cv2 }
    root <- tryCatch(uniroot(f, c(log(0.05), log(200)))$root, error = function(e) NA)
    if (is.na(root)) return(list(prior = NULL, sse = NA_real_, why = "Weibull: no shape matches this mean and SD."))
    k <- exp(root)
    return(list(prior = list(family = "weibull", params = c(shape = k, scale = mean / gamma(1 + 1 / k))), sse = NA_real_))
  }
  if (family %in% c("skew_normal", "logit_normal")) {
    return(list(prior = NULL, sse = NA_real_, why = "use the quantile method for this family."))
  }
  pl_fit_moments_base(family, mean, sd, fixed)
}

# ---- Stan helpers aware of custom log-densities -----------------------------------

#' Log-density expression for `target +=` / log_mix(), e.g. "beta_lpdf(theta | 2, 2)"
pl_stan_lpdf_expr <- function(prior, par) {
  d <- pl_dist(prior$family)
  if (!is.null(d$stan_lpdf)) return(d$stan_lpdf(prior$params, par))
  s <- pl_stan_string(prior)
  out <- sub("^(\\w+)\\((.*)\\)$", paste0("\\1_lpdf(", par, " | \\2)"), s)
  if (prior$family %in% c("half_normal", "half_t", "half_cauchy")) out <- paste0(out, " + log(2)")
  out
}

#' A single prior statement: "theta ~ beta(2, 2);" or "target += ...;"
pl_stan_prior_stmt <- function(prior, par) {
  d <- pl_dist(prior$family)
  if (!is.null(d$stan_lpdf)) return(sprintf("target += %s;", d$stan_lpdf(prior$params, par)))
  sprintf("%s ~ %s;", par, pl_stan_string(prior))
}

# Builder code lines now use the custom-aware statement
pl_stan_line <- function(prior, name = "theta") pl_stan_prior_stmt(prior, name)

# Keep the dropdowns grouped by support: real, bounded, positive, unit
PL_DISTS <- PL_DISTS[c("normal", "student_t", "cauchy", "logistic", "laplace", "skew_normal",
                       "uniform",
                       "half_normal", "half_t", "half_cauchy", "exponential", "gamma", "inv_gamma",
                       "scaled_inv_chisq", "lognormal", "weibull",
                       "beta", "beta_proportion", "logit_normal")]

pl_brms_note <- function(prior) {
  note <- pl_dist(prior$family)$brms_note
  if (is.null(note)) "" else paste0(" (", note, ")")
}
