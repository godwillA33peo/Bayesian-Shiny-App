# ============================================================================
# Prior Lab: computational engine
# ============================================================================
# Pure functions (no Shiny) so they can be unit-tested from tests/:
#   1. Elicitation      - fit a family to quantile judgements or mean & SD
#   2. Grid posterior   - any prior (or two-component mixture) x Binomial /
#                          Poisson / Normal likelihood, on a natural or link scale
#   3. Sensitivity      - power-scaling of prior and likelihood
#   4. Prior predictive - for the single-parameter models and for GLMs
#   5. Hierarchical     - exact normal hierarchical model ("8 schools") with any
#                          prior on tau, and implied between-group spread
#   6. LKJ              - marginal prior of one correlation
# ============================================================================

# ---- 1. Elicitation ---------------------------------------------------------------

PL_ELICIT_FAMILIES <- list(
  real     = c("normal", "student_t", "logistic", "laplace"),
  positive = c("gamma", "lognormal", "inv_gamma", "half_normal", "half_t", "exponential"),
  unit     = c("beta")
)

# Rough location / spread from quantile judgements (used for starting values)
pl_elicit_guess <- function(probs, vals) {
  m <- if (length(vals) == 1) vals else approx(probs, vals, 0.5, rule = 2)$y
  s <- NA_real_
  if (length(vals) >= 2) {
    z <- qnorm(probs)
    s <- (max(vals) - min(vals)) / (max(z) - min(z))
  }
  if (!is.finite(s) || s <= 0) s <- abs(m) * 0.5 + 0.1
  c(m = m, s = s)
}

pl_elicit_starts <- function(family, probs, vals, fixed = list()) {
  g <- pl_elicit_guess(probs, vals)
  m <- g[["m"]]; s <- g[["s"]]
  nu <- if (!is.null(fixed$nu)) fixed$nu else 3
  mpos <- max(m, 1e-3)
  base <- switch(family,
    normal      = c(mu = m, sigma = s),
    student_t   = c(nu = nu, mu = m, sigma = s),
    logistic    = c(mu = m, s = s * sqrt(3) / pi),
    laplace     = c(mu = m, b = s / sqrt(2)),
    gamma       = c(shape = (mpos / s)^2, rate = mpos / s^2),
    lognormal   = {
      lg <- pl_elicit_guess(probs, log(pmax(vals, 1e-12)))
      c(mu = lg[["m"]], sigma = lg[["s"]])
    },
    inv_gamma   = {
      sh <- (mpos / s)^2 + 2
      c(shape = sh, scale = mpos * (sh - 1))
    },
    half_normal = c(sigma = median(vals / qnorm((1 + probs) / 2))),
    half_t      = c(nu = nu, sigma = median(vals / qt((1 + probs) / 2, nu))),
    exponential = c(rate = median(-log(1 - probs) / vals)),
    beta        = {
      mm <- min(max(m, 0.01), 0.99)
      v <- min(s^2, mm * (1 - mm) * 0.9)
      k <- mm * (1 - mm) / v - 1
      c(a = mm * k, b = (1 - mm) * k)
    },
    stop("No starting values for ", family)
  )
  # A few perturbed starts: scale-type parameters halved and doubled
  d <- pl_dist(family)
  pos <- vapply(d$pars, function(pp) is.finite(pp$min) && pp$min == 0, logical(1))
  names(pos) <- pl_par_names(family)
  free_pos <- setdiff(names(pos)[pos], names(fixed))
  starts <- list(base)
  for (f in c(0.5, 2)) {
    st <- base
    st[free_pos] <- st[free_pos] * f
    starts[[length(starts) + 1]] <- st
  }
  starts
}

#' Fit a prior family to quantile judgements by least squares on the CDF
#' (the approach used by the SHELF elicitation framework).
#' @param probs cumulative probabilities, e.g. c(0.05, 0.5, 0.95)
#' @param vals  the values the expert gave for those probabilities
#' @param fixed named list of parameters held fixed (e.g. list(nu = 5))
pl_fit_quantiles <- function(family, probs, vals, fixed = list()) {
  d <- pl_dist(family)
  nm <- pl_par_names(family)
  free <- setdiff(nm, names(fixed))
  is_pos <- vapply(d$pars, function(pp) is.finite(pp$min) && pp$min == 0, logical(1))
  names(is_pos) <- nm

  to_par <- function(z) {
    v <- z
    v[is_pos[free]] <- exp(z[is_pos[free]])
    full <- c(setNames(v, free), unlist(fixed))
    full[nm]
  }
  to_z <- function(par) {
    z <- par[free]
    z[is_pos[free]] <- log(z[is_pos[free]])
    z
  }
  obj <- function(z) {
    par <- to_par(z)
    if (any(!is.finite(par))) return(1e10)
    Fv <- tryCatch(suppressWarnings(d$p(vals, par)), error = function(e) rep(NA_real_, length(vals)))
    if (any(!is.finite(Fv))) return(1e10)
    sum((Fv - probs)^2)
  }

  best <- NULL
  for (s in pl_elicit_starts(family, probs, vals, fixed)) {
    z0 <- to_z(s)
    if (any(!is.finite(z0))) next
    fit <- if (length(free) == 1) {
      optim(z0, obj, method = "Brent", lower = z0 - 20, upper = z0 + 20)
    } else {
      f1 <- optim(z0, obj, method = "Nelder-Mead", control = list(maxit = 4000, reltol = 1e-14))
      optim(f1$par, obj, method = "Nelder-Mead", control = list(maxit = 4000, reltol = 1e-14))
    }
    if (is.null(best) || fit$value < best$value) best <- fit
  }
  if (is.null(best)) return(NULL)
  list(prior = list(family = family, params = to_par(best$par)), sse = best$value)
}

#' Match a family to a mean and SD. When impossible, `prior` is NULL and `why` says why.
pl_fit_moments <- function(family, mean, sd, fixed = list()) {
  fail <- function(why) list(prior = NULL, sse = NA_real_, why = why)
  if (!is.finite(mean) || !is.finite(sd) || sd <= 0) return(fail("Mean and SD must be finite, with SD > 0."))
  par <- switch(family,
    normal    = c(mu = mean, sigma = sd),
    student_t = {
      nu <- if (!is.null(fixed$nu)) fixed$nu else 3
      if (nu <= 2) return(fail("Student-t needs df > 2 to have a finite SD."))
      c(nu = nu, mu = mean, sigma = sd * sqrt((nu - 2) / nu))
    },
    logistic  = c(mu = mean, s = sd * sqrt(3) / pi),
    laplace   = c(mu = mean, b = sd / sqrt(2)),
    gamma     = {
      if (mean <= 0) return(fail("Gamma needs a positive mean."))
      c(shape = (mean / sd)^2, rate = mean / sd^2)
    },
    lognormal = {
      if (mean <= 0) return(fail("Log-Normal needs a positive mean."))
      s2 <- log(1 + sd^2 / mean^2)
      c(mu = log(mean) - s2 / 2, sigma = sqrt(s2))
    },
    inv_gamma = {
      if (mean <= 0) return(fail("Inverse-Gamma needs a positive mean."))
      sh <- (mean / sd)^2 + 2
      c(shape = sh, scale = mean * (sh - 1))
    },
    beta      = {
      if (mean <= 0 || mean >= 1) return(fail("Beta needs a mean between 0 and 1."))
      if (sd^2 >= mean * (1 - mean)) return(fail("SD is too large for a Beta with this mean (needs SD\u{00b2} < mean(1 - mean))."))
      k <- mean * (1 - mean) / sd^2 - 1
      c(a = mean * k, b = (1 - mean) * k)
    },
    return(fail("One-parameter family: use the quantile method instead."))
  )
  list(prior = list(family = family, params = par[pl_par_names(family)]), sse = NA_real_)
}

# ---- 2. Grid posterior for single-parameter models ---------------------------------

#' Likelihood specification.
#' type  "binomial" (data: n, k), "poisson" (data: y, E), "normal" (data: n, ybar, sigma)
#' scale "natural" puts the prior on theta / lambda / mu; "link" puts it on
#'       logit(theta) / log(lambda). Normal has only "natural".
pl_lik_spec <- function(type, data, scale = "natural") {
  if (type == "binomial") {
    n <- data$n; k <- data$k
    th_hat <- (k + 0.5) / (n + 1)
    if (scale == "link") {
      list(
        type = type, scale = scale, support = "real", has_data = n > 0,
        par_label = "logit(\u{03b8})  (log-odds)", nat_label = "\u{03b8} (probability)",
        to_nat = plogis,
        loglik = function(eta) lchoose(n, k) + k * plogis(eta, log.p = TRUE) + (n - k) * plogis(-eta, log.p = TRUE),
        ref_logprior = function(eta) 0.5 * plogis(eta, log.p = TRUE) + 0.5 * plogis(-eta, log.p = TRUE),
        ref_label = "Jeffreys Beta(\u{00bd}, \u{00bd})",
        centre = qlogis(th_hat), se = 1 / sqrt(max(n, 1) * th_hat * (1 - th_hat)),
        stat_label = "successes", obs = k, n = n
      )
    } else {
      list(
        type = type, scale = scale, support = "unit", has_data = n > 0,
        par_label = "\u{03b8} (probability)", nat_label = "\u{03b8} (probability)",
        to_nat = identity,
        loglik = function(th) dbinom(k, n, th, log = TRUE),
        ref_logprior = function(th) -0.5 * log(th) - 0.5 * log1p(-th),
        ref_label = "Jeffreys Beta(\u{00bd}, \u{00bd})",
        centre = th_hat, se = sqrt(th_hat * (1 - th_hat) / max(n, 1)),
        stat_label = "successes", obs = k, n = n
      )
    }
  } else if (type == "poisson") {
    y <- data$y; E <- data$E
    lam_hat <- (y + 0.5) / E
    if (scale == "link") {
      list(
        type = type, scale = scale, support = "real", has_data = TRUE,
        par_label = "log(\u{03bb})  (log-rate)", nat_label = "\u{03bb} (rate per unit exposure)",
        to_nat = exp,
        loglik = function(eta) y * (log(E) + eta) - E * exp(eta) - lgamma(y + 1),
        ref_logprior = function(eta) 0.5 * eta,
        ref_label = "Jeffreys \u{221d} \u{03bb}^(-\u{00bd})",
        centre = log(lam_hat), se = 1 / sqrt(y + 0.5),
        stat_label = "events", obs = y, E = E
      )
    } else {
      list(
        type = type, scale = scale, support = "positive", has_data = TRUE,
        par_label = "\u{03bb} (rate per unit exposure)", nat_label = "\u{03bb} (rate per unit exposure)",
        to_nat = identity,
        loglik = function(lam) dpois(y, E * lam, log = TRUE),
        ref_logprior = function(lam) -0.5 * log(lam),
        ref_label = "Jeffreys \u{221d} \u{03bb}^(-\u{00bd})",
        centre = lam_hat, se = sqrt(lam_hat / E),
        stat_label = "events", obs = y, E = E
      )
    }
  } else if (type == "normal") {
    n <- data$n; ybar <- data$ybar; sigma <- data$sigma
    se <- if (n > 0) sigma / sqrt(n) else Inf
    list(
      type = type, scale = "natural", support = "real", has_data = n > 0,
      par_label = "\u{03bc} (mean)", nat_label = "\u{03bc} (mean)",
      to_nat = identity,
      loglik = function(mu) if (n > 0) dnorm(ybar, mu, se, log = TRUE) else rep(0, length(mu)),
      ref_logprior = function(mu) rep(0, length(mu)),
      ref_label = "Flat",
      centre = ybar, se = if (n > 0) se else sigma,
      stat_label = "sample mean", obs = ybar, n = n, sigma = sigma
    )
  } else stop("Unknown likelihood type: ", type)
}

# Mixture prior: list(components = list(prior, ...), weights = c(...))
pl_mixture <- function(priors, weights = NULL) {
  if (is.null(weights)) weights <- rep(1 / length(priors), length(priors))
  list(components = priors, weights = weights / sum(weights))
}

pl_mix_logd <- function(mix, x) {
  lps <- vapply(seq_along(mix$components), function(i) {
    log(mix$weights[i]) + pl_logd(mix$components[[i]], x)
  }, numeric(length(x)))
  if (is.null(dim(lps))) lps <- matrix(lps, nrow = length(x))
  mx <- apply(lps, 1, max)
  out <- mx + log(rowSums(exp(lps - mx)))
  out[!is.finite(mx)] <- -Inf
  out
}

pl_mix_r <- function(mix, n) {
  comp <- sample.int(length(mix$components), n, replace = TRUE, prob = mix$weights)
  out <- numeric(n)
  for (i in seq_along(mix$components)) {
    idx <- which(comp == i)
    if (length(idx)) out[idx] <- pl_r(mix$components[[i]], length(idx))
  }
  out
}

# Wide range for the first (coarse) grid pass
pl_coarse_range <- function(mix, lik) {
  lim <- PL_SUPPORT_LIMITS[[lik$support]]
  rngs <- vapply(mix$components, function(pr) {
    d <- pl_dist(pr$family)
    if (d$support == "bounded") return(unname(pr$params[c("a", "b")]))
    pl_q(pr, c(1e-5, 1 - 1e-5))
  }, numeric(2))
  rng <- c(min(rngs[1, ]), max(rngs[2, ]))
  if (lik$has_data) rng <- range(c(rng, lik$centre - 12 * lik$se, lik$centre + 12 * lik$se))
  # widen so power-scaled priors (alpha < 1) are still covered
  ctr <- mean(rng); hw <- diff(rng) / 2
  rng <- c(ctr - 2 * hw, ctr + 2 * hw)
  c(max(rng[1], lim[1]), min(rng[2], lim[2]))
}

# Midpoint grid with log-weights from an unnormalised log-density function
pl_grid_eval <- function(logf, range, n) {
  h <- diff(range) / n
  x <- range[1] + (seq_len(n) - 0.5) * h
  lw <- logf(x)
  lw[is.nan(lw)] <- -Inf
  mx <- max(lw)
  if (!is.finite(mx)) return(NULL)
  w <- exp(lw - mx)
  list(x = x, h = h, lw = lw, w = w / sum(w), logZ = mx + log(sum(w)) + log(h))
}

#' Two-pass grid: coarse over a wide range, then fine over where the mass is.
pl_grid_fit <- function(logf, coarse_range, support, n_coarse = 20001, n_fine = 4001, tail = 1e-7) {
  lim <- PL_SUPPORT_LIMITS[[support]]
  g <- pl_grid_eval(logf, coarse_range, n_coarse)
  if (is.null(g)) return(NULL)
  cw <- cumsum(g$w)
  lo <- g$x[max(1, which(cw >= tail)[1] - 3)]
  hi <- g$x[min(length(g$x), which(cw >= 1 - tail)[1] + 3)]
  pad <- max(0.1 * (hi - lo), 3 * g$h)
  rng <- c(max(lo - pad, lim[1], coarse_range[1]), min(hi + pad, lim[2], coarse_range[2]))
  pl_grid_eval(logf, rng, n_fine)
}

# Summaries of a grid distribution (optionally after transforming x)
pl_grid_quantile <- function(x, w, u) {
  cw <- cumsum(w) - w / 2
  approx(cw, x, xout = u, rule = 2, ties = "ordered")$y
}

pl_grid_hdi <- function(x, w, level) {
  o <- order(w, decreasing = TRUE)
  keep <- o[seq_len(which(cumsum(w[o]) >= level)[1])]
  range(x[keep])
}

pl_grid_summary <- function(x, w, level = 0.95, interval = c("equal", "hdi"), threshold = NULL, f = identity) {
  interval <- match.arg(interval)
  z <- f(x)
  ok <- is.finite(z)
  z <- z[ok]; ww <- w[ok] / sum(w[ok])
  m <- sum(ww * z)
  a <- (1 - level) / 2
  ci <- if (interval == "hdi") pl_grid_hdi(z, ww, level) else pl_grid_quantile(z, ww, c(a, 1 - a))
  out <- data.frame(
    Mean = m,
    SD = sqrt(max(sum(ww * (z - m)^2), 0)),
    Median = pl_grid_quantile(z, ww, 0.5),
    Lower = ci[1], Upper = ci[2]
  )
  if (!is.null(threshold)) out$`P(> c)` <- sum(ww[z > threshold])
  out
}

#' Full analysis for a single-parameter model.
#' @param mix  prior mixture (pl_mixture)
#' @param lik  likelihood spec (pl_lik_spec)
pl_posterior_analysis <- function(mix, lik) {
  logprior <- function(x) pl_mix_logd(mix, x)
  logpost <- function(x) logprior(x) + lik$loglik(x)
  crange <- pl_coarse_range(mix, lik)
  post <- pl_grid_fit(logpost, crange, lik$support)
  if (is.null(post)) stop("The posterior could not be computed: prior and likelihood do not overlap on this scale.")
  prior_g <- pl_grid_fit(logprior, crange, lik$support)
  ref <- NULL
  if (lik$has_data) {
    ref <- tryCatch(pl_grid_fit(function(x) lik$ref_logprior(x) + lik$loglik(x), crange, lik$support),
                    error = function(e) NULL)
  }

  # Posterior probability of each mixture component
  comp_post <- NULL
  if (length(mix$components) > 1) {
    lz <- vapply(seq_along(mix$components), function(i) {
      lw <- pl_logd(mix$components[[i]], post$x) + lik$loglik(post$x)
      mx <- max(lw)
      if (!is.finite(mx)) return(-Inf)
      log(mix$weights[i]) + mx + log(sum(exp(lw - mx)))
    }, numeric(1))
    comp_post <- exp(lz - max(lz)) / sum(exp(lz - max(lz)))
  }

  list(mix = mix, lik = lik, post = post, prior = prior_g, ref = ref,
       logprior = logprior, logpost = logpost, comp_post = comp_post,
       coarse_range = crange)
}

# Display curves (prior, scaled likelihood, posterior, reference) on one grid
pl_posterior_curves <- function(an, n = 801) {
  lik <- an$lik
  lim <- PL_SUPPORT_LIMITS[[lik$support]]
  prior_rng <- pl_union_range(an$mix$components)
  post_rng <- pl_grid_quantile(an$post$x, an$post$w, c(0.0005, 0.9995))
  rng <- range(c(prior_rng, post_rng))
  if (!is.null(an$ref)) rng <- range(c(rng, pl_grid_quantile(an$ref$x, an$ref$w, c(0.001, 0.999))))
  # do not let a very diffuse prior squash the posterior into a spike
  pw <- diff(post_rng)
  rng[1] <- max(rng[1], post_rng[1] - 12 * pw)
  rng[2] <- min(rng[2], post_rng[2] + 12 * pw)
  rng <- c(max(rng[1], lim[1]), min(rng[2], lim[2]))
  h <- diff(rng) / n
  x <- rng[1] + (seq_len(n) - 0.5) * h

  dens_from <- function(lf, logZ) {
    v <- exp(lf(x) - logZ)
    v[!is.finite(v)] <- NA
    v
  }
  post_d <- dens_from(an$logpost, an$post$logZ)
  prior_d <- dens_from(an$logprior, an$prior$logZ)
  out <- data.frame(x = x, Prior = prior_d, Posterior = post_d)
  if (lik$has_data) {
    ll <- lik$loglik(x)
    lk <- exp(ll - max(ll[is.finite(ll)]))
    out$Likelihood <- lk * max(post_d, na.rm = TRUE)
  }
  if (!is.null(an$ref)) {
    out$Reference <- dens_from(function(z) lik$ref_logprior(z) + lik$loglik(z), an$ref$logZ)
  }
  out
}

# ---- 3. Power-scaling sensitivity -------------------------------------------------------

# Weights of the power-scaled posterior on a common grid
pl_powerscale_grid <- function(an, alpha, component = c("prior", "likelihood"), x) {
  component <- match.arg(component)
  lp <- an$logprior(x); ll <- an$lik$loglik(x)
  lw <- if (component == "prior") alpha * lp + ll else lp + alpha * ll
  lw[is.nan(lw)] <- -Inf
  w <- exp(lw - max(lw))
  w / sum(w)
}

pl_sens_grid_x <- function(an, n = 6001) {
  lim <- PL_SUPPORT_LIMITS[[an$lik$support]]
  rng <- range(an$post$x)
  w <- diff(rng)
  rng <- c(max(rng[1] - 0.6 * w, lim[1], an$coarse_range[1]),
           min(rng[2] + 0.6 * w, lim[2], an$coarse_range[2]))
  h <- diff(rng) / n
  rng[1] + (seq_len(n) - 0.5) * h
}

#' Posterior mean and interval as prior / likelihood power alpha varies.
pl_powerscale_path <- function(an, alphas = 2^seq(-1, 1, by = 0.25), level = 0.95) {
  x <- pl_sens_grid_x(an)
  a <- (1 - level) / 2
  rows <- list()
  for (comp in c("prior", "likelihood")) {
    if (comp == "likelihood" && !an$lik$has_data) next
    for (al in alphas) {
      w <- pl_powerscale_grid(an, al, comp, x)
      m <- sum(w * x)
      q <- pl_grid_quantile(x, w, c(a, 0.5, 1 - a))
      rows[[length(rows) + 1]] <- data.frame(component = comp, alpha = al, mean = m,
                                             lower = q[1], median = q[2], upper = q[3])
    }
  }
  do.call(rbind, rows)
}

#' Sensitivity index: Wasserstein-1 distance between the posteriors at
#' alpha = 2^-0.1 and 2^+0.1, expressed per doubling of alpha and in units of
#' the base posterior SD. Idea follows Kallioinen et al. (2023, Stat Comput),
#' "Detecting and diagnosing prior and likelihood sensitivity with
#' power-scaling"; the distance measure here is our own simpler choice.
pl_powerscale_index <- function(an, component) {
  x <- pl_sens_grid_x(an)
  h <- x[2] - x[1]
  w0 <- pl_powerscale_grid(an, 1, component, x)
  sd0 <- sqrt(sum(w0 * (x - sum(w0 * x))^2))
  wl <- pl_powerscale_grid(an, 2^-0.1, component, x)
  wh <- pl_powerscale_grid(an, 2^0.1, component, x)
  w1 <- sum(abs(cumsum(wh) - cumsum(wl))) * h
  w1 / (0.2 * sd0)
}

PL_SENS_THRESHOLD <- 0.05

#' @param ppc_p optional two-sided prior predictive tail probability; when both
#'   components are sensitive it separates genuine conflict from an
#'   informative prior that simply agrees less than perfectly with the data.
pl_sensitivity_table <- function(an, ppc_p = NULL) {
  ps <- pl_powerscale_index(an, "prior")
  ls <- if (an$lik$has_data) pl_powerscale_index(an, "likelihood") else NA_real_
  thr <- PL_SENS_THRESHOLD
  level <- "ok"
  diag <- if (is.na(ls)) {
    level <- "warn"
    "No data entered: the posterior is the prior."
  } else if (ps >= thr && ls >= thr && !is.null(ppc_p) && ppc_p < 0.05) {
    level <- "alert"
    "Prior\u{2013}data conflict: the posterior moves when either the prior or the likelihood is re-weighted, and the data are surprising under the prior (see the prior predictive tab). Reconsider the prior, or use a robust mixture prior."
  } else if (ps >= thr && ls >= thr) {
    level <- "warn"
    "Prior and data both shape the posterior: re-weighting either one moves it. That is expected with an informative prior and is not by itself a conflict (the prior predictive check is fine). Report how the result changes under a weaker prior, as shown in the left panel."
  } else if (ps >= thr && ls < thr) {
    level <- "warn"
    "Prior-dominated (weak likelihood): the data add little beyond the prior. Results lean on the prior, so justify it carefully or collect more data."
  } else if (ps < thr && ls >= thr) {
    "Data-driven: the posterior responds to the likelihood and is robust to modest changes in prior strength."
  } else {
    "Low sensitivity to both: check the model if this is unexpected."
  }
  list(prior = ps, likelihood = ls, diagnosis = diag, level = level)
}

# Approximate prior effective sample size (precision-based):
#   ESS ~ n_eff * (Var_ref / Var_post - 1)
# Exact for the conjugate Normal model (= sigma^2 / tau^2); approximate otherwise.
pl_prior_ess <- function(an) {
  if (is.null(an$ref)) return(NA_real_)
  lik <- an$lik
  n_eff <- switch(lik$type, binomial = lik$n, poisson = lik$E, normal = lik$n)
  f <- lik$to_nat
  v_post <- pl_grid_summary(an$post$x, an$post$w, f = f)$SD^2
  v_ref <- pl_grid_summary(an$ref$x, an$ref$w, f = f)$SD^2
  n_eff * (v_ref / v_post - 1)
}

# ---- 4a. Prior predictive for single-parameter models ----------------------------------

pl_prior_predictive <- function(mix, lik, ndraw = 20000) {
  eta <- pl_mix_r(mix, ndraw)
  nat <- lik$to_nat(eta)
  ysim <- switch(lik$type,
    binomial = {
      th <- pmin(pmax(nat, 0), 1)
      rbinom(ndraw, lik$n, th)
    },
    poisson = {
      lam <- lik$E * nat
      lam[!is.finite(lam) | lam > 1e12] <- 1e12
      suppressWarnings(rpois(ndraw, lam))
    },
    normal = rnorm(ndraw, nat, lik$sigma / sqrt(max(lik$n, 1)))
  )
  ysim <- as.numeric(ysim)
  ysim[is.na(ysim)] <- Inf
  obs <- lik$obs
  p_lo <- mean(ysim <= obs); p_hi <- mean(ysim >= obs)
  list(ysim = ysim, obs = obs, p_lower = p_lo, p_upper = p_hi,
       p_two = min(1, 2 * min(p_lo, p_hi)))
}

# ---- 4b. Prior predictive for regression models -------------------------------------------

#' Simulate from the prior predictive of a GLM with K standardised predictors.
#' family: "gaussian" (identity), "bernoulli" (logit), "poisson" (log)
pl_sim_regression <- function(family, K, prior_int, prior_b, prior_sigma = NULL,
                              ndraw = 4000, n_obs = 50, n_lines = 100) {
  b0 <- pl_r(prior_int, ndraw)
  B <- if (K > 0) matrix(pl_r(prior_b, ndraw * K), ndraw, K) else matrix(0, ndraw, 0)
  sig <- if (family == "gaussian") pl_r(prior_sigma, ndraw) else NULL
  inv <- switch(family, gaussian = identity, bernoulli = plogis, poisson = exp)

  # one random covariate profile per draw -> implied mean for a "random person"
  Xr <- matrix(rnorm(ndraw * K), ndraw, K)
  eta_r <- b0 + if (K > 0) rowSums(Xr * B) else 0
  mu_r <- inv(eta_r)

  # one simulated outcome per draw (prior predictive distribution of y)
  y_r <- switch(family,
    gaussian = rnorm(ndraw, mu_r, sig),
    bernoulli = rbinom(ndraw, 1, mu_r),
    poisson = {
      lam <- mu_r
      lam[!is.finite(lam) | lam > 1e12] <- 1e12
      suppressWarnings(as.numeric(rpois(ndraw, lam)))
    }
  )

  # whole simulated datasets of size n_obs -> dataset-level summaries
  idx <- rep(seq_len(ndraw), each = n_obs)
  Xd <- matrix(rnorm(ndraw * n_obs * K), ndraw * n_obs, K)
  eta_d <- b0[idx] + if (K > 0) rowSums(Xd * B[idx, , drop = FALSE]) else 0
  mu_d <- inv(eta_d)
  ds <- switch(family,
    gaussian = {
      yd <- rnorm(length(mu_d), mu_d, sig[idx])
      list(stat = tapply(yd, idx, mean), label = sprintf("Mean of y in a simulated dataset (n = %d)", n_obs))
    },
    bernoulli = {
      yd <- rbinom(length(mu_d), 1, mu_d)
      list(stat = tapply(yd, idx, mean), label = sprintf("Share of y = 1 in a simulated dataset (n = %d)", n_obs))
    },
    poisson = {
      lam <- mu_d
      lam[!is.finite(lam) | lam > 1e12] <- 1e12
      yd <- suppressWarnings(as.numeric(rpois(length(lam), lam)))
      yd[is.na(yd)] <- 1e12
      list(stat = tapply(yd, idx, mean), label = sprintf("Mean count in a simulated dataset (n = %d)", n_obs))
    }
  )

  # curves of the mean against x1 (other predictors held at 0)
  lines <- NULL
  if (K > 0) {
    xs <- seq(-2.5, 2.5, length.out = 41)
    nl <- min(n_lines, ndraw)
    lines <- data.frame(
      draw = rep(seq_len(nl), each = length(xs)),
      x = rep(xs, nl),
      mu = inv(rep(b0[seq_len(nl)], each = length(xs)) + rep(B[seq_len(nl), 1], each = length(xs)) * rep(xs, nl))
    )
  }

  list(family = family, K = K, b0 = b0, B = B, sigma = sig, eta = eta_r, mu = mu_r, y = y_r,
       dataset_stat = as.numeric(ds$stat), dataset_label = ds$label, lines = lines,
       eta_sd = sd(eta_r))
}

# ---- 5. Hierarchical normal model ----------------------------------------------------------

# Density of tau implied by a prior placed either on tau or on tau^2
pl_tau_density <- function(prior, tau, on_variance = FALSE) {
  if (!on_variance) return(pl_d(prior, tau))
  out <- pl_d(prior, tau^2) * 2 * tau
  out[!is.finite(out)] <- 0
  out
}

pl_tau_draws <- function(prior, n, on_variance = FALSE) {
  v <- pl_r(prior, n)
  if (on_variance) sqrt(pmax(v, 0)) else v
}

pl_tau_quantile <- function(prior, u, on_variance = FALSE) {
  q <- pl_q(prior, u)
  if (on_variance) sqrt(pmax(q, 0)) else q
}

# log marginal likelihood p(y | tau), integrating mu (flat or Normal(m0, s0)) and theta
pl_hier_loglik <- function(tau, y, s, mu_prior = NULL) {
  vapply(tau, function(t) {
    v <- s^2 + t^2
    if (is.null(mu_prior)) {
      P <- sum(1 / v); V <- 1 / P; M <- V * sum(y / v)
      -0.5 * sum(log(v)) + 0.5 * log(V) - 0.5 * sum((y - M)^2 / v)
    } else {
      m0 <- mu_prior$m0; s0 <- mu_prior$s0
      P <- 1 / s0^2 + sum(1 / v); V <- 1 / P
      M <- V * (m0 / s0^2 + sum(y / v))
      -0.5 * sum(log(v)) + 0.5 * log(V) - 0.5 * (sum(y^2 / v) + m0^2 / s0^2 - M^2 / V)
    }
  }, numeric(1))
}

# Conditional moments of mu and theta_j given tau
pl_hier_conditional <- function(t, y, s, mu_prior = NULL) {
  v <- s^2 + t^2
  if (is.null(mu_prior)) {
    V <- 1 / sum(1 / v); M <- V * sum(y / v)
  } else {
    V <- 1 / (1 / mu_prior$s0^2 + sum(1 / v))
    M <- V * (mu_prior$m0 / mu_prior$s0^2 + sum(y / v))
  }
  B <- s^2 / v                       # shrinkage factor towards mu
  list(mu_mean = M, mu_var = V,
       theta_mean = (1 - B) * y + B * M,
       theta_var = (1 - B) * s^2 + B^2 * V,
       B = B)
}

#' Exact posterior for y_j ~ N(theta_j, s_j), theta_j ~ N(mu, tau), any prior on tau.
pl_hier_analysis <- function(y, s, tau_prior, on_variance = FALSE, mu_prior = NULL,
                             n_grid = 3000, ndraw = 10000, level = 0.95) {
  J <- length(y)
  scale_y <- (if (J > 1) sd(y) else 0) + max(s)
  q_hi <- tryCatch(pl_tau_quantile(tau_prior, 0.999, on_variance), error = function(e) Inf)
  if (!is.finite(q_hi)) q_hi <- 50 * scale_y
  upper <- max(10 * scale_y, min(q_hi, 50 * scale_y))
  if (pl_dist(tau_prior$family)$support == "bounded") {
    b <- tau_prior$params[["b"]]
    upper <- if (on_variance) sqrt(b) else b
  }
  tau <- c(0, exp(seq(log(upper * 1e-6), log(upper), length.out = n_grid)))
  # trapezoid weights for the non-uniform grid
  dt <- diff(tau)
  tw <- c(dt[1] / 2, (dt[-1] + dt[-length(dt)]) / 2, dt[length(dt)] / 2)

  lp <- log(pl_tau_density(tau_prior, tau, on_variance))
  ll <- pl_hier_loglik(tau, y, s, mu_prior)
  lpost <- lp + ll
  lpost[is.nan(lpost)] <- -Inf
  if (!any(is.finite(lpost))) stop("The posterior for tau could not be computed with this prior.")
  dens <- exp(lpost - max(lpost))
  dens <- dens / sum(dens * tw)
  w <- dens * tw
  w <- w / sum(w)

  prior_dens <- exp(lp)
  prior_dens[!is.finite(prior_dens)] <- NA

  cond <- lapply(tau, pl_hier_conditional, y = y, s = s, mu_prior = mu_prior)
  th_mean <- t(vapply(cond, `[[`, numeric(J), "theta_mean"))
  th_var <- t(vapply(cond, `[[`, numeric(J), "theta_var"))
  if (J == 1) { th_mean <- matrix(th_mean, ncol = 1); th_var <- matrix(th_var, ncol = 1) }
  mu_mean <- vapply(cond, `[[`, 0, "mu_mean")
  mu_var <- vapply(cond, `[[`, 0, "mu_var")

  # exact posterior means; intervals by Monte Carlo over the grid
  post_theta_mean <- colSums(w * th_mean)
  ti <- sample.int(length(tau), ndraw, replace = TRUE, prob = w)
  th_draws <- vapply(seq_len(J), function(j) rnorm(ndraw, th_mean[ti, j], sqrt(th_var[ti, j])), numeric(ndraw))
  if (J == 1) th_draws <- matrix(th_draws, ncol = 1)
  mu_draws <- rnorm(ndraw, mu_mean[ti], sqrt(mu_var[ti]))
  a <- (1 - level) / 2

  cw <- cumsum(w)
  tau_q <- approx(cw, tau, c(a, 0.5, 1 - a), rule = 2, ties = "ordered")$y

  list(
    tau = tau, tau_weights = w, tau_post_density = dens, tau_prior_density = prior_dens,
    tau_mean = sum(w * tau), tau_q = tau_q,
    theta = data.frame(
      j = seq_len(J), y = y, s = s,
      post_mean = post_theta_mean,
      post_sd = apply(th_draws, 2, sd),
      lower = apply(th_draws, 2, quantile, a),
      upper = apply(th_draws, 2, quantile, 1 - a)
    ),
    mu = c(mean = sum(w * mu_mean), quantile(mu_draws, c(a, 1 - a))),
    th_mean_by_tau = th_mean, mu_mean_by_tau = mu_mean,
    pooled = sum(y / s^2) / sum(1 / s^2), pooled_se = sqrt(1 / sum(1 / s^2))
  )
}

PL_EIGHT_SCHOOLS <- data.frame(
  group = LETTERS[1:8],
  estimate = c(28, 8, -3, 7, -1, 1, 18, 12),
  se = c(15, 10, 16, 11, 9, 11, 10, 18)
)

#' Implied spread of group means under a prior for the group-level SD tau.
#' link: "logit", "log" or "identity"; baseline given on the outcome scale.
pl_sim_group_spread <- function(tau_prior, on_variance = FALSE, link = "logit", baseline = 0.5,
                                J = 20, ndraw = 4000, n_strip = 30) {
  tau <- pl_tau_draws(tau_prior, ndraw, on_variance)
  b <- switch(link, logit = qlogis(baseline), log = log(baseline), identity = baseline)
  inv <- switch(link, logit = plogis, log = exp, identity = identity)
  z90 <- qnorm(0.9)
  ratio <- switch(link,
    logit = exp(2 * z90 * tau), log = exp(2 * z90 * tau), identity = 2 * z90 * tau)
  ratio_label <- switch(link,
    logit = "Odds ratio: 90th vs 10th percentile group",
    log = "Rate ratio: 90th vs 10th percentile group",
    identity = "Difference: 90th vs 10th percentile group")
  diff_nat <- inv(b + z90 * tau) - inv(b - z90 * tau)
  ns <- min(n_strip, ndraw)
  strip <- data.frame(
    draw = rep(seq_len(ns), each = J),
    tau = rep(tau[seq_len(ns)], each = J),
    value = inv(b + rnorm(ns * J) * rep(tau[seq_len(ns)], each = J))
  )
  group_means <- inv(b + rnorm(ndraw) * tau)
  list(tau = tau, ratio = ratio, ratio_label = ratio_label, diff_nat = diff_nat,
       strip = strip, group_means = group_means, link = link, baseline = baseline)
}

# ---- 6. LKJ ------------------------------------------------------------------------------

#' Marginal density of one correlation under LKJ(eta) for a K x K matrix:
#' (r + 1) / 2 ~ Beta(eta - 1 + K/2, eta - 1 + K/2)
pl_lkj_marginal <- function(r, eta, K) {
  a <- eta - 1 + K / 2
  dbeta((r + 1) / 2, a, a) / 2
}

pl_lkj_prob_abs_gt <- function(cut, eta, K) {
  a <- eta - 1 + K / 2
  2 * pbeta((1 - cut) / 2, a, a)
}
