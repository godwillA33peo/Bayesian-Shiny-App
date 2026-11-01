# ============================================================================
# Prior Lab: distribution registry
# ============================================================================
# One place that knows, for every prior family the Prior Lab offers:
#   - its parameters (names, labels, defaults, constraints)
#   - its support ("real", "positive", "unit", or "bounded" for Uniform)
#   - density / CDF / quantile / random-draw functions
#   - analytic mean and SD where they exist
#   - how to write it in Stan / brms syntax
#
# Every function takes a *named* numeric parameter vector `p`,
# e.g. p = c(mu = 0, sigma = 1).
# Only base R (stats) is used, so nothing new needs installing on shinyapps.io.
# ============================================================================

# ---- small helpers ----------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

# Format numbers for code snippets and labels: 4 significant digits, no
# trailing zeros, no scientific notation for everyday magnitudes.
pl_fmt <- function(x, digits = 4) {
  vapply(x, function(v) {
    if (is.na(v)) return("NA")
    if (is.infinite(v)) return(if (v > 0) "Inf" else "-Inf")
    if (abs(v) < 1e-10) v <- 0   # grid round-off, e.g. 4.8e-17 for a mean of 0
    v <- signif(v, digits)
    sci <- v != 0 && (abs(v) >= 1e6 || abs(v) < 1e-4)
    format(v, scientific = sci, trim = TRUE, drop0trailing = TRUE)
  }, character(1))
}

pl_par <- function(name, label, default, min = -Inf, step = 0.1, integer = FALSE) {
  list(name = name, label = label, default = default, min = min,
       step = step, integer = integer)
}

# Expected value of |T| for a Student-t with nu > 1 degrees of freedom
pl_half_t_mean_factor <- function(nu) {
  2 * sqrt(nu) * exp(lgamma((nu + 1) / 2) - lgamma(nu / 2)) / ((nu - 1) * sqrt(pi))
}

# ---- the registry -------------------------------------------------------------

PL_DISTS <- list(

  normal = list(
    label = "Normal(\u{03bc}, \u{03c3})", short = "Normal", support = "real",
    pars = list(pl_par("mu", "Mean \u{03bc}", 0), pl_par("sigma", "SD \u{03c3}", 1, min = 0)),
    d = function(x, p) dnorm(x, p[["mu"]], p[["sigma"]]),
    p = function(x, p) pnorm(x, p[["mu"]], p[["sigma"]]),
    q = function(u, p) qnorm(u, p[["mu"]], p[["sigma"]]),
    r = function(n, p) rnorm(n, p[["mu"]], p[["sigma"]]),
    mean = function(p) p[["mu"]], sd = function(p) p[["sigma"]],
    stan = function(p) sprintf("normal(%s, %s)", pl_fmt(p[["mu"]]), pl_fmt(p[["sigma"]])),
    rcode = function(p) sprintf("rnorm(n, %s, %s)", pl_fmt(p[["mu"]]), pl_fmt(p[["sigma"]]))
  ),

  student_t = list(
    label = "Student-t(\u{03bd}, \u{03bc}, \u{03c3})", short = "Student-t", support = "real",
    pars = list(pl_par("nu", "df \u{03bd}", 3, min = 0, step = 1),
                pl_par("mu", "Location \u{03bc}", 0),
                pl_par("sigma", "Scale \u{03c3}", 2.5, min = 0)),
    d = function(x, p) dt((x - p[["mu"]]) / p[["sigma"]], p[["nu"]]) / p[["sigma"]],
    p = function(x, p) pt((x - p[["mu"]]) / p[["sigma"]], p[["nu"]]),
    q = function(u, p) p[["mu"]] + p[["sigma"]] * qt(u, p[["nu"]]),
    r = function(n, p) p[["mu"]] + p[["sigma"]] * rt(n, p[["nu"]]),
    mean = function(p) if (p[["nu"]] > 1) p[["mu"]] else NA_real_,
    sd = function(p) {
      if (p[["nu"]] > 2) p[["sigma"]] * sqrt(p[["nu"]] / (p[["nu"]] - 2))
      else if (p[["nu"]] > 1) Inf else NA_real_
    },
    stan = function(p) sprintf("student_t(%s, %s, %s)", pl_fmt(p[["nu"]]), pl_fmt(p[["mu"]]), pl_fmt(p[["sigma"]])),
    rcode = function(p) sprintf("%s + %s * rt(n, %s)", pl_fmt(p[["mu"]]), pl_fmt(p[["sigma"]]), pl_fmt(p[["nu"]]))
  ),

  cauchy = list(
    label = "Cauchy(\u{03bc}, \u{03c3})", short = "Cauchy", support = "real",
    pars = list(pl_par("mu", "Location \u{03bc}", 0), pl_par("sigma", "Scale \u{03c3}", 2.5, min = 0)),
    d = function(x, p) dcauchy(x, p[["mu"]], p[["sigma"]]),
    p = function(x, p) pcauchy(x, p[["mu"]], p[["sigma"]]),
    q = function(u, p) qcauchy(u, p[["mu"]], p[["sigma"]]),
    r = function(n, p) rcauchy(n, p[["mu"]], p[["sigma"]]),
    mean = function(p) NA_real_, sd = function(p) NA_real_,
    stan = function(p) sprintf("cauchy(%s, %s)", pl_fmt(p[["mu"]]), pl_fmt(p[["sigma"]])),
    rcode = function(p) sprintf("rcauchy(n, %s, %s)", pl_fmt(p[["mu"]]), pl_fmt(p[["sigma"]]))
  ),

  logistic = list(
    label = "Logistic(\u{03bc}, s)", short = "Logistic", support = "real",
    pars = list(pl_par("mu", "Location \u{03bc}", 0), pl_par("s", "Scale s", 1, min = 0)),
    d = function(x, p) dlogis(x, p[["mu"]], p[["s"]]),
    p = function(x, p) plogis(x, p[["mu"]], p[["s"]]),
    q = function(u, p) qlogis(u, p[["mu"]], p[["s"]]),
    r = function(n, p) rlogis(n, p[["mu"]], p[["s"]]),
    mean = function(p) p[["mu"]], sd = function(p) p[["s"]] * pi / sqrt(3),
    stan = function(p) sprintf("logistic(%s, %s)", pl_fmt(p[["mu"]]), pl_fmt(p[["s"]])),
    rcode = function(p) sprintf("rlogis(n, %s, %s)", pl_fmt(p[["mu"]]), pl_fmt(p[["s"]]))
  ),

  laplace = list(
    label = "Laplace(\u{03bc}, b)", short = "Laplace", support = "real",
    pars = list(pl_par("mu", "Location \u{03bc}", 0), pl_par("b", "Scale b", 1, min = 0)),
    d = function(x, p) exp(-abs(x - p[["mu"]]) / p[["b"]]) / (2 * p[["b"]]),
    p = function(x, p) {
      z <- (x - p[["mu"]]) / p[["b"]]
      ifelse(z < 0, 0.5 * exp(pmin(z, 0)), 1 - 0.5 * exp(-pmax(z, 0)))
    },
    q = function(u, p) {
      ifelse(u < 0.5, p[["mu"]] + p[["b"]] * log(2 * u), p[["mu"]] - p[["b"]] * log(2 - 2 * u))
    },
    r = function(n, p) p[["mu"]] + p[["b"]] * (rexp(n) - rexp(n)),
    mean = function(p) p[["mu"]], sd = function(p) sqrt(2) * p[["b"]],
    stan = function(p) sprintf("double_exponential(%s, %s)", pl_fmt(p[["mu"]]), pl_fmt(p[["b"]])),
    rcode = function(p) sprintf("%s + %s * (rexp(n) - rexp(n))", pl_fmt(p[["mu"]]), pl_fmt(p[["b"]]))
  ),

  uniform = list(
    label = "Uniform(a, b)", short = "Uniform", support = "bounded",
    pars = list(pl_par("a", "Lower a", 0), pl_par("b", "Upper b", 1)),
    d = function(x, p) dunif(x, p[["a"]], p[["b"]]),
    p = function(x, p) punif(x, p[["a"]], p[["b"]]),
    q = function(u, p) qunif(u, p[["a"]], p[["b"]]),
    r = function(n, p) runif(n, p[["a"]], p[["b"]]),
    mean = function(p) (p[["a"]] + p[["b"]]) / 2,
    sd = function(p) (p[["b"]] - p[["a"]]) / sqrt(12),
    stan = function(p) sprintf("uniform(%s, %s)", pl_fmt(p[["a"]]), pl_fmt(p[["b"]])),
    rcode = function(p) sprintf("runif(n, %s, %s)", pl_fmt(p[["a"]]), pl_fmt(p[["b"]])),
    check = function(p) if (p[["b"]] <= p[["a"]]) "Uniform: upper bound b must be greater than lower bound a." else NULL
  ),

  half_normal = list(
    label = "Half-Normal(\u{03c3})", short = "Half-Normal", support = "positive",
    pars = list(pl_par("sigma", "Scale \u{03c3}", 1, min = 0)),
    d = function(x, p) ifelse(x < 0, 0, 2 * dnorm(x, 0, p[["sigma"]])),
    p = function(x, p) ifelse(x < 0, 0, 2 * pnorm(x, 0, p[["sigma"]]) - 1),
    q = function(u, p) p[["sigma"]] * qnorm((1 + u) / 2),
    r = function(n, p) abs(rnorm(n, 0, p[["sigma"]])),
    mean = function(p) p[["sigma"]] * sqrt(2 / pi),
    sd = function(p) p[["sigma"]] * sqrt(1 - 2 / pi),
    stan = function(p) sprintf("normal(0, %s)", pl_fmt(p[["sigma"]])),
    rcode = function(p) sprintf("abs(rnorm(n, 0, %s))", pl_fmt(p[["sigma"]]))
  ),

  half_t = list(
    label = "Half-Student-t(\u{03bd}, \u{03c3})", short = "Half-t", support = "positive",
    pars = list(pl_par("nu", "df \u{03bd}", 3, min = 0, step = 1), pl_par("sigma", "Scale \u{03c3}", 1, min = 0)),
    d = function(x, p) ifelse(x < 0, 0, 2 * dt(x / p[["sigma"]], p[["nu"]]) / p[["sigma"]]),
    p = function(x, p) ifelse(x < 0, 0, 2 * pt(x / p[["sigma"]], p[["nu"]]) - 1),
    q = function(u, p) p[["sigma"]] * qt((1 + u) / 2, p[["nu"]]),
    r = function(n, p) abs(p[["sigma"]] * rt(n, p[["nu"]])),
    mean = function(p) if (p[["nu"]] > 1) p[["sigma"]] * pl_half_t_mean_factor(p[["nu"]]) else NA_real_,
    sd = function(p) {
      nu <- p[["nu"]]
      if (nu > 2) {
        m <- pl_half_t_mean_factor(nu)
        p[["sigma"]] * sqrt(nu / (nu - 2) - m^2)
      } else NA_real_
    },
    stan = function(p) sprintf("student_t(%s, 0, %s)", pl_fmt(p[["nu"]]), pl_fmt(p[["sigma"]])),
    rcode = function(p) sprintf("abs(%s * rt(n, %s))", pl_fmt(p[["sigma"]]), pl_fmt(p[["nu"]]))
  ),

  half_cauchy = list(
    label = "Half-Cauchy(\u{03c3})", short = "Half-Cauchy", support = "positive",
    pars = list(pl_par("sigma", "Scale \u{03c3}", 1, min = 0)),
    d = function(x, p) ifelse(x < 0, 0, 2 * dcauchy(x, 0, p[["sigma"]])),
    p = function(x, p) ifelse(x < 0, 0, 2 / pi * atan(x / p[["sigma"]])),
    q = function(u, p) p[["sigma"]] * tan(pi * u / 2),
    r = function(n, p) abs(rcauchy(n, 0, p[["sigma"]])),
    mean = function(p) NA_real_, sd = function(p) NA_real_,
    stan = function(p) sprintf("cauchy(0, %s)", pl_fmt(p[["sigma"]])),
    rcode = function(p) sprintf("abs(rcauchy(n, 0, %s))", pl_fmt(p[["sigma"]]))
  ),

  exponential = list(
    label = "Exponential(\u{03bb})", short = "Exponential", support = "positive",
    pars = list(pl_par("rate", "Rate \u{03bb}", 1, min = 0)),
    d = function(x, p) dexp(x, p[["rate"]]),
    p = function(x, p) pexp(x, p[["rate"]]),
    q = function(u, p) qexp(u, p[["rate"]]),
    r = function(n, p) rexp(n, p[["rate"]]),
    mean = function(p) 1 / p[["rate"]], sd = function(p) 1 / p[["rate"]],
    stan = function(p) sprintf("exponential(%s)", pl_fmt(p[["rate"]])),
    rcode = function(p) sprintf("rexp(n, %s)", pl_fmt(p[["rate"]]))
  ),

  gamma = list(
    label = "Gamma(shape, rate)", short = "Gamma", support = "positive",
    pars = list(pl_par("shape", "Shape \u{03b1}", 2, min = 0), pl_par("rate", "Rate \u{03b2}", 1, min = 0)),
    d = function(x, p) dgamma(x, shape = p[["shape"]], rate = p[["rate"]]),
    p = function(x, p) pgamma(x, shape = p[["shape"]], rate = p[["rate"]]),
    q = function(u, p) qgamma(u, shape = p[["shape"]], rate = p[["rate"]]),
    r = function(n, p) rgamma(n, shape = p[["shape"]], rate = p[["rate"]]),
    mean = function(p) p[["shape"]] / p[["rate"]], sd = function(p) sqrt(p[["shape"]]) / p[["rate"]],
    stan = function(p) sprintf("gamma(%s, %s)", pl_fmt(p[["shape"]]), pl_fmt(p[["rate"]])),
    rcode = function(p) sprintf("rgamma(n, shape = %s, rate = %s)", pl_fmt(p[["shape"]]), pl_fmt(p[["rate"]]))
  ),

  inv_gamma = list(
    label = "Inverse-Gamma(shape, scale)", short = "Inv-Gamma", support = "positive",
    pars = list(pl_par("shape", "Shape \u{03b1}", 3, min = 0), pl_par("scale", "Scale \u{03b2}", 2, min = 0)),
    d = function(x, p) {
      out <- numeric(length(x))
      ok <- x > 0
      out[ok] <- dgamma(1 / x[ok], shape = p[["shape"]], rate = p[["scale"]]) / x[ok]^2
      out
    },
    p = function(x, p) {
      out <- numeric(length(x))
      ok <- x > 0
      out[ok] <- pgamma(1 / x[ok], shape = p[["shape"]], rate = p[["scale"]], lower.tail = FALSE)
      out
    },
    q = function(u, p) 1 / qgamma(1 - u, shape = p[["shape"]], rate = p[["scale"]]),
    r = function(n, p) 1 / rgamma(n, shape = p[["shape"]], rate = p[["scale"]]),
    mean = function(p) if (p[["shape"]] > 1) p[["scale"]] / (p[["shape"]] - 1) else NA_real_,
    sd = function(p) {
      a <- p[["shape"]]
      if (a > 2) p[["scale"]] / ((a - 1) * sqrt(a - 2)) else NA_real_
    },
    stan = function(p) sprintf("inv_gamma(%s, %s)", pl_fmt(p[["shape"]]), pl_fmt(p[["scale"]])),
    rcode = function(p) sprintf("1 / rgamma(n, shape = %s, rate = %s)", pl_fmt(p[["shape"]]), pl_fmt(p[["scale"]]))
  ),

  lognormal = list(
    label = "Log-Normal(\u{03bc}, \u{03c3})", short = "Log-Normal", support = "positive",
    pars = list(pl_par("mu", "Log-mean \u{03bc}", 0), pl_par("sigma", "Log-SD \u{03c3}", 0.5, min = 0)),
    d = function(x, p) dlnorm(x, p[["mu"]], p[["sigma"]]),
    p = function(x, p) plnorm(x, p[["mu"]], p[["sigma"]]),
    q = function(u, p) qlnorm(u, p[["mu"]], p[["sigma"]]),
    r = function(n, p) rlnorm(n, p[["mu"]], p[["sigma"]]),
    mean = function(p) exp(p[["mu"]] + p[["sigma"]]^2 / 2),
    sd = function(p) sqrt((exp(p[["sigma"]]^2) - 1) * exp(2 * p[["mu"]] + p[["sigma"]]^2)),
    stan = function(p) sprintf("lognormal(%s, %s)", pl_fmt(p[["mu"]]), pl_fmt(p[["sigma"]])),
    rcode = function(p) sprintf("rlnorm(n, %s, %s)", pl_fmt(p[["mu"]]), pl_fmt(p[["sigma"]]))
  ),

  beta = list(
    label = "Beta(a, b)", short = "Beta", support = "unit",
    pars = list(pl_par("a", "Shape a", 2, min = 0), pl_par("b", "Shape b", 2, min = 0)),
    d = function(x, p) dbeta(x, p[["a"]], p[["b"]]),
    p = function(x, p) pbeta(x, p[["a"]], p[["b"]]),
    q = function(u, p) qbeta(u, p[["a"]], p[["b"]]),
    r = function(n, p) rbeta(n, p[["a"]], p[["b"]]),
    mean = function(p) p[["a"]] / (p[["a"]] + p[["b"]]),
    sd = function(p) {
      a <- p[["a"]]; b <- p[["b"]]
      sqrt(a * b / ((a + b)^2 * (a + b + 1)))
    },
    stan = function(p) sprintf("beta(%s, %s)", pl_fmt(p[["a"]]), pl_fmt(p[["b"]])),
    rcode = function(p) sprintf("rbeta(n, %s, %s)", pl_fmt(p[["a"]]), pl_fmt(p[["b"]]))
  )
)

# Supports each scale accepts. "bounded" (Uniform) is checked separately
# against the scale's limits in pl_check_prior().
PL_SUPPORT_LIMITS <- list(
  real     = c(-Inf, Inf),
  positive = c(0, Inf),
  unit     = c(0, 1)
)

PL_SUPPORT_LABELS <- c(
  real = "any real number", positive = "positive values", unit = "values between 0 and 1"
)

# ---- accessors -------------------------------------------------------------------

pl_dist <- function(family) {
  d <- PL_DISTS[[family]]
  if (is.null(d)) stop("Unknown prior family: ", family)
  d
}

pl_par_names <- function(family) vapply(pl_dist(family)$pars, `[[`, "", "name")

pl_default_params <- function(family) {
  d <- pl_dist(family)
  setNames(vapply(d$pars, `[[`, 0, "default"), pl_par_names(family))
}

# Families whose support fits one of the requested supports.
# Uniform ("bounded") is offered for every support.
pl_families_for <- function(supports = NULL) {
  keys <- names(PL_DISTS)
  if (is.null(supports)) return(keys)
  keep <- vapply(PL_DISTS, function(d) d$support %in% c(supports, "bounded"), logical(1))
  keys[keep]
}

pl_family_choices <- function(supports = NULL) {
  keys <- pl_families_for(supports)
  setNames(keys, vapply(PL_DISTS[keys], `[[`, "", "label"))
}

# Make a prior object: list(family, params)
pl_prior <- function(family, ...) {
  vals <- c(...)
  if (is.list(vals)) vals <- unlist(vals)
  nm <- pl_par_names(family)
  if (is.null(names(vals))) names(vals) <- nm
  list(family = family, params = vals[nm])
}

# Validate a prior; returns NULL if fine, otherwise a message.
# `support` (optional) is the support of the parameter it is placed on.
pl_check_prior <- function(prior, support = NULL) {
  d <- PL_DISTS[[prior$family]]
  if (is.null(d)) return(paste("Unknown family", prior$family))
  p <- prior$params
  nm <- pl_par_names(prior$family)
  if (length(p) != length(nm) || any(is.na(p[nm])) || any(!is.finite(p[nm]))) {
    return(paste0(d$short, ": please enter a value for every parameter."))
  }
  for (par in d$pars) {
    if (is.finite(par$min) && p[[par$name]] <= par$min) {
      return(sprintf("%s: %s must be greater than %s.", d$short, par$label, pl_fmt(par$min)))
    }
  }
  if (!is.null(d$check)) {
    msg <- d$check(p)
    if (!is.null(msg)) return(msg)
  }
  if (!is.null(support)) {
    if (d$support == "bounded") {
      lim <- PL_SUPPORT_LIMITS[[support]]
      if (p[["a"]] < lim[1] || p[["b"]] > lim[2]) {
        return(sprintf("Uniform bounds must lie within the parameter's range (%s).",
                       PL_SUPPORT_LABELS[[support]]))
      }
    } else if (!(d$support %in% support)) {
      return(sprintf("%s is defined on %s, but this parameter needs %s.",
                     d$short, PL_SUPPORT_LABELS[[d$support]], PL_SUPPORT_LABELS[[support[1]]]))
    }
  }
  NULL
}

# Support limits of a prior (as numeric c(lower, upper))
pl_support_limits <- function(prior) {
  d <- pl_dist(prior$family)
  if (d$support == "bounded") return(unname(prior$params[c("a", "b")]))
  PL_SUPPORT_LIMITS[[d$support]]
}

pl_prior_label <- function(prior) {
  d <- pl_dist(prior$family)
  sprintf("%s(%s)", d$short, paste(pl_fmt(prior$params), collapse = ", "))
}

# Stan-style sampling statement body, e.g. "normal(0, 1)"
pl_stan_string <- function(prior) pl_dist(prior$family)$stan(prior$params)

# Evaluate density / CDF / quantile / draws
pl_d <- function(prior, x) pl_dist(prior$family)$d(x, prior$params)
pl_p <- function(prior, x) pl_dist(prior$family)$p(x, prior$params)
pl_q <- function(prior, u) pl_dist(prior$family)$q(u, prior$params)
pl_r <- function(prior, n) pl_dist(prior$family)$r(n, prior$params)

# Log density, safe at zero density
pl_logd <- function(prior, x) {
  dens <- pl_d(prior, x)
  out <- rep(-Inf, length(x))
  pos <- is.finite(dens) & dens > 0
  out[pos] <- log(dens[pos])
  out[is.infinite(dens) & dens > 0] <- 700 # integrable singularity at a boundary
  out
}

# ---- summaries ----------------------------------------------------------------------

# Summary row for one prior on its own scale
pl_summary <- function(prior, level = 0.95, threshold = NULL) {
  d <- pl_dist(prior$family)
  a <- (1 - level) / 2
  out <- data.frame(
    Mean = d$mean(prior$params),
    SD = d$sd(prior$params),
    Median = pl_q(prior, 0.5),
    Lower = pl_q(prior, a),
    Upper = pl_q(prior, 1 - a),
    check.names = FALSE
  )
  if (!is.null(threshold)) out$`P(> c)` <- 1 - pl_p(prior, threshold)
  out
}

# ---- plotting ranges -----------------------------------------------------------------

# Sensible x-range for showing a density: central 96% widened by 25% each side,
# clipped to the support. Heavy tails (Cauchy) stay visible without flattening
# everything else into a spike.
pl_auto_range <- function(prior, probs = c(0.02, 0.98), expand = 0.25) {
  d <- pl_dist(prior$family)
  lim <- pl_support_limits(prior)
  if (d$support == "bounded") {
    w <- lim[2] - lim[1]
    return(c(lim[1] - 0.1 * w, lim[2] + 0.1 * w))
  }
  qs <- pl_q(prior, probs)
  w <- diff(qs)
  if (!is.finite(w) || w <= 0) w <- 1
  rng <- c(qs[1] - expand * w, qs[2] + expand * w)
  if (d$support == "positive") rng[1] <- 0
  if (d$support == "unit") rng <- c(0, 1)
  rng
}

pl_union_range <- function(priors, ...) {
  rngs <- vapply(priors, function(pr) pl_auto_range(pr, ...), numeric(2))
  c(min(rngs[1, ]), max(rngs[2, ]))
}

# Density curve data on a grid. Points sitting exactly on a support boundary are
# nudged inside so Beta(0.5, 0.5)-style singularities do not produce Inf.
pl_density_df <- function(prior, range, n = 801) {
  x <- seq(range[1], range[2], length.out = n)
  lim <- pl_support_limits(prior)
  eps <- 1e-6 * diff(range)
  x[x == lim[1]] <- lim[1] + eps
  x[x == lim[2]] <- lim[2] - eps
  y <- pl_d(prior, x)
  y[!is.finite(y)] <- NA
  data.frame(x = x, density = y, cdf = pl_p(prior, x))
}

# y-axis cap that ignores spikes sitting right at a support boundary
pl_density_cap <- function(dens_list) {
  caps <- vapply(dens_list, function(y) {
    y <- y[is.finite(y)]
    if (length(y) < 10) return(max(c(y, 1), na.rm = TRUE))
    max(y[3:(length(y) - 2)], na.rm = TRUE)
  }, numeric(1))
  1.1 * max(caps[is.finite(caps)], 1e-8)
}

# ---- implied-scale transforms --------------------------------------------------------

PL_TRANSFORMS <- list(
  none = list(label = "No transform", f = identity, axis = "\u{03b8}", log_axis = FALSE),
  inv_logit = list(label = "Probability: inverse-logit(\u{03b8})  [logistic regression]",
                   f = plogis, axis = "Probability = logit\u{207b}\u{00b9}(\u{03b8})", log_axis = FALSE),
  probit = list(label = "Probability: \u{03a6}(\u{03b8})  [probit regression]",
                f = pnorm, axis = "Probability = \u{03a6}(\u{03b8})", log_axis = FALSE),
  exp = list(label = "Ratio: exp(\u{03b8})  [odds / rate / hazard ratio]",
             f = exp, axis = "exp(\u{03b8})  (log axis)", log_axis = TRUE),
  square = list(label = "Variance: \u{03b8}\u{00b2}  [if \u{03b8} is an SD]",
                f = function(x) x^2, axis = "\u{03b8}\u{00b2}", log_axis = FALSE),
  inv_sq = list(label = "Precision: 1/\u{03b8}\u{00b2}  [if \u{03b8} is an SD]",
                f = function(x) 1 / x^2, axis = "1/\u{03b8}\u{00b2}  (log axis)", log_axis = TRUE)
)

pl_transform_choices <- function() {
  setNames(names(PL_TRANSFORMS), vapply(PL_TRANSFORMS, `[[`, "", "label"))
}

# How much implied-scale mass sits in "silly" regions, per transform
pl_extreme_mass <- function(transform, z) {
  z <- z[is.finite(z)]
  switch(transform,
    inv_logit = , probit = c(label = "P(p < 0.01 or p > 0.99)", value = mean(z < 0.01 | z > 0.99)),
    exp = c(label = "P(ratio < 0.1 or > 10)", value = mean(z < 0.1 | z > 10)),
    NULL
  )
}

# ---- code generation ----------------------------------------------------------------

PL_BRMS_CLASSES <- c(
  "Intercept (class = \"Intercept\")" = "Intercept",
  "Regression coefficient (class = \"b\")" = "b",
  "Group-level SD (class = \"sd\")" = "sd",
  "Residual SD (class = \"sigma\")" = "sigma",
  "Generic parameter (Stan only)" = "none"
)

pl_brms_line <- function(prior, class) {
  if (identical(class, "none")) return(NA_character_)
  d <- pl_dist(prior$family)
  extra <- ""
  if (d$support == "bounded") {
    # brms needs explicit bounds for uniform priors; sd / sigma already have lb = 0
    extra <- if (class %in% c("sd", "sigma")) {
      sprintf(", ub = %s", pl_fmt(prior$params[["b"]]))
    } else {
      sprintf(", lb = %s, ub = %s", pl_fmt(prior$params[["a"]]), pl_fmt(prior$params[["b"]]))
    }
  }
  sprintf("prior(%s, class = \"%s\"%s)", pl_stan_string(prior), class, extra)
}

pl_stan_line <- function(prior, name = "theta") {
  sprintf("%s ~ %s;", name, pl_stan_string(prior))
}

pl_r_line <- function(prior, name = "theta") {
  sprintf("%s <- %s", name, pl_dist(prior$family)$rcode(prior$params))
}

# Serialise/deserialise priors for the downloadable library CSV
pl_params_to_string <- function(params) {
  paste(sprintf("%s=%s", names(params), format(params, digits = 15, trim = TRUE)), collapse = "; ")
}

pl_params_from_string <- function(s) {
  bits <- trimws(strsplit(s, ";", fixed = TRUE)[[1]])
  bits <- bits[nzchar(bits)]
  kv <- strsplit(bits, "=", fixed = TRUE)
  vals <- vapply(kv, function(x) as.numeric(trimws(x[2])), numeric(1))
  setNames(vals, vapply(kv, function(x) trimws(x[1]), character(1)))
}
