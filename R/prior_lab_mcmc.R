# ============================================================================
# Prior Lab: a small, dependable MCMC engine in base R
# ============================================================================
# Used by the Model Lab for models with several parameters.
#
#   1. Find the posterior mode with optim() and its Hessian (Laplace
#      approximation) -> a good starting proposal covariance.
#   2. Start 4 chains from over-dispersed points around the mode.
#   3. Adaptive random-walk Metropolis during warm-up (Haario et al. 2001
#      covariance adaptation + Robbins-Monro step-size tuning towards a 0.234
#      acceptance rate). While sampling, kernels are fixed: a 50/50 mixture of
#      that random walk and an independence proposal (multivariate t, 5 df,
#      fitted to the warm-up draws).
#   4. Diagnostics: rank-normalised split R-hat and bulk / tail ESS
#      (Vehtari, Gelman, Simpson, Carpenter & Buerkner, 2021).
#
# Parameters are sampled on an unconstrained scale; positive parameters via
# log, (0,1) parameters via logit, ordered vectors via first value plus
# log-increments. Jacobians are added so priors stay on the natural scale.
# ============================================================================

# ---- parameter blocks and transforms ----------------------------------------------

#' A parameter block: name, type ("real", "positive", "unit", "ordered"), size
pl_par_block <- function(name, type = "real", size = 1, label = name, desc = "") {
  list(name = name, type = type, size = size, label = label, desc = desc)
}

pl_block_index <- function(blocks) {
  ends <- cumsum(vapply(blocks, `[[`, 0, "size"))
  starts <- ends - vapply(blocks, `[[`, 0, "size") + 1
  setNames(lapply(seq_along(blocks), function(i) starts[i]:ends[i]), vapply(blocks, `[[`, "", "name"))
}

#' Unconstrained vector -> list of constrained parameters plus log-Jacobian
pl_constrain <- function(z, blocks, idx = pl_block_index(blocks)) {
  out <- list(); lj <- 0
  for (b in blocks) {
    zz <- z[idx[[b$name]]]
    if (b$type == "real") {
      out[[b$name]] <- zz
    } else if (b$type == "positive") {
      out[[b$name]] <- exp(zz); lj <- lj + sum(zz)
    } else if (b$type == "unit") {
      out[[b$name]] <- plogis(zz)
      lj <- lj + sum(plogis(zz, log.p = TRUE) + plogis(-zz, log.p = TRUE))
    } else if (b$type == "ordered") {
      out[[b$name]] <- cumsum(c(zz[1], exp(zz[-1])))
      lj <- lj + sum(zz[-1])
    }
  }
  attr(out, "logJ") <- lj
  out
}

#' Constrained list -> unconstrained vector
pl_unconstrain <- function(par, blocks) {
  unlist(lapply(blocks, function(b) {
    v <- par[[b$name]]
    switch(b$type,
           real = v,
           positive = log(v),
           unit = qlogis(v),
           ordered = c(v[1], log(diff(v))))
  }), use.names = FALSE)
}

pl_flat_names <- function(blocks) {
  unlist(lapply(blocks, function(b) {
    if (b$size == 1) b$name else paste0(b$name, "[", seq_len(b$size), "]")
  }))
}

# ---- sampler ---------------------------------------------------------------------------

#' Adaptive Metropolis with Laplace-approximation start.
#' @param logpost function(z) on the unconstrained scale (returns -Inf if invalid)
#' @param z_init  a starting point (unconstrained)
#' @return list(draws = array [iter, chain, param] on the unconstrained scale,
#'              accept = per-chain acceptance, mode, cov)
pl_mcmc <- function(logpost, z_init, n_warmup = 1000, n_iter = 1000, n_chains = 4) {
  d <- length(z_init)
  safe_lp <- function(z) {
    v <- tryCatch(logpost(z), error = function(e) -Inf)
    if (!is.finite(v)) -Inf else v
  }

  # 1. posterior mode and Laplace covariance
  nlp <- function(z) { v <- safe_lp(z); if (is.finite(v)) -v else 1e100 }
  opt <- tryCatch(optim(z_init, nlp, method = "BFGS", hessian = TRUE,
                        control = list(maxit = 500, reltol = 1e-10)),
                  error = function(e) NULL)
  mode <- if (!is.null(opt) && is.finite(opt$value) && opt$value < 1e99) opt$par else z_init
  cov0 <- NULL
  if (!is.null(opt) && !is.null(opt$hessian) && all(is.finite(opt$hessian))) {
    H <- (opt$hessian + t(opt$hessian)) / 2
    ev <- tryCatch(eigen(H, symmetric = TRUE), error = function(e) NULL)
    if (!is.null(ev) && all(ev$values > 1e-10)) cov0 <- solve(H)
  }
  if (is.null(cov0)) cov0 <- diag(0.1, d)

  draws <- array(NA_real_, dim = c(n_iter, n_chains, d))
  accept <- numeric(n_chains)
  L0 <- tryCatch(t(chol(cov0)), error = function(e) diag(sqrt(pmax(diag(cov0), 1e-8)), d))

  for (ch in seq_len(n_chains)) {
    # 2. over-dispersed start: mode + 2 x Laplace SD noise (retry until finite)
    z <- mode; lp <- -Inf
    for (k in 1:50) {
      cand <- as.numeric(mode + 2 * L0 %*% rnorm(d))
      lp <- safe_lp(cand)
      if (is.finite(lp)) { z <- cand; break }
    }
    if (!is.finite(lp)) { z <- mode; lp <- safe_lp(z) }

    Sigma <- cov0; L <- L0
    log_s <- log(2.38^2 / d) / 2         # proposal scale (on the SD scale)
    hist <- matrix(NA_real_, n_warmup, d)
    n_acc <- 0
    # 3a. warm-up with adaptation
    for (i in seq_len(n_warmup)) {
      prop <- as.numeric(z + exp(log_s) * (L %*% rnorm(d)))
      lpp <- safe_lp(prop)
      a <- if (is.finite(lpp)) min(1, exp(lpp - lp)) else 0
      if (runif(1) < a) { z <- prop; lp <- lpp }
      hist[i, ] <- z
      log_s <- log_s + (a - 0.234) / (i^0.6)
      if (i >= 150 && i %% 50 == 0) {
        win <- hist[ceiling(i / 2):i, , drop = FALSE]
        S <- cov(win)
        if (all(is.finite(S))) {
          S <- S + diag(1e-8 * max(1e-8, mean(diag(S))), d)
          Lt <- tryCatch(t(chol(S)), error = function(e) NULL)
          if (!is.null(Lt)) { Sigma <- S; L <- Lt }
        }
      }
    }
    # 3b. sampling with fixed kernels: each iteration is either a random-walk
    # step or an independence step from a multivariate-t (5 df) fitted to the
    # second half of warm-up. Both are valid Metropolis-Hastings kernels, so
    # their mixture leaves the posterior invariant; the independence step
    # makes near-Gaussian posteriors mix far faster.
    win <- hist[ceiling(n_warmup / 2):n_warmup, , drop = FALSE]
    centre <- colMeans(win)
    Sw <- cov(win)
    Li <- tryCatch(t(chol(1.5^2 * (Sw + diag(1e-10, d)))), error = function(e) NULL)
    nu_t <- 5
    lq <- function(v) {
      u <- forwardsolve(Li, v - centre)
      -(nu_t + d) / 2 * log1p(sum(u^2) / nu_t)
    }
    lq_z <- if (!is.null(Li)) lq(z) else NA
    for (i in seq_len(n_iter)) {
      if (!is.null(Li) && runif(1) < 0.5) {
        prop <- as.numeric(centre + Li %*% (rnorm(d) / sqrt(rchisq(1, nu_t) / nu_t)))
        lpp <- safe_lp(prop)
        lq_p <- lq(prop)
        if (is.finite(lpp) && log(runif(1)) < (lpp - lp) + (lq_z - lq_p)) {
          z <- prop; lp <- lpp; lq_z <- lq_p; n_acc <- n_acc + 1
        }
      } else {
        prop <- as.numeric(z + exp(log_s) * (L %*% rnorm(d)))
        lpp <- safe_lp(prop)
        if (is.finite(lpp) && log(runif(1)) < lpp - lp) {
          z <- prop; lp <- lpp; n_acc <- n_acc + 1
          if (!is.null(Li)) lq_z <- lq(z)
        }
      }
      draws[i, ch, ] <- z
    }
    accept[ch] <- n_acc / n_iter
  }
  list(draws = draws, accept = accept, mode = mode, cov = cov0)
}

# ---- diagnostics (Vehtari et al. 2021) ---------------------------------------------------

pl_split_chains <- function(x) {
  # x: matrix [iter, chain] -> matrix with chains split in half
  n <- nrow(x); h <- floor(n / 2)
  cbind(x[1:h, , drop = FALSE], x[(n - h + 1):n, , drop = FALSE])
}

pl_rank_normalise <- function(x) {
  r <- rank(x, ties.method = "average")
  matrix(qnorm((r - 3 / 8) / (length(x) + 1 / 4)), nrow = nrow(x))
}

pl_rhat_basic <- function(x) {
  n <- nrow(x)
  W <- mean(apply(x, 2, var))
  B <- n * var(colMeans(x))
  if (!is.finite(W) || W <= 0) return(NA_real_)
  sqrt(((n - 1) / n * W + B / n) / W)
}

#' Rank-normalised split R-hat: max of bulk and folded versions
pl_rhat <- function(x) {
  xs <- pl_split_chains(x)
  bulk <- pl_rhat_basic(pl_rank_normalise(xs))
  fold <- pl_rhat_basic(pl_rank_normalise(abs(xs - median(xs))))
  max(bulk, fold)
}

#' ESS of a [iter, chain] matrix using Geyer's initial monotone sequence
pl_ess_basic <- function(x) {
  n <- nrow(x); m <- ncol(x)
  if (n < 4) return(NA_real_)
  acov <- sapply(seq_len(m), function(j) {
    as.numeric(acf(x[, j], lag.max = n - 1, type = "covariance", plot = FALSE, demean = TRUE)$acf)
  })
  if (is.null(dim(acov))) acov <- matrix(acov, ncol = m)
  chain_mean <- colMeans(x)
  mean_var <- mean(acov[1, ]) * n / (n - 1)
  var_plus <- mean_var * (n - 1) / n
  if (m > 1) var_plus <- var_plus + var(chain_mean)
  if (!is.finite(var_plus) || var_plus <= 0) return(NA_real_)
  rho <- 1 - (mean_var - rowMeans(acov)) / var_plus
  rho[1] <- 1
  # Geyer: sum pairs while positive, enforce monotone decrease
  t <- 0; sum_pairs <- 0; prev <- Inf
  while (t + 1 < n) {
    p <- rho[t + 1] + rho[t + 2]
    if (!is.finite(p) || p < 0) break
    p <- min(p, prev); prev <- p
    sum_pairs <- sum_pairs + p
    t <- t + 2
  }
  tau <- -1 + 2 * sum_pairs
  tau <- max(tau, 1 / log10(n * m))
  n * m / tau
}

pl_ess_bulk <- function(x) pl_ess_basic(pl_rank_normalise(pl_split_chains(x)))

pl_ess_tail <- function(x) {
  xs <- pl_split_chains(x)
  q <- quantile(xs, c(0.05, 0.95), names = FALSE)
  min(pl_ess_basic((xs <= q[1]) * 1 + 0), pl_ess_basic((xs <= q[2]) * 1 + 0))
}

#' Summary table for a draws array [iter, chain, param] on the constrained scale
pl_draws_summary <- function(draws, names, level = 0.95) {
  a <- (1 - level) / 2
  rows <- lapply(seq_along(names), function(k) {
    x <- draws[, , k, drop = TRUE]
    if (is.null(dim(x))) x <- matrix(x, ncol = 1)
    v <- as.numeric(x)
    q <- quantile(v, c(a, 0.5, 1 - a), names = FALSE)
    data.frame(param = names[k], mean = mean(v), sd = sd(v), lower = q[1], median = q[2], upper = q[3],
               rhat = suppressWarnings(pl_rhat(x)), ess_bulk = suppressWarnings(pl_ess_bulk(x)),
               ess_tail = suppressWarnings(pl_ess_tail(x)))
  })
  do.call(rbind, rows)
}

#' WAIC from a pointwise log-likelihood matrix [draws, observations]
pl_waic <- function(ll) {
  mx <- apply(ll, 2, max)
  lppd_i <- mx + log(colMeans(exp(sweep(ll, 2, mx))))
  pwaic_i <- apply(ll, 2, var)
  elpd_i <- lppd_i - pwaic_i
  n <- ncol(ll)
  list(elpd = sum(elpd_i), se = sqrt(n * var(elpd_i)), p = sum(pwaic_i), pointwise = elpd_i,
       n_high_p = sum(pwaic_i > 0.4))
}
