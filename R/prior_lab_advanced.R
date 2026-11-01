# ============================================================================
# Prior Lab: simulators for multivariate and structured priors
# ============================================================================
#   - shrinkage priors for many regression coefficients
#       Normal, Laplace, regularised horseshoe (Piironen & Vehtari 2017),
#       R2D2 (Zhang et al. 2022, as implemented in brms), spike-and-slab
#   - Dirichlet priors for proportions that sum to 1
#   - Inverse-Wishart vs LKJ + independent SDs for covariance matrices
# ============================================================================

PL_SHRINK <- c(normal = "Normal", laplace = "Laplace (Bayesian lasso)",
               horseshoe = "Regularised horseshoe", r2d2 = "R2D2", spike = "Spike-and-slab")
PL_SHRINK_COLS <- c(normal = "#2a78d6", laplace = "#eb6834", horseshoe = "#0f9b7a",
                    r2d2 = "#a03fb0", spike = "#52514e")
PL_SHRINK_LTY <- c(normal = "solid", laplace = "longdash", horseshoe = "dashed",
                   r2d2 = "dotdash", spike = "dotted")

pl_rdirichlet <- function(n, alpha) {
  g <- matrix(rgamma(n * length(alpha), shape = rep(alpha, each = n)), n)
  g / rowSums(g)
}

#' Draw S coefficient vectors of length p from a shrinkage prior.
#' @param o list of settings: normal_sd, laplace_b, p0, n, sigma, slab_scale,
#'   slab_df, r2_mean, r2_prec, cons_d2, spike_pi, slab_sd, spike_sd
pl_sim_shrinkage <- function(type, p, o, S = 4000) {
  switch(type,
    normal = matrix(rnorm(S * p, 0, o$normal_sd), S),
    laplace = matrix(o$laplace_b * (rexp(S * p) - rexp(S * p)), S),
    horseshoe = {
      tau0 <- o$p0 / max(p - o$p0, 1) * o$sigma / sqrt(o$n)
      tau <- abs(rcauchy(S, 0, tau0))
      lam <- matrix(abs(rcauchy(S * p)), S)
      c2 <- 1 / rgamma(S, o$slab_df / 2, rate = o$slab_df * o$slab_scale^2 / 2)
      lt2 <- c2 * lam^2 / (c2 + tau^2 * lam^2)          # regularised local scales
      matrix(rnorm(S * p), S) * sqrt(tau^2 * lt2)
    },
    r2d2 = {
      r2 <- rbeta(S, o$r2_mean * o$r2_prec, (1 - o$r2_mean) * o$r2_prec)
      phi <- pl_rdirichlet(S, rep(o$cons_d2, p))
      matrix(rnorm(S * p), S) * o$sigma * sqrt(r2 / (1 - r2) * phi)
    },
    spike = {
      g <- matrix(runif(S * p) < o$spike_pi, S)
      matrix(rnorm(S * p), S) * ifelse(g, o$slab_sd, o$spike_sd)
    }
  )
}

#' Implied R^2 with p independent standardised predictors and residual SD sigma
pl_implied_r2 <- function(B, sigma) {
  v <- rowSums(B^2)
  v / (v + sigma^2)
}

pl_shrink_brms <- function(type, p, o) {
  switch(type,
    normal = sprintf("prior(normal(0, %s), class = \"b\")", pl_fmt(o$normal_sd)),
    laplace = sprintf("prior(double_exponential(0, %s), class = \"b\")", pl_fmt(o$laplace_b)),
    horseshoe = sprintf("prior(horseshoe(df = 1, par_ratio = %s, scale_slab = %s, df_slab = %s), class = \"b\")",
                        pl_fmt(o$p0 / max(p - o$p0, 1), 3), pl_fmt(o$slab_scale), pl_fmt(o$slab_df)),
    r2d2 = sprintf("prior(R2D2(mean_R2 = %s, prec_R2 = %s, cons_D2 = %s), class = \"b\")",
                   pl_fmt(o$r2_mean), pl_fmt(o$r2_prec), pl_fmt(o$cons_d2)),
    spike = sprintf("# not in brms. In Stan, a continuous spike-and-slab:\n# for (j in 1:K) target += log_mix(%s, normal_lpdf(b[j] | 0, %s), normal_lpdf(b[j] | 0, %s));",
                    pl_fmt(o$spike_pi), pl_fmt(o$slab_sd), pl_fmt(o$spike_sd))
  )
}

# ---- covariance priors ------------------------------------------------------------------

#' Inverse-Wishart(nu, s^2 I) draws: returns SD of variable 1 and correlation r12
pl_sim_iw <- function(K, nu, s, S = 4000) {
  Sinv <- diag(1 / s^2, K)
  W <- rWishart(S, nu, Sinv)
  out <- t(vapply(seq_len(S), function(i) {
    Sig <- solve(W[, , i])
    c(sd1 = sqrt(Sig[1, 1]), r12 = Sig[1, 2] / sqrt(Sig[1, 1] * Sig[2, 2]))
  }, numeric(2)))
  as.data.frame(out)
}

#' LKJ(eta) correlations with independent SD priors: SDs and r12 are independent,
#' and r12 has the exact LKJ marginal (r + 1)/2 ~ Beta(eta - 1 + K/2, same)
pl_sim_lkj <- function(K, eta, sd_prior, S = 4000) {
  a <- eta - 1 + K / 2
  data.frame(sd1 = pl_r(sd_prior, S), r12 = 2 * rbeta(S, a, a) - 1)
}
