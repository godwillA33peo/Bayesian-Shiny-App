# Optional, slow (compiles 3 Stan models). Run from the project root:
#   Rscript tests/stan_crosscheck.R
# Fits the generated Stan programs with rstan and compares them with the
# in-app sampler. Needs rstan and a C++ toolchain.
for (f in c("R/prior_lab_dists.R","R/prior_lab_engine.R","R/prior_lab_families.R","R/prior_lab_mcmc.R","R/prior_lab_models.R")) source(f)
suppressMessages(library(rstan)); rstan_options(auto_write = FALSE)
for (model in c("zip", "ordinal", "weibull")) {
  df <- PL_EXAMPLES[[model]]$make()
  d <- pl_model_data(model, df$y, df$x, df$event)
  pri <- PL_MODELS[[model]]$defaults(d)
  code <- pl_stan_model_program(model, TRUE, pri, d$K)
  sd <- list(N = d$N, y = if (PL_MODELS[[model]]$y_type %in% c("count", "ordinal", "binary")) as.integer(d$y) else d$y, x = d$x)
  if (model == "ordinal") sd$K <- d$K
  if (model == "weibull") sd$event <- as.integer(d$event)
  t0 <- Sys.time()
  fit <- stan(model_code = code, data = sd, chains = 4, iter = 2000, seed = 1, refresh = 0)
  st <- summary(fit)$summary
  set.seed(2026)
  app <- pl_model_fit(model, d, pri, 1000, 2000)
  s <- pl_draws_summary(app$draws, app$names)
  stan_names <- sub("cut\\[", "cut[", s$param)
  cat(sprintf("\n== %s (Stan %.0fs incl. compile)\n", model, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  print(data.frame(param = s$param, app_mean = round(s$mean, 4), stan_mean = round(st[stan_names, "mean"], 4),
                   app_sd = round(s$sd, 4), stan_sd = round(st[stan_names, "sd"], 4)), row.names = FALSE)
}
