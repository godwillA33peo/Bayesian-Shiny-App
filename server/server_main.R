source("server/mod_normal.R",   local = TRUE)
source("server/mod_poisson.R",  local = TRUE)
source("server/mod_binomial.R", local = TRUE)
source("server/mod_mcmc.R",     local = TRUE)

server <- function(input, output, session) {
  normalServer("normal")
  poissonServer("poisson")
  binomialServer("binomial")
  mcmcServer("mcmc")
}