library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(shinythemes)
# learnr and rsconnect were loaded here before but are not used by the running
# app (rsconnect is only needed on the machine that deploys). Dropping them
# makes the deployed bundle smaller and start faster.

# Load custom Bayesian utility functions (extracted from TeachBayes)
source("R/bayes_utils.R", local = TRUE)

# Prior Lab: distributions, maths engine and shared UI pieces
source("R/prior_lab_dists.R", local = TRUE)
source("R/prior_lab_engine.R", local = TRUE)
source("R/prior_lab_families.R", local = TRUE)
source("R/prior_lab_shared.R", local = TRUE)
source("R/prior_lab_mcmc.R", local = TRUE)
source("R/prior_lab_models.R", local = TRUE)
source("R/prior_lab_advanced.R", local = TRUE)
source("R/prior_lab_presets.R", local = TRUE)
