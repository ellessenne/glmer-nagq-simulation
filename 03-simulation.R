# Script to run a simulation with varying number of knots for the integration in glmer()
# ---

# Packages
library(tidyverse)
library(lme4)
library(boot)
library(future)
library(furrr)

# Load scripts
source("01-simulate-data.R")
source("02-fit-model.R")

# Simulation settings:
# Number of repetitions
.B <- 10
# Number of patients per dataset
.N <- 200
# Number of integration points to test
.k <- c(0:10, 15, 20, 25, 30, 40, 50)
# Dataset with scenarios/repetition combinations:
scenarios <- crossing(
  k = .k,
  b = seq(.B)
)

# Simulate .B datasets:
data <- map(.x = seq(.B), .f = function(i) simulate_data(i = i, N = .N))

# Run .B repetitions using {furrr}
plan(multisession)
res <- future_map_dfr(.x = seq(nrow(scenarios)), .f = function(i) {
  fit_model(i = unique(data[[scenarios$b[i]]][["i"]]), data = data[[scenarios$b[i]]], k = scenarios$k[i])
}, .progress = TRUE)
plan(sequential)
saveRDS(object = res, file = "data/03-simulation-res.RDS")
