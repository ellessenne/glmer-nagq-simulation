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
.B <- 1000
# Number of patients per dataset
.N <- 500
# Number of integration points to test
.k <- c(0:10, seq(15, 50, by = 5))
# Dataset with scenarios/repetition combinations:
scenarios <- crossing(
  k = .k,
  b = seq(.B)
)
# Random seed for reproducibility
set.seed(138475683)

# True parameters
truth <- tibble(
  term = c("(Intercept)", "trt", "time", "trt:time", "sd__(Intercept)"),
  true = c(-2, -1, 0.5, -0.1, 4)
)
saveRDS(object = truth, file = "data/03-truth.RDS")

# Simulate .B datasets:
data <- map(.x = seq(.B), .f = function(i) {
  simulate_data(
    i = i,
    N = .N,
    beta0 = truth$true[truth$term == "(Intercept)"],
    beta1 = truth$true[truth$term == "trt"],
    beta2 = truth$true[truth$term == "time"],
    beta3 = truth$true[truth$term == "trt:time"],
    sigma_b = truth$true[truth$term == "sd__(Intercept)"]
  )
})

# Run .B repetitions using {furrr}
plan(multisession)
res <- future_map_dfr(.x = seq(nrow(scenarios)), .f = function(i) {
  fit_model(i = unique(data[[scenarios$b[i]]][["i"]]), data = data[[scenarios$b[i]]], k = scenarios$k[i])
}, .progress = TRUE)
plan(sequential)
saveRDS(object = res, file = "data/03-simulation-res.RDS")
