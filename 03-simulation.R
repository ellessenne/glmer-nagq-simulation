# Script to run a simulation with varying number of knots for the integration in glmer()
# ---

# Packages
library(tidyverse)
library(lme4)
library(boot)
library(future)
library(furrr)
library(rsimsum)

# Load scripts
source("01-simulate-data.R")
source("02-fit-model.R")

# Simulation settings:
# First, running 20 repetitions to estimate how many repetitions to run to
# have Monte Carlo errors < 0.01
.Bp <- 20
# Number of patients per dataset
.N <- 500
# Number of integration points to test
.k <- c(0:10, seq(15, 50, by = 5))
# Dataset with scenarios/repetition combinations:
scenarios <- crossing(
  k = .k,
  b = seq(.Bp)
)
# Random seed for reproducibility
set.seed(138475683)

# True parameters
truth <- tibble(
  term = c("(Intercept)", "trt", "time", "trt:time", "sd__(Intercept)"),
  true = c(-2, -1, 0.5, -0.1, 4)
)
saveRDS(object = truth, file = "data/03-truth.RDS")

# Simulate .Bp datasets:
data <- map(.x = seq(.Bp), .f = function(i) {
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

# Run .Bp repetitions using {furrr}
plan(multisession)
res <- future_map_dfr(.x = seq(nrow(scenarios)), .f = function(i) {
  fit_model(i = unique(data[[scenarios$b[i]]][["i"]]), data = data[[scenarios$b[i]]], k = scenarios$k[i])
}, .progress = TRUE)
plan(sequential)
saveRDS(object = res, file = "data/03-simulation-res-preliminary.RDS")

# Estimate .B to expect a Monte Carlo error < 0.01
res <- left_join(res, truth, by = "term")
s <- multisimsum(data = res, par = "term", estvarname = "estimate", se = "std.error", true = "true", by = "gq")
# View(get_data(summary(s)), title = "Rectangular")
mVar <- max(tidy(summary(s, stats = c("empse", "modelse")))$est^2, na.rm = TRUE)
# Number of replications .B to expect a Monte Carlo SE of bias of 0.01
# Rounding up to the nearest 100th to be conservative
.B <- plyr::round_any(x = mVar / (0.01^2), accuracy = 100, f = ceiling)
print(.B)
saveRDS(object = list(.B = .B, mVar = mVar), file = "data/03-simulation-full-mccalc.RDS")

# Now, simulate .B datasets and run .B simulations:
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
scenarios <- crossing(
  k = .k,
  b = seq(.B)
)

# Now, run .B repetitions using {furrr}
plan(multisession)
res <- future_map_dfr(.x = seq(nrow(scenarios)), .f = function(i) {
  fit_model(i = unique(data[[scenarios$b[i]]][["i"]]), data = data[[scenarios$b[i]]], k = scenarios$k[i])
}, .progress = TRUE)
plan(sequential)
saveRDS(object = res, file = "data/03-simulation-res.RDS")
