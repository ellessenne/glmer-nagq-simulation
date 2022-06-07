# Function to simulate data according to a longitudinal logistic regression model with a random intercept
simulate_data <- function(i, # Indexing the repetition
                          N, # Number of subjects to simulate in a cohort
                          beta0 = -2, # Intercept of the logistic regression model
                          beta1 = -1, # Fixed effect of treatment
                          beta2 = 0.5, # Fixed effect of time
                          beta3 = -0.1, # Fixed treatment-time interaction
                          sigma_b = 4, # SD of the random intercept distribution
                          n = 100, # Number of potential, uniformly-distributed visits to simulate per patient
                          max_t = 10 # Maximum follow-up time
) {
  baseline <- tibble(
    i = i,
    id = seq(N),
    trt = rbinom(n = N, size = 1, prob = 0.5),
    b0i = rnorm(n = N, sd = sigma_b)
  )
  full <- crossing(
    baseline,
    n = seq(n)
  ) %>%
    mutate(u = runif(n = nrow(.))) %>%
    arrange(id, n) %>%
    group_by(id) %>%
    mutate(time = cumsum(u)) %>%
    ungroup() %>%
    filter(time <= max_t) %>%
    mutate(p = inv.logit(beta0 + b0i + beta1 * trt + beta2 * time + beta3 * trt * time)) %>%
    mutate(y = rbinom(n = nrow(.), size = 1, prob = p)) %>%
    select(i, id, y, time, trt)
  return(full)
}
