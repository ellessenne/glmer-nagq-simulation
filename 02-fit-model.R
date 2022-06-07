# Function to fit a correctly-specified model using glmer()
fit_model <- function(i, # Indexes the repetition
                      data, # Simulated dataset
                      k # Number of integration points
) {
  library(lme4)
  t1 <- Sys.time()
  fit <- glmer(
    formula = y ~ trt * time + (1 | id),
    data = data,
    family = binomial(link = "logit"),
    nAGQ = k
  )
  t2 <- Sys.time()
  library(broom.mixed)
  tidy(fit) %>%
    select(term, estimate, std.error) %>%
    mutate(gq = k, i = i) %>%
    mutate(time = as.numeric(difftime(t2, t1, units = "secs")))
}
