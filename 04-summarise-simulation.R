# Script to run a simulation with varying number of knots for the integration in glmer()
# ---

# Packages
library(tidyverse)
library(ggridges)
library(rsimsum)
library(ragg)
library(hrbrthemes)

# Load and combine simulation results
simdata <- readRDS(file = "data/03-simulation-res.RDS")
truth <- readRDS(file = "data/03-truth.RDS")
res <- left_join(simdata, truth, by = "term")

# Analyse results of simulation study
sims <- multisimsum(data = res, par = "term", estvarname = "estimate", true = "true", by = "gq")
sims <- tidy(summary(sims, stats = "bias")) %>%
  mutate(sig = as.numeric(lower > 0 | upper < 0)) %>%
  mutate(gqn = as.numeric(as.character(gq))) %>%
  select(-stat) %>%
  rename(Term = term)

# Plot for bias
pbias <- ggplot(sims, aes(x = gqn, y = est, color = factor(sig))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey75") +
  geom_line(linetype = "dotted") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 1 / 2) +
  geom_point() +
  facet_wrap(~Term, scales = "free_y", ncol = 2, labeller = label_both) +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  scale_color_manual(values = c("grey50", "black")) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "none") +
  labs(x = "Quadrature Points", y = "Bias (95% Monte Carlo C.I.)")
pbias
ggsave(filename = "figures/pbias.png", plot = pbias, device = agg_png, width = 6, height = 5)

ptime <- res %>%
  distinct(gq, i, time) %>%
  ggplot(aes(x = time, y = factor(gq), fill = stat(x))) +
  geom_density_ridges_gradient(show.legend = FALSE, rel_min_height = 0.01) +
  scale_fill_viridis_c(option = "magma") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_bw(base_size = 12) +
  labs(x = "Time (in seconds)", y = "Quadrature Points")
ptime
ggsave(filename = "figures/ptime.png", plot = ptime, device = agg_png, width = 4, height = 4)
