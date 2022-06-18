# Script to summarise the results of the simulation study
# ---

# Packages
library(tidyverse)
library(ggridges)
library(rsimsum)
library(ragg)

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

# Plot for bias, fixed effects only
pbias_fixed <- sims %>%
  filter(Term != "sd__(Intercept)") %>%
  ggplot(aes(x = gqn, y = est, color = factor(sig))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey75") +
  geom_line(linetype = "dotted") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 1 / 2) +
  geom_point() +
  facet_wrap(~Term, scales = "free_y", ncol = 2, labeller = label_both) +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  scale_y_continuous(n.breaks = 5) +
  scale_color_manual(values = c("grey50", "black")) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "none") +
  labs(x = "Quadrature Points", y = "Bias (95% C.I.)")
pbias_fixed
ggsave(filename = "figures/pbias_fixed.png", plot = pbias_fixed, device = agg_png, width = 5, height = 5 * 3 / 4, dpi = 600)

# Plot for bias, variance components
pbias_var <- sims %>%
  filter(Term == "sd__(Intercept)") %>%
  ggplot(aes(x = gqn, y = est, color = factor(sig))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey75") +
  geom_line(linetype = "dotted") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 1 / 2) +
  geom_point() +
  facet_wrap(~Term, scales = "free_y", ncol = 2, labeller = label_both) +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  scale_y_continuous(n.breaks = 5) +
  scale_color_manual(values = c("grey50", "black")) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), legend.position = "none") +
  labs(x = "Quadrature Points", y = "Bias (95% C.I.)")
pbias_var
ggsave(filename = "figures/pbias_var.png", plot = pbias_var, device = agg_png, width = 4, height = 3, dpi = 600)

# Plot for time distribution
ptime <- res %>%
  distinct(gq, i, time) %>%
  ggplot(aes(x = factor(gq), y = time)) +
  geom_boxplot(outlier.size = 0.1) +
  scale_y_continuous(n.breaks = 5) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Time (in seconds)", x = "Quadrature Points")
ptime
ggsave(filename = "figures/ptime.png", plot = ptime, device = agg_png, width = 4, height = 3, dpi = 600)

# Convergence?
pconv <- ggplot(sims, aes(y = mcse, x = rank(mcse), color = Term)) +
  geom_hline(yintercept = 0.01, linetype = "dashed", color = "red") +
  geom_point() +
  scale_y_continuous(n.breaks = 5) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom") +
  labs(y = "Monte Carlo Standard Error", x = "Rank(Monte Carlo Standard Error)", color = "")
ggsave(filename = "figures/pconv.png", plot = pconv, device = agg_png, width = 6, height = 4, dpi = 600)
