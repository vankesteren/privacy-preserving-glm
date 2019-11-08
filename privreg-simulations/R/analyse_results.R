library(tidyverse)
library(firatheme)
# analyse simulation
results    <- readRDS("./data/results.rds")
conditions <- readRDS("./data/conditions.rds")

# convergence
conditions$iter      <- sapply(results, function(cnd) mean(sapply(cnd, function(r) r$priv$iter)))
conditions$iter_up   <- sapply(results, function(cnd) quantile(sapply(cnd, function(r) r$priv$iter), 0.975))
conditions$iter_lo   <- sapply(results, function(cnd) quantile(sapply(cnd, function(r) r$priv$iter), 0.025))

# parameter benchmark
conditions$coef <- sapply(results, function(cnd) mean(c(sapply(cnd, function(r) {
  (r$priv$coef - r$full$coef) / r$full$coef
}))))*100
conditions$coef_up <- sapply(results, function(cnd) quantile(c(sapply(cnd, function(r) {
  (r$priv$coef - r$full$coef) / r$full$coef
})), 0.975))*100
conditions$coef_lo <- sapply(results, function(cnd) quantile(c(sapply(cnd, function(r) {
  (r$priv$coef - r$full$coef) / r$full$coef
})), 0.025))*100

# se bias
conditions$se <- sapply(results, function(cnd) mean(c(sapply(cnd, function(r) {
  (r$priv$se - r$full$se) / r$full$se
}))))*100
conditions$se_up <- sapply(results, function(cnd) quantile(c(sapply(cnd, function(r) {
  (r$priv$se - r$full$se) / r$full$se
})), 0.975))*100
conditions$se_lo <- sapply(results, function(cnd) quantile(c(sapply(cnd, function(r) {
  (r$priv$se - r$full$se) / r$full$se
})), 0.025))*100


# z-score bias
conditions$z <- sapply(results, function(cnd) mean(c(sapply(cnd, function(r) {
  zfull <- r$full$coef/r$full$se
  zpriv <- r$priv$coef/r$priv$se
  abs((zpriv - zfull) / zfull)
}))))*100
conditions$z_up <- sapply(results, function(cnd) quantile(c(sapply(cnd, function(r) {
  zfull <- r$full$coef/r$full$se
  zpriv <- r$priv$coef/r$priv$se
  abs((zpriv - zfull) / zfull)
})), 0.975))*100
conditions$z_lo <- sapply(results, function(cnd) quantile(c(sapply(cnd, function(r) {
  zfull <- r$full$coef/r$full$se
  zpriv <- r$priv$coef/r$priv$se
  abs((zpriv - zfull) / zfull)
})), 0.025))*100

# plots
pd <- position_dodge(width = 10)

# convergence plot
conditions %>% 
  ggplot(aes(x = P, y = iter, ymin = iter_lo, ymax = iter_up, linetype = Pcov, shape = Pcov)) +
  geom_line(size = .6, position = pd) +
  geom_pointrange(size = .6, position = pd, fill = "black", linetype = 1) +
  facet_wrap(~outcome) +
  xlim(0, max(conditions$P) + 5) +
  theme_fira() +
  scale_shape_manual(values = c(21, 22)) +
  labs(x = "Number of features", y = "Iterations", 
       linetype = "Feature\ncovariance", shape = "Feature\ncovariance") +
  theme(legend.title.align = 0)

firaSave("img/iter_plot.pdf", width = 9, height = 5)

# Coefficient plot
conditions %>% 
  ggplot(aes(x = P, y = coef, ymin = coef_lo, ymax = coef_up, linetype = Pcov, shape = Pcov)) +
  geom_line(size = .6, position = pd) +
  geom_pointrange(size = .6, position = pd, fill = "black", linetype = 1) +
  facet_wrap(~outcome) +
  xlim(0, max(conditions$P) + 5) +
  theme_fira() +
  scale_shape_manual(values = c(21, 22)) +
  labs(x = "Number of features", y = "Coefficient bias %", 
       linetype = "Feature\ncovariance", shape = "Feature\ncovariance") +
  theme(legend.title.align = 0)

firaSave("img/coef_plot.pdf", width = 9, height = 5)

# SE plot
conditions %>% 
  ggplot(aes(x = P, y = se, ymin = se_lo, ymax = se_up, linetype = Pcov, shape = Pcov)) +
  geom_line(size = .6, position = pd) +
  geom_pointrange(size = .6, position = pd, fill = "black", linetype = 1) +
  facet_wrap(~outcome) +
  xlim(0, max(conditions$P) + 5) +
  theme_fira() +
  scale_shape_manual(values = c(21, 22)) +
  labs(x = "Number of features", y = "Standard error bias %", 
       linetype = "Feature\ncovariance", shape = "Feature\ncovariance") +
  theme(legend.title.align = 0)

firaSave("img/se_plot.pdf", width = 9, height = 5)

# t score plot
conditions %>% 
  ggplot(aes(x = P, y = z, ymin = z_lo, ymax = z_up, linetype = Pcov, shape = Pcov)) +
  geom_line(size = .6, position = pd) +
  geom_pointrange(size = .6, position = pd, fill = "black", linetype = 1) +
  facet_wrap(~outcome) +
  xlim(0, max(conditions$P) + 5) +
  theme_fira() +
  scale_shape_manual(values = c(21, 22)) +
  labs(x = "Number of features", y = "T-score bias %", 
       linetype = "Feature\ncovariance", shape = "Feature\ncovariance") +
  theme(legend.title.align = 0)

firaSave("img/z_plot.pdf", width = 9, height = 5)


