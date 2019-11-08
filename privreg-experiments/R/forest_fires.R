# Forest fires analysis script (local version)
# 2019-10-24
# Erik-Jan van Kesteren
# Chang Sun
# Lianne Ippel 
# ------------------------------
library(privreg)
library(tidyverse)
library(firatheme)

dat_a <- readRDS("data/ForestFire/forest_weather.rds")[,-9]
ind <- c(1, 2, 5, 6, 7, 8)
dat_a[, ind] <- lapply(dat_a[, ind], scale)
dat_b <- readRDS("data/ForestFire/forest_firedept.rds")[,-5]
dat_b[,] <- lapply(dat_b, scale)
y   <- readRDS("data/ForestFire/forest_weather.rds")[[9]]
logy <- log(y + 1)

X_a <- model.matrix(logy ~ ., dat_a)[,-1]
X_b <- model.matrix(logy ~ . + 0, dat_b)
result <- privreg_local(logy, X_a, X_b, family = gaussian(), 
                        tol = 1e-12, maxit = 3e4)

df <- tibble(
  param  = as_factor(rep(c(colnames(X_a), colnames(X_b)), 2)),
  method = rep(c("GLM", "privreg"), each = ncol(X_a) + ncol(X_b)),
  coef   = c(result$full$coef, result$priv$coef),
  upper  = coef + 1.96*c(result$full$se, result$priv$se),
  lower  = coef - 1.96*c(result$full$se, result$priv$se)
)

ggplot(df, aes(x = param, y = coef, ymin = lower, ymax = upper, shape = method)) +
  geom_rect(xmin = ncol(X_a) + .5, xmax = Inf, ymin = -Inf, ymax = Inf, 
            fill = "#BBBBBB", alpha = 0.01) +
  geom_pointrange(position = position_dodge(width = .8), fill = "black", size = .3) +
  theme_fira() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  scale_shape_manual(values = c(21, 22)) +
  labs(y = "Coefficient value (95% CI)", x = "", shape = "Method")

firaSave("./img/forest_fires.pdf", width = 9, height = 4)
