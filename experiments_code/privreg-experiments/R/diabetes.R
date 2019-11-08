# Diabetes analysis script (local version)
# 2019-10-24
# Erik-Jan van Kesteren
# Chang Sun
# Lianne Ippel 
# ------------------------------
library(privreg)
library(tidyverse)
library(firatheme)

# read the data
dat_a <- readRDS("data/Diabetes/diab_administration.rds") %>% select(-readmitted)
dat_b <- readRDS("data/Diabetes/diab_medication.rds")     %>% select(-readmitted)
y     <- readRDS("data/Diabetes/diab_administration.rds") %>% .[["readmitted"]]

# use only 1.5k samples to make standard error computable
set.seed(45)
idx <- sample(nrow(dat_a), 15000)
X_a <- model.matrix(y ~ ., dat_a)
X_b <- model.matrix(y ~ ., dat_b)[,-1]
result <- privreg_local(as.numeric(y)[idx], X_a[idx, ], X_b[idx, ], family = binomial(), 
                        tol = 1e-13, maxit = 1e3, se = TRUE)

# create a plot
df <- tibble(
  param  = as_factor(rep(c(colnames(X_a), colnames(X_b)), 2)),
  method = rep(c("GLM", "privreg"), each = ncol(X_a) + ncol(X_b)),
  coef   = c(result$full$coef, result$priv$coef),
  upper  = coef + 1.96*c(result$full$se, result$priv$se),
  lower  = coef - 1.96*c(result$full$se, result$priv$se)
)

ggplot(df, aes(x = param, y = coef, shape = method, ymin = lower, ymax = upper)) +
  geom_rect(xmin = ncol(X_a) + .5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "#AAAAAA02") + 
  geom_pointrange(position = position_dodge(width = .8), fill = "black", size = .3) +
  theme_fira() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  scale_shape_manual(values = c(21, 22)) +
  labs(y = "Coefficient value (95% CI)", x = "", shape = "Method")

firaSave("./img/diabetes.pdf", width = 9, height = 5)

# noteworthy: compare insulin to only pharma data
summary(glm(y~X_b, family = "binomial"))$coefficients[13,] # significant positive effect
summary(glm(y~X_a+X_b+0, family = "binomial"))$coefficients[41,] # significant negative effect

 