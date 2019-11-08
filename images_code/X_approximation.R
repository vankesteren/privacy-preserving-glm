# Data approximation image
# 2019-07-10
# Erik-Jan van Kesteren
# ----------------------
library(MASS)
library(tidyverse)
library(firatheme)
set.seed(45)

# mean square error function 
mse <- function(x, y) mean((x - y)^2)

# original data properties
P       <- 20      # number of features held by alice
N       <- 100     # number of observations in the data
dat_var <- 2       # variance of the features in data 

# simulate data
X <- matrix(rnorm(N * P, sd = sqrt(dat_var)), N)

# perform rank-r approximations
mses <- numeric(P)
for (r in 1:P) {
  # simulate B matrix from r bootstrap samples
  B <- matrix(rep(1:P, r), P) + matrix(rnorm(P * r, sd = 0.01), P)
  # r predictions are shared with bob
  Y_prd <- X %*% B
  # X approximation by bob based on Moore-Penrose inverse
  X_min <- Y_prd %*% ginv(B)
  # compute the mean square error of approximation
  mses[r] <- mse(X_min, X)
}

# plot, together with prediction
pp <-
  tibble(mse = mses, rank = 1:P, pred = dat_var - (dat_var / P)*(1:P)) %>% 
  ggplot(aes(x = rank, y = mse)) + 
  geom_line(col = firaCols[1], size = 1) + 
  geom_point(col = firaCols[1], size = 1) + 
  geom_line(aes(y = pred), lty = 2, size = 1) + 
  theme_fira() +
  labs(y = "MSE", x = "Rank approximation of X")

# save plot
firaSave(plot = pp, filename = "paper/x_approx.pdf", device = "pdf", 
         width = 6, height = 4)

firaSave(plot = pp, filename = "paper/x_approx.png", device = "png", 
         width = 6, height = 4, dpi = 300)
