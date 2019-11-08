# Block / Cyclic coordinate descent image
# 2019-11-07
# Erik-Jan van Kesteren
# ----------------------
library(tidyverse)
library(firatheme) # devtools::install_github("vankesteren/firatheme")
library(cowplot)
library(MASS)

# generate data
set.seed(45)
N <- 100
P <- 9
S <- cov2cor(rWishart(1, P, diag(P))[,,1])
X <- scale(mvrnorm(N, rep(0, P), S), scale = FALSE)
b <- runif(P, -1, 1)
y <- X %*% b + rnorm(N, sd = sqrt(crossprod(b)))

# perform CCD estimation
iter  <- 10000
b_mat <- matrix(0, iter, P)

# marginal estimates
b_hat <- apply(X, 2, function(x) crossprod(x, y)) / apply(X, 2, crossprod)
for (i in 1:iter) {
  b_mat[i,] <- b_hat
  for (p in 1:P) {
    y_res    <- y - X[, -p] %*% b_hat[-p]
    b_hat[p] <- crossprod(X[, p], y_res) / crossprod(X[,p])
  }
}

# plot
coord_plot <- b_mat %>% 
  as_tibble %>% 
  set_names(paste0("b", 1:9)) %>% 
  mutate(Iteration = 1:iter) %>% 
  gather("Parameter", "Estimate", -Iteration) %>% 
  ggplot(aes(x = Iteration, y = Estimate, linetype = Parameter)) +
  geom_line(size = .7) +
  scale_x_log10() +
  theme_fira() +
  ylim(-1, 1.5)

# perform BCD estimation
iter  <- 10000
b_mat <- matrix(0, iter, P)

# blocks
blocks <- list(1:5, 6:9)

# marginal estimates
b_hat <- apply(X, 2, function(x) crossprod(x, y)) / apply(X, 2, crossprod)
for (i in 1:iter) {
  b_mat[i,] <- b_hat
  for (p in blocks) {
    y_res    <- y - X[, -p] %*% b_hat[-p]
    b_hat[p] <- solve(crossprod(X[,p]), crossprod(X[, p], y_res))
  }
}

# plot
block_plot <- b_mat %>% 
  as_tibble %>% 
  set_names(paste0("b", 1:9)) %>% 
  mutate(Iteration = 1:iter) %>% 
  gather("Parameter", "Estimate", -Iteration) %>% 
  ggplot(aes(x = Iteration, y = Estimate, linetype = Parameter)) +
  geom_line(size = .7) +
  scale_linetype_manual(values = c(rep(1, 5), rep(4, 4))) +
  scale_x_log10() +
  theme_fira() +
  ylim(-1, 1.5)

# Combine the image
plot_grid(
  coord_plot + theme(legend.position = "none"), 
  block_plot + theme(legend.position = "none"),  
  nrow = 1, 
  labels = c("A", "B"), 
  label_size = 20,
  label_fontfamily = "Fira Sans"
)

firaSave("paper/paths.pdf", width = 12, height = 5)
firaSave("paper/paths.png", device = "png", width = 12, height = 5, dpi = 300)

