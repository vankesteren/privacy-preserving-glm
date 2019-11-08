# Bob forest fires data script
# 2019-11-04
# Erik-Jan van Kesteren
# Chang Sun
# Lianne Ippel 
# ----------------------

# load the package
library(privreg)

# set up timing measurements

# load the dataset
forest_data <- readRDS("data/ForestFire/forest_firedept.rds")

# scale the dataset (standardize with mean 0 and sd 1) for better convergence
forest_data[, -5] <- lapply(forest_data[, -5], scale)

# create the regression object
forest_reg <- PrivReg$new(
  formula   = log(area + 1) ~ .,
  data      = forest_data,
  family    = gaussian(),
  intercept = FALSE,
  name      = "Maastricht",
  verbose   = TRUE,
  crypt_key = "forest"
)

# set control params
forest_reg$set_control(tol = 1e-12, max_iter = 500, se = TRUE)

# Now Bob should connect ...
forest_reg$connect("131.211.87.37")

# Check if we are connected
forest_reg$connected()

# Parameter estimation initialised from alice site ...

# Inspect the results
forest_reg$summary()

# Inspect the timings
forest_reg$elapsed()

# Save the results to disk
saveRDS(forest_reg, file = "R/bob/output/forest_reg.rds")
