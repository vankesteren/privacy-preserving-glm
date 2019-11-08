# Alice forest fires data script
# 2019-11-04
# Erik-Jan van Kesteren
# Chang Sun
# Lianne Ippel 
# ------------------------------

# load the package
library(privreg)

# set up timing measurements

# load the dataset
forest_data <- readRDS("data/ForestFire/forest_weather.rds")

# scale the dataset (standardize with mean 0 and sd 1) for better convergence
ind <- c(1, 2, 5, 6, 7, 8)
forest_data[, ind] <- lapply(forest_data[, ind], scale)

# create the regression object
forest_reg <- PrivReg$new(
  formula   = log(area + 1) ~ .,
  data      = forest_data,
  family    = gaussian(),
  intercept = FALSE,
  name      = "Cambridge",
  verbose   = TRUE,
  crypt_key = "forest"
)

# set control params
forest_reg$set_control(tol = 1e-12, max_iter = 500, se = TRUE)

# listen to incoming connections
forest_reg$listen()

# Now Bob should connect ...

# Check if we are connected
forest_reg$connected()

# If so, continue with the following lines...

# Estimate the parameters & the standard errors
forest_reg$estimate()

# Inspect the results
forest_reg$summary()

# Inspect the timings
forest_reg$elapsed()

# Save the results to disk
saveRDS(forest_reg, file = "R/alice/output/forest_reg.rds")
