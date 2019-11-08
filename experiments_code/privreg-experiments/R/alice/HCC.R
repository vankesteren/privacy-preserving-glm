# Alice hcc data script
# 2019-11-05
# Erik-Jan van Kesteren
# Chang Sun
# Lianne Ippel 
# ----------------------

# load the package
library(privreg)

# load the dataset
hcc_data <- readRDS("data/HCC/hcc_labresults.rds")
hcc_data[,-21] <- lapply(hcc_data[,-21], scale)

# create the regression object
hcc_reg <- PrivReg$new(
  formula   = as.numeric(Class) ~ ., 
  family    = "binomial",
  intercept = TRUE,
  data      = hcc_data,
  name      = "Cambridge",
  verbose   = TRUE,
  crypt_key = "hcc"
)

# adjust max iterations appropriately
hcc_reg$set_control(max_iter = 2000, tol = 1e-13)

# Check if we are connected
hcc_reg$listen()

# If so, continue with the following lines...
hcc_reg$estimate()

# Inspect the results
hcc_reg$summary()

# See how much time it took
hcc_reg$elapsed()

# Save the results to disk
saveRDS(hcc_reg, file = "R/alice/output/hcc_reg.rds")
