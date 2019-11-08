# Bob test data script
# 2019-11-05
# Erik-Jan van Kesteren
# Chang Sun
# Lianne Ippel 
# ----------------------

# load the package
library(privreg)

# load the dataset
hcc_data <- readRDS("data/HCC/hcc_characteristics.rds")
hcc_data[, 21:23] <- lapply(hcc_data[, 21:23], scale)

# create the regression object
hcc_reg <- PrivReg$new(
  formula   = as.numeric(Class) ~ ., 
  family    = "binomial",
  intercept = FALSE,
  data      = hcc_data,
  name      = "Maastricht",
  verbose   = TRUE,
  crypt_key = "hcc"
)

# adjust max iterations appropriately
hcc_reg$set_control(max_iter = 2000, tol = 1e-13)

# Now Bob should connect ...
hcc_reg$connect("131.211.87.37")

# Check if we are connected
hcc_reg$connected()

# Parameter estimation initialised from alice site ...

# Inspect the results
hcc_reg$summary()

# Inspect the timings
hcc_reg$elapsed()

# Save the results to disk
saveRDS(hcc_reg, file = "R/bob/output/hcc_reg.rds")
