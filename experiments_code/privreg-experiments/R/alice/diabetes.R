# Alice test data script
# 2019-11-05
# Erik-Jan van Kesteren
# Chang Sun
# Lianne Ippel 
# ----------------------

# load the package
library(privreg)

# load the dataset
diab_data <- readRDS("data/Diabetes/diab_administration.rds")

# sample 15k participants
set.seed(45)
idx <- sample(nrow(diab_data), 15000)

# create the regression object
diab_reg <- PrivReg$new(
  formula   = readmitted ~ .,
  data      = diab_data[idx, ], 
  family    = "binomial",
  intercept = TRUE,
  name      = "Cambridge",
  verbose   = TRUE,
  crypt_key = "diabetes"
)
diab_reg$set_control(tol = 1e-13, max_iter = 500)

# listen to incoming connections
diab_reg$listen()

# Now Bob should connect ...

# Check if we are connected
diab_reg$connected()

# If so, continue with the following lines...

# First estimate the parameters
diab_reg$estimate()

# Inspect the results
diab_reg$summary()

# Save the results to disk
saveRDS(diab_reg, file = "R/alice/output/diab_reg.rds")
