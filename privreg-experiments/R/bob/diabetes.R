# Bob test data script
# 2019-11-05
# Erik-Jan van Kesteren
# Chang Sun
# Lianne Ippel 
# ----------------------

# load the package
library(privreg)

# load the dataset
diab_data <- readRDS("data/Diabetes/diab_medication.rds")

# sample 15k participants
set.seed(45)
idx <- sample(nrow(diab_data), 15000)

# create the regression object
diab_reg <- PrivReg$new(
  formula   = readmitted ~ .,
  data      = diab_data[idx, ],
  family    = "binomial",
  intercept = FALSE,
  name      = "Maastricht",
  verbose   = TRUE,
  crypt_key = "diabetes"
)
diab_reg$set_control(tol = 1e-13, max_iter = 500)

# Now Bob should connect ...
diab_reg$connect("131.211.87.37")

# Check if we are connected
diab_reg$connected()

# Parameter estimation initialised from alice site ...

# Inspect the results
diab_reg$summary()

# Inspect the timings
diab_reg$elapsed()

# Save the results to disk
saveRDS(diab_reg, file = "R/bob/output/diab_reg.rds")
