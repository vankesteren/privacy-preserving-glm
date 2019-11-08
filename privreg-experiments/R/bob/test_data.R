# Bob test data script
# 2019-06-03
# Erik-Jan van Kesteren
# Chang Sun
# Lianne Ippel 
# ----------------------

# load the package
library(privreg)

# load the dataset
test_data <- read.csv("bob/data/test_data.csv")

# create the regression object
test_reg <- PrivReg$new(
  formula   = y ~ . + 0,
  data      = test_data,
  name      = "bob",
  verbose   = TRUE,
  crypt_key = "testkey"
)

# Now Bob should connect ...
test_reg$connect("82.13.69.26")

# Check if we are connected
test_reg$connected()

# Parameter estimation and bootstrap initialised from alice site ...

# Inspect the results
test_reg$summary()

# Inspect the timings
test_reg$elapsed()

# Save the results to disk
saveRDS(test_reg, file = "bob/output/test_reg.rds")
