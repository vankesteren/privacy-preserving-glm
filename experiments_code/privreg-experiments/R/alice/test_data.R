# Alice test data script
# 2019-06-03
# Erik-Jan van Kesteren
# Chang Sun
# Lianne Ippel 
# ----------------------

# load the package
library(privreg)

# set up timing measurements

# load the dataset
test_data <- read.csv("alice/data/test_data.csv")

# create the regression object
test_reg <- PrivReg$new(
  formula   = y ~ . + 0,
  data      = test_data,
  name      = "Cambridge",
  verbose   = TRUE,
  crypt_key = "testkey"
)

# listen to incoming connections
test_reg$listen()

# Now Bob should connect ...

# Check if we are connected
test_reg$connected()

# If so, continue with the following lines...

# First estimate the parameters
test_reg$estimate()

# when it finishes, run the following line

# (optional) run bootstrap
test_reg$bootstrap(R = 1000)

# Inspect the results
test_reg$summary()

# Inspect the timings
test_reg$elapsed()

# Save the results to disk
saveRDS(test_reg, file = "alice/output/test_reg.rds")
