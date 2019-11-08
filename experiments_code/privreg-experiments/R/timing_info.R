# Get timing info for analyses
# 2019-11-05
# Erik-Jan van Kesteren
# ----------------------

# forest fires
readRDS("R/alice/output/forest_reg.rds")$elapsed()

# HCC
readRDS("R/alice/output/hcc_reg.rds")$elapsed()

# diabetes
readRDS("R/alice/output/diab_reg.rds")$elapsed()

