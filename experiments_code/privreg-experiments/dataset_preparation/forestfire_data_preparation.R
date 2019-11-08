# Dataset preparation for forest fires data
# 2019-10-30
# Erik-Jan van Kesteren
# Chang Sun
# Lianne Ippel
library(tidyverse)

# Weather data
forest_A <- read_csv(
  file = "dataset_preparation/Original data/ForestFire/forestfires.csv",
  col_types = cols_only(
    X     = col_double(),
    Y     = col_double(),
    month = col_factor(),
    day   = col_factor(),
    temp  = col_double(),
    RH    = col_double(),
    wind  = col_double(),
    rain  = col_double(),
    area  = col_double()
  )
)

# Fire dept data
forest_B <- read_csv(
  file = "dataset_preparation/Original data/ForestFire/forestfires.csv",
  col_types = cols_only(
    FFMC = col_double(),
    DMC  = col_double(),
    DC   = col_double(),
    ISI  = col_double(),
    area = col_double()
  )
)

# save data
saveRDS(forest_A, file = "data/ForestFire/forest_weather.rds")
saveRDS(forest_B, file = "data/ForestFire/forest_firedept.rds")