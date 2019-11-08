# Dataset preparation for HCC data
# 2019-10-30
# Erik-Jan van Kesteren
# Chang Sun
# Lianne Ippel 
library(tidyverse)
library(mice)

hcc <- read_csv(
  "dataset_preparation/Original data/hcc.csv",
  col_types = cols_only(
    "INR"        = col_double(),
    "AFP"        = col_double(),
    "Hemoglobin" = col_double(),
    "MCV"        = col_double(),
    "Leucocytes" = col_double(),
    "Platelets"  = col_double(),
    "Albumin"    = col_double(),
    "TotalBil"   = col_double(),
    "ALT"        = col_double(),
    "AST"        = col_double(),
    "GGT"        = col_double(),
    "ALP"        = col_double(),
    "TP"         = col_double(),
    "Creatinine" = col_double(),
    "Nodules"    = col_integer(),
    "MajorDim"   = col_double(),
    "DirBil"     = col_double(),
    "Iron"       = col_double(),
    "Sat"        = col_double(),
    "Ferritin"   = col_double(),
    "Class"      = col_integer(),
    "Gender"     = col_factor(),
    "Symptoms"   = col_factor(),
    "Alcohol"    = col_factor(),
    "Cirrhosis"  = col_factor(),
    "Endemic"    = col_factor(),
    "Smoking"    = col_factor(),
    "Diabetes"   = col_factor(),
    "Obesity"    = col_factor(),
    "Hemochro"   = col_factor(),
    "AHT"        = col_factor(),
    "CRI"        = col_factor(),
    "HIV"        = col_factor(),
    "NASH"       = col_factor(),
    "Varices"    = col_factor(),
    "Spleno"     = col_factor(),
    "PHT"        = col_factor(),
    "PVT"        = col_factor(),
    "Metastasis" = col_factor(),
    "Hallmark"   = col_factor(),
    "Age"        = col_integer(),
    "GramsDay"   = col_double(),
    "PacksYear"  = col_double()
  )
)


# imputed dataset
hcc_imp <- mice::complete(mice::mice(hcc, m = 1, seed = 45))

# split data
hcc_A <- hcc_imp[,1:21]
hcc_B <- hcc_imp[,21:43]

# save data
saveRDS(hcc_A, file = "data/HCC/hcc_labresults.rds")
saveRDS(hcc_B, file = "data/HCC/hcc_characteristics.rds")
 