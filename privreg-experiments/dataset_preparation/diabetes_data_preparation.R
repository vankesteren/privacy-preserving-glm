# Dataset preparation for diabetes data
# 2019-10-30
# Erik-Jan van Kesteren
# Chang Sun
# Lianne Ippel
library(tidyverse)

# Link to paper: https://www.hindawi.com/journals/bmri/2014/781670/
# Column tables: https://www.hindawi.com/journals/bmri/2014/781670/tab1/

# Read the dataset
diab <- read_csv(
  "dataset_preparation/Original data/Diabetes/diabetic_data.csv",
  col_types = cols_only(
    "race"                     = col_factor(),
    "gender"                   = col_factor(),
    "age"                      = col_factor(ordered = TRUE),
    "time_in_hospital"         = col_integer(),
    "num_lab_procedures"       = col_integer(),
    "num_procedures"           = col_integer(),
    "num_medications"          = col_integer(),
    "number_outpatient"        = col_integer(),
    "number_emergency"         = col_integer(),
    "number_inpatient"         = col_integer(),
    "number_diagnoses"         = col_integer(),
    "diabetesMed"              = col_factor(),
    "change"                   = col_factor(),
    "max_glu_serum"            = col_character(),
    "A1Cresult"                = col_character(),
    "metformin"                = col_character(),
    "repaglinide"              = col_character(),
    "nateglinide"              = col_character(),
    "chlorpropamide"           = col_character(),
    "glimepiride"              = col_character(),
    "acetohexamide"            = col_character(),
    "glipizide"                = col_character(),
    "glyburide"                = col_character(),
    "tolbutamide"              = col_character(),
    "pioglitazone"             = col_character(),
    "rosiglitazone"            = col_character(),
    "acarbose"                 = col_character(),
    "miglitol"                 = col_character(),
    "troglitazone"             = col_character(),
    "tolazamide"               = col_character(),
    "insulin"                  = col_character(),
    "glyburide-metformin"      = col_character(),
    "glipizide-metformin"      = col_character(),
    "glimepiride-pioglitazone" = col_character(),
    "metformin-pioglitazone"   = col_character(),
    "readmitted"               = col_character()
  ), 
  na = "?"
) %>% mutate(
  "gender"                   = 
    as_factor(ifelse(gender == "Unknown/Invalid", NA, levels(gender)[gender])),
  "glu_serum_positive"       = case_when(
    max_glu_serum == "Norm" ~ "No",
    max_glu_serum == "None" ~ "NA",
    TRUE ~ "Yes"
  ),
  "glu_serum_positive"       = as_factor(ifelse(glu_serum_positive == "NA", "Unknown", glu_serum_positive)),
  "A1C_positive"             = case_when(
    A1Cresult == "Norm" ~ "No",
    A1Cresult == "None" ~ "NA",
    TRUE ~ "Yes"
  ),
  "A1C_positive"             = as_factor(ifelse(A1C_positive == "NA", "Unknown", A1C_positive)),
  "metformin"                = as_factor(ifelse(metformin                  == "No", "No", "Yes")),
  "repaglinide"              = as_factor(ifelse(repaglinide                == "No", "No", "Yes")),
  "nateglinide"              = as_factor(ifelse(nateglinide                == "No", "No", "Yes")),
  "chlorpropamide"           = as_factor(ifelse(chlorpropamide             == "No", "No", "Yes")),
  "glimepiride"              = as_factor(ifelse(glimepiride                == "No", "No", "Yes")),
  "acetohexamide"            = as_factor(ifelse(acetohexamide              == "No", "No", "Yes")),
  "glipizide"                = as_factor(ifelse(glipizide                  == "No", "No", "Yes")),
  "glyburide"                = as_factor(ifelse(glyburide                  == "No", "No", "Yes")),
  "tolbutamide"              = as_factor(ifelse(tolbutamide                == "No", "No", "Yes")),
  "pioglitazone"             = as_factor(ifelse(pioglitazone               == "No", "No", "Yes")),
  "rosiglitazone"            = as_factor(ifelse(rosiglitazone              == "No", "No", "Yes")),
  "acarbose"                 = as_factor(ifelse(acarbose                   == "No", "No", "Yes")),
  "miglitol"                 = as_factor(ifelse(miglitol                   == "No", "No", "Yes")),
  "troglitazone"             = as_factor(ifelse(troglitazone               == "No", "No", "Yes")),
  "tolazamide"               = as_factor(ifelse(tolazamide                 == "No", "No", "Yes")),
  "insulin"                  = as_factor(ifelse(insulin                    == "No", "No", "Yes")),
  "glyburide-metformin"      = as_factor(ifelse(`glyburide-metformin`      == "No", "No", "Yes")),
  "glipizide-metformin"      = as_factor(ifelse(`glipizide-metformin`      == "No", "No", "Yes")),
  "glimepiride-pioglitazone" = as_factor(ifelse(`glimepiride-pioglitazone` == "No", "No", "Yes")),
  "metformin-pioglitazone"   = as_factor(ifelse(`metformin-pioglitazone`   == "No", "No", "Yes")),
  "readmitted"               = ifelse(readmitted == "NO", 0, 1)
) %>% select(-max_glu_serum, -A1Cresult)

# Administrative data
diab_A <- 
  diab %>% 
  drop_na() %>% 
  select(
    race, gender, age, time_in_hospital, num_lab_procedures, num_procedures, 
    num_medications, number_outpatient, number_emergency, number_inpatient, 
    number_diagnoses, diabetesMed, change, glu_serum_positive, A1C_positive,
    readmitted
  )

# Medication
diab_B <- 
  diab %>% 
  drop_na() %>% 
  select(
    metformin, 
    repaglinide, 
    nateglinide, 
    chlorpropamide, 
    glimepiride, 
    #acetohexamide, 
    glipizide, 
    glyburide, 
    #tolbutamide, 
    pioglitazone, 
    rosiglitazone, 
    acarbose, 
    miglitol, 
    #troglitazone, 
    #tolazamide, 
    insulin, 
    `glyburide-metformin`, 
    `glipizide-metformin`, 
    #`glimepiride-pioglitazone`, 
    #`metformin-pioglitazone`,
    readmitted
  )

# save data
saveRDS(diab_A, file = "data/Diabetes/diab_administration.rds")
saveRDS(diab_B, file = "data/Diabetes/diab_medication.rds")
