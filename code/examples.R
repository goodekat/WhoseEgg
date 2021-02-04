#
# example_vars <-
#   data.frame(
#     Egg_ID = 1,
#     #Month = 4,
#     #Julian_Day = 120,
#     Date = "2020-01-12",
#     Temperature = 16,
#     Conductivity = 435,
#     Egg_Stage = 1,
#     Compact_Diffuse = "Compact",
#     Pigment = "Yes",
#     Sticky_Debris = "No",
#     Deflated = "No",
#     Larval_Length = 0,
#     Membrane_Ave = 1.393,
#     Membrane_SD = 0.12109,
#     #Membrane_CV = 0.08692,
#     #Yolk_to_Membrane_Ratio = 0.57986,
#     Yolk_Ave = 0.80777,
#     Yolk_SD = 0.1685#,
#     #Yolk_CV = 0.2086
#   )
# #
#example_vars <- readxl::read_excel("data/example_data/one_obs_min_vars.xlsx")

# Source the helper functions used by the app
source("code/helper-functions.R")

# Load the random forest models (trained on the years of 2014-2016)
rfs <- readRDS("data/rfs_for_app.rds")

# Input data
input_data <- read.csv("data/example_data/ten_obs_all_vars.csv")
check_for_vars(input_data)
get_missing_vars(input_data)

processed_inputs <-
  input_data %>%
  compute_variables() %>%
  adjust_variable_types() %>%
  adjust_factor_levels() %>%
  sort_vars()

# Prepare the inputs for the random forest
inputs_clean <- processed_inputs

# Get the predictions and random forest probabilities
get_rf_preds <-
  # Get the predictions and random forest probabilities
  list(
    family_pred  = as.character(predict(rfs$Family_ACGC, inputs_clean)),
    genus_pred   = as.character(predict(rfs$Genus_ACGC, inputs_clean)),
    species_pred = as.character(predict(rfs$Common_Name_ACGC, inputs_clean)),
    family_prob  = data.frame(predict(rfs$Family_ACGC, inputs_clean, type = "prob")),
    genus_prob   = data.frame(predict(rfs$Genus_ACGC, inputs_clean, type = "prob")),
    species_prob = data.frame(predict(rfs$Common_Name_ACGC, inputs_clean, type = "prob"))
  )

# Get the random forest predictions
RFpreds <- get_rf_preds

# Put the random forest results in a table
data.frame(
  'Egg ID' = processed_inputs$Egg_ID,
  "Family" = RFpreds$family_pred,
  "Family Probability" = get_rf_prob(RFpreds, "family"),
  "Family Pred Int" = "to do",
  "Genus" = RFpreds$genus_pred,
  "Genus Probability" = get_rf_prob(RFpreds, "genus"),
  "Genus Pred Int" = "to do",
  "Species" = RFpreds$species_pred,
  "Species Probability" = get_rf_prob(RFpreds, "species"),
  "Species Pred Int" = "to do",
  check.names = FALSE
)
