# Source the helper functions used by the app
source("code/helper-functions.R")

# Load the random forest models (trained on the years of 2014-2016)
rfs <- readRDS("data/rfs_for_app.rds")

# Load the egg data
eggdata <- read.csv("../data/eggdata_for_app.csv")

# Input data
#input_data <- read.csv("data/example_data/ten_obs_extra_vars.csv")
#input_data <- readxl::read_xlsx("data/example_data/one_obs_min_vars.xlsx")
#input_data <- readxl::read_xlsx("data/example_data/ten_obs_wrong_level.xlsx")
#input_data <- read.csv("data/example_data/one_obs_missing_vars.csv")
input_data <- readxl::read_xlsx("data/example_data/ten_obs_missing_vals.xlsx")
#input_data <- read.csv("data/example_data/ten_obs_missing_vals.csv")
#input_data <- readxl::read_xlsx("data/example_data/ten_obs_obs_outside_ranges.xlsx")
#input_data <- readxl::read_xlsx("data/example_data/ten_obs_ranges_and_missing.xlsx")
check_for_vars(input_data)
get_missing_vars(input_data)
check_fct_levels(input_data)
get_wrong_fct_levels(input_data)
check_var_ranges(input_data)
get_outside_var_ranges(input_data)

# Process the inputs as needed for the random forest
processed_inputs <-
  input_data %>%
  compute_variables() %>%
  adjust_variable_types() %>%
  adjust_factor_levels() %>%
  sort_vars()

# Prepare the inputs for the random forest
inputs_clean <- processed_inputs %>% select(all_of(rf_pred_vars))

# Get the predictions and random forest probabilities
pred_list <-
  list(
    family_pred  = as.character(predict(rfs$Family_ACGC, inputs_clean)),
    genus_pred   = as.character(predict(rfs$Genus_ACGC, inputs_clean)),
    species_pred = as.character(predict(rfs$Common_Name_ACGC, inputs_clean)),
    family_prob  = data.frame(predict(rfs$Family_ACGC, inputs_clean, type = "prob")),
    genus_prob   = data.frame(predict(rfs$Genus_ACGC, inputs_clean, type = "prob")),
    species_prob = data.frame(predict(rfs$Common_Name_ACGC, inputs_clean, type = "prob"))
  )

# Put the predictions in a data frame with the input values
data_and_preds <- 
  processed_inputs %>%
  mutate(
    Family_Pred = pred_list$family_pred,
    Family_Prob = get_rf_prob(pred_list, "family"),
    Genus_Pred   = pred_list$genus_pred,
    Genus_Prob = get_rf_prob(pred_list, "genus"),
    Species_Pred = pred_list$species_pred,
    Species_Prob = get_rf_prob(pred_list, "species")
  ) %>%
  bind_cols(
    pred_list$family_prob %>% rename_all(.funs = function(x) paste0("Family_Prob_", x)),
    pred_list$genus_prob %>% rename_all(.funs = function(x) paste0("Genus_Prob_", x)),
    pred_list$species_prob %>% rename_all(.funs = function(x) paste0("Species_Prob_", x))
  )

# Create plots summarizing the random forest predictions
rf_pred_plot(data_and_preds)

# Create plots with the random forest probabilities for all taxonomic levels
rf_prob_plot(data_and_preds, 1)
