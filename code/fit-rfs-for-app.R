library(purrr)

# Make a list of the response variables
vars_resp = c(
  "Genus",
  "Common_Name",
  "Family_ACGC",
  "Genus_ACGC",
  "Common_Name_ACGC"
)

# Make a vector of the predictor variables to be used in the models
vars_pred = c(
  "Month",
  "Julian_Day",
  "Temperature",
  "Conductivity",
  "Larval_Length",
  "Membrane_Ave",
  "Membrane_SD",
  "Membrane_CV",
  "Yolk_to_Membrane_Ratio",
  "Yolk_Ave",
  "Yolk_SD",
  "Yolk_CV",
  "Egg_Stage",
  "Compact_Diffuse",
  "Pigment",
  "Sticky_Debris",
  "Deflated"
)

# Function for fitting a random forest model given a response
# variable, predictor variables, and a dataset
fit_rf <- function(resp, preds, data) {
  
  # Fit the random forest
  set.seed(808)
  rf <- randomForest(
    data %>% pull(resp) ~ .,
    data = data %>% select(all_of(preds)),
    importance = T,
    ntree = 1000
  )
  
  # Create name for the random forest and put model in a named list
  rf_name = ifelse(length(preds) == 17, resp, paste0(resp, "_reduced"))
  rf_list = list(rf)
  names(rf_list) = rf_name
  
  # Return the named list
  return(rf_list)
  
}

# Models with all predictor variables
rfs_for_app_full = map(
  .x = vars_resp,
  .f = fit_rf,
  preds = vars_pred,
  data = eggdata_for_app
) %>% flatten()

# Model with reduced predictor variables
rf141516_reduced =
  fit_rf(resp = "Common_Name_ACGC",
         preds = vars_pred_reduced,
         data = eggdata141516)

# Join the models
rfs141516 = append(rfs141516_full, rf141516_reduced)

# ...and save them
saveRDS(rfs141516, rfs141516_file_path)
