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

# input_data <- read.csv("data/example_data/ten_obs_all_vars.csv")
# check_for_vars(input_data)
# get_missing_vars(input_data)
# 
# processed_inputs <-
#   input_data %>%
#   compute_variables() %>%
#   adjust_variable_types() %>%
#   adjust_factor_levels() %>%
#   sort_vars()
# 
# # Prepare the inputs for the random forest
# inputs_clean <- processed_inputs
# 
# # Get the predictions and random forest probabilities
# get_rf_preds <-
#   list(
#     family_pred  = as.character(predict(rfs$Family_ACGC, inputs_clean)),
#     genus_pred   = as.character(predict(rfs$Genus_ACGC, inputs_clean)),
#     species_pred = as.character(predict(rfs$Common_Name_ACGC, inputs_clean)),
#     family_prob  = data.frame(predict(rfs$Family_ACGC, inputs_clean, type = "prob")),
#     genus_prob   = data.frame(predict(rfs$Genus_ACGC, inputs_clean, type = "prob")),
#     species_prob = data.frame(predict(
#       rfs$Common_Name_ACGC, inputs_clean, type = "prob"
#     ))
#   )
# 
# # Get the random forest predictions
# RFpreds <- get_rf_preds
# 
# # Put the random forest results in a table
# data.frame(
#   'Egg ID' = processed_inputs$Egg_ID,
#   "Family" = RFpreds$family_pred,
#   "Family Probability" = get_rf_prob(RFpreds, "family"),
#   "Family Pred Int" = "to do",
#   "Genus" = RFpreds$genus_pred,
#   "Genus Probability" =  get_rf_prob(RFpreds, "genus"),
#   "Genus Pred Int" = "to do",
#   "Species" = RFpreds$species_pred,
#   "Species Probability" = get_rf_prob(RFpreds, "species"),
#   "Species Pred Int" = "to do",
#   check.names = FALSE
# )

# Function for putting input values into a data frame
inputs_to_df <- function(input) {
  data.frame(
    # Location variables
    "Egg_ID" = input$Egg_ID,
    "Date" = input$Date,
    "Temperature" = input$Temperature,
    "Conductivity" = input$Conductivity,
    # Categorical egg measurements
    "Deflated" = input$Deflated,
    "Pigment" = input$Pigment,
    "Egg_Stage" = input$Egg_Stage,
    "Compact_Diffuse" = input$Compact_Diffuse,
    "Sticky_Debris" = input$Sticky_Debris,
    # Quantitative egg measurements
    "Membrane_Ave" = input$Membrane_Ave,
    "Membrane_SD" = input$Membrane_SD,
    "Yolk_Ave" = input$Yolk_Ave,
    "Yolk_SD" = input$Yolk_SD,
    "Larval_Length" = input$Larval_Length
  )
}

# Function for computing variables based on given input values
compute_variables <- function(df) {
  df %>% 
    # Compute month and Julian day
    mutate(Date = lubridate::ymd(Date)) %>%
    mutate(Month = lubridate::month(Date), 
           Julian_Day = lubridate::yday(Date)) %>%
    select(-Date) %>%
    mutate(Membrane_CV = Membrane_SD / Membrane_Ave,
           Yolk_CV = Yolk_SD / Yolk_Ave, 
           Yolk_to_Membrane_Ratio = Yolk_Ave / Membrane_Ave)
}

# Function for specifying the variable types
adjust_variable_types <- function(df) {
  df %>%
    # Integer variables
    mutate_at(.vars = c("Month", "Julian_Day"), .funs = as.integer) %>%
    # Numeric variables
    mutate_at(
      .vars = c(
        "Temperature",
        "Conductivity",
        "Larval_Length",
        "Membrane_Ave",
        "Membrane_SD",
        "Membrane_CV",
        "Yolk_to_Membrane_Ratio",
        "Yolk_Ave",
        "Yolk_SD",
        "Yolk_CV"
      ),
      .funs = as.numeric
    ) %>%
    # Factor variables
    mutate_at(
      .vars = c(
        "Egg_Stage",
        "Compact_Diffuse",
        "Pigment",
        "Sticky_Debris",
        "Deflated"
      ),
      .funs = factor
    )
}

# Function for adjusting the factor levels of the data frame with the
# morphometric variables as needed
adjust_factor_levels <- function(df) {
  
  # Create factors with levels
  es_levels = c("1", "2", "3", "4", "5", "6", "7", "8", "BROKEN", "D")
  cd_levels = c("C", "D")
  yn_levels = c("N", "Y")
  
  # Check if the levels are correct and change if not (stop if a
  # level that is not in the training data is found)
  
  # Egg stage
  if ("Broken" %in% levels(df$Egg_Stage)) {
    df <- df %>% mutate(Egg_Stage = fct_recode(Egg_Stage, "BROKEN" = "Broken"))
  }
  if ("Diffuse" %in% levels(df$Egg_Stage)) {
    df <- df %>% mutate(Egg_Stage = fct_recode(Egg_Stage, "D" = "Diffuse"))
  }
  if (sum(levels(df$Egg_Stage) != es_levels) > 0) {
    if (!(sum(levels(df$Egg_Stage) %in% es_levels))) {
      stop ("Level in Egg_Stage that is not in training data.")
    }
    df$Egg_Stage <- factor(df$Egg_Stage,levels = es_levels)
  }
  
  # Compact diffuse
  if ("Compact" %in% levels(df$Compact_Diffuse)) {
    df <- df %>% mutate(Compact_Diffuse = fct_recode(Compact_Diffuse, "C" = "Compact"))
  }
  if ("Diffuse" %in% levels(df$Compact_Diffuse)) {
    df <- df %>% mutate(Compact_Diffuse = fct_recode(Compact_Diffuse, "D" = "Diffuse"))
  }
  if (sum(levels(df$Compact_Diffuse) != cd_levels) > 0) {
    if (!(sum(levels(df$Compact_Diffuse) %in% cd_levels))) {
      stop ("Level in Compact_Diffuse that is not in training data.")
    }
    df$Compact_Diffuse <- factor(df$Compact_Diffuse, levels = cd_levels)
  }
  
  # Pigment
  if ("Yes" %in% levels(df$Pigment)) {
    df <- df %>% mutate(Pigment = fct_recode(Pigment, "Y" = "Yes"))
  }
  if ("No" %in% levels(df$Pigment)) {
    df <- df %>% mutate(Pigment = fct_recode(Pigment, "N" = "No"))
  }
  if (sum(levels(df$Pigment) != yn_levels) > 0) {
    if (!(sum(levels(df$Pigment) %in% yn_levels))) {
      stop ("Level in Pigment that is not in training data.")
    }
    df$Pigment <- factor(df$Pigment, levels = yn_levels)
  }
  
  # Sticky debris
  if ("Yes" %in% levels(df$Sticky_Debris)) {
    df <- df %>% mutate(Sticky_Debris = fct_recode(Sticky_Debris, "Y" = "Yes"))
  }
  if ("No" %in% levels(df$Sticky_Debris)) {
    df <- df %>% mutate(Sticky_Debris = fct_recode(Sticky_Debris, "N" = "No"))
  }
  if (sum(levels(df$Sticky_Debris) != yn_levels) > 0) {
    if (!(sum(levels(df$Sticky_Debris) %in% yn_levels))) {
      stop ("Level in Sticky_Debris that is not in training data.")
    }
    df$Sticky_Debris <- factor(df$Sticky_Debris, levels = yn_levels)
  }
  
  # Deflated
  if ("Yes" %in% levels(df$Deflated)) {
    df <- df %>% mutate(Deflated = fct_recode(Deflated, "Y" = "Yes"))
  }
  if ("No" %in% levels(df$Deflated)) {
    df <- df %>% mutate(Deflated = fct_recode(Deflated, "N" = "No"))
  }
  if (sum(levels(df$Deflated) != yn_levels) > 0) {
    if (!(sum(levels(df$Deflated) %in% yn_levels))) {
      stop ("Level in Deflated that is not in training data.")
    }
    df$Deflated <- factor(df$Deflated, levels = yn_levels)
  }
  
  # Return the data frame
  return(df) 
  
}

# Function for extracting the RF probabilities for the maximum level
get_rf_prob <- function(rf_results, taxa) {
  n_eggs <- length(rf_results[[paste0(taxa, "_pred")]])
  map_dbl(
    .x = 1:n_eggs,
    .f = function(idx) {
      rf_results[[paste0(taxa, "_prob")]] %>%
        slice(idx) %>%
        pull(stringr::str_replace(rf_results[[paste0(taxa, "_pred")]][[idx]], " ", "\\."))
    }
  )
}

# Function to create bar plot of random forest probabilities
rf_prob_plot <- function(rf_results) {
  
  results_joined <- 
    bind_rows(
      rf_results$family_prob %>%
        pivot_longer(
          names_to = "level",
          values_to = "prob",
          cols = everything()
        ) %>%
        mutate(taxa = "Family"),
      rf_results$genus_prob %>%
        pivot_longer(
          names_to = "level",
          values_to = "prob",
          cols = everything()
        ) %>%
        mutate(taxa = "Genus"),
      rf_results$species_prob %>%
        pivot_longer(
          names_to = "level",
          values_to = "prob",
          cols = everything()
        ) %>%
        mutate(taxa = "Species")
    ) %>%
    select(taxa, level, prob) %>%
    mutate(level = stringr::str_replace(level, "\\.", " "))
  
  levels_ordered <- 
    results_joined %>%
    arrange(taxa, prob) %>%
    pull(level) %>%
    unique()
  
  results_joined %>%
    mutate(level = factor(level, levels = levels_ordered)) %>%
    ggplot(aes(x = prob, y = level, label = prob)) + 
    geom_col() + 
    geom_text(nudge_x = 0.05) +
    facet_wrap(. ~ taxa, nrow = 1, scales = "free_y") + 
    xlim(0, 1 + 0.05) +
    theme_bw() +
    theme(
      axis.title.y = element_blank(),
      strip.background = element_rect(fill = "white", color = "white")
    ) + 
    labs(x = "Random Forest Probability")
  
}

# Function to sort variables
sort_vars <- function(df) {
  df %>%
    select(
      Egg_ID,
      Month, 
      Julian_Day,
      Temperature,
      Conductivity,
      Deflated,
      Pigment,
      Egg_Stage,
      Compact_Diffuse,
      Sticky_Debris,
      Membrane_Ave,
      Membrane_SD,
      Membrane_CV,
      Yolk_Ave,
      Yolk_SD,
      Yolk_CV,
      Yolk_to_Membrane_Ratio,
      Larval_Length
    )
}

check_for_vars <- function(df) {
  necessary_vars <- 
    c(
      "Egg_ID",
      "Date",
      "Temperature",
      "Conductivity",
      "Deflated",
      "Pigment",
      "Egg_Stage",
      "Compact_Diffuse",
      "Sticky_Debris",
      "Membrane_Ave",
      "Membrane_SD",
      "Yolk_Ave",
      "Yolk_SD",
      "Larval_Length"
    )
  return(sum(necessary_vars %in% names(df)) == 14)
}

get_missing_vars <- function(df) {
  necessary_vars <- 
    c(
      "Egg_ID",
      "Date",
      "Temperature",
      "Conductivity",
      "Deflated",
      "Pigment",
      "Egg_Stage",
      "Compact_Diffuse",
      "Sticky_Debris",
      "Membrane_Ave",
      "Membrane_SD",
      "Yolk_Ave",
      "Yolk_SD",
      "Larval_Length"
    )
  return(necessary_vars[!(necessary_vars %in% names(df))])
}
