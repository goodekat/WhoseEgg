
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

# Function to create bar plot summarizing random forest predictions 
rf_pred_plot <- function(rf_results, idx) {
  
  pred_plot_data <-
    data.frame(
      "Family" = RFpreds$family_pred,
      "Genus" = RFpreds$genus_pred,
      "Species" = RFpreds$species_pred
    ) %>%
    pivot_longer(names_to = "taxa",
                 values_to = "pred",
                 cols = everything()) %>%
    count(taxa, pred) %>%
    arrange(taxa, n) %>%
    mutate(order = factor(row_number()))
  
  pred_plot_data %>%
    ggplot(aes(x = order, y = n, label = n)) + 
    geom_bar(stat = "identity", fill = "grey50") +
    geom_text(aes(y = 0), nudge_y = 0.5) +
    facet_wrap(. ~ taxa, scales = "free_y") + 
    coord_flip() + 
    scale_x_discrete(
      breaks = pred_plot_data$order,
      labels = pred_plot_data$pred,
      expand = c(0,0)
    ) +
    theme_bw() + 
    theme(
      axis.title.y = element_blank(),
      strip.background = element_rect(fill = "white", color = "white")
    ) +
    labs(
      y = "Total number of eggs",
      title = "Taxonomic level predictions from random forests"
    )
  
}

# Function to create plot of random forest probabilities for 
# and individual egg
rf_prob_plot <- function(rf_results, idx) {
  
  results_joined <- 
    bind_rows(
      rf_results$family_prob[idx,] %>%
        pivot_longer(
          names_to = "level",
          values_to = "prob",
          cols = everything()
        ) %>%
        mutate(taxa = "Family"),
      rf_results$genus_prob[idx,] %>%
        pivot_longer(
          names_to = "level",
          values_to = "prob",
          cols = everything()
        ) %>%
        mutate(taxa = "Genus"),
      rf_results$species_prob[idx,] %>%
        pivot_longer(
          names_to = "level",
          values_to = "prob",
          cols = everything()
        ) %>%
        mutate(taxa = "Species")
    ) %>%
    select(taxa, level, prob) %>%
    mutate(level = stringr::str_replace(level, "\\.", " ")) %>%
    arrange(taxa, prob) %>%
    mutate(order = factor(row_number()))
  
  results_joined %>%
    ggplot(aes(x = prob, y = order, label = prob)) + 
    geom_col(fill = "grey50") + 
    geom_text(aes(x = 0), nudge_x = 0.1) +
    facet_wrap(. ~ taxa, nrow = 1, scales = "free_y") + 
    scale_y_discrete(
      breaks = results_joined$order,
      labels = results_joined$level,
      expand = c(0,0)
    ) +
    xlim(0, 1 + 0.05) +
    theme_bw() +
    theme(
      axis.title.y = element_blank(),
      strip.background = element_rect(fill = "white", color = "white")
    ) + 
    labs(x = "Random Forest Probability", 
         title = "Random forest probabilities for all taxonomic levels")
  
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
