
# Function for computing variables based on given input values
compute_variables <- function(df) {
  df %>% 
    # Compute Julian day
    mutate(Date = lubridate::make_date(Year, Month, Day)) %>%
    mutate(Julian_Day = lubridate::yday(Date),
           Membrane_CV = Membrane_SD / Membrane_Ave,
           Yolk_CV = Yolk_SD / Yolk_Ave, 
           Yolk_to_Membrane_Ratio = Yolk_Ave / Membrane_Ave) %>%
    select(-Date)
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
    # Replace blank cells with NA
    mutate_at(
      .vars = c(
        "Egg_Stage",
        "Compact_Diffuse",
        "Pigment",
        "Sticky_Debris",
        "Deflated"
      ),
      .funs = function(x) ifelse(x == "", NA, x)
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
  
  # Create vectors of required factor levels
  es_levels = c("1", "2", "3", "4", "5", "6", "7", "8", "BROKEN", "D")
  cd_levels = c("C", "D")
  yn_levels = c("N", "Y")
  
  # Adjust levels as appropriate for each factor
  
  # Egg stage
  if ("Broken" %in% levels(df$Egg_Stage)) {
    df <- df %>% mutate(Egg_Stage = fct_recode(Egg_Stage, "BROKEN" = "Broken"))
  }
  if ("broken" %in% levels(df$Egg_Stage)) {
    df <- df %>% mutate(Egg_Stage = fct_recode(Egg_Stage, "BROKEN" = "broken"))
  }
  if ("Diffuse" %in% levels(df$Egg_Stage)) {
    df <- df %>% mutate(Egg_Stage = fct_recode(Egg_Stage, "D" = "Diffuse"))
  }
  if ("diffuse" %in% levels(df$Egg_Stage)) {
    df <- df %>% mutate(Egg_Stage = fct_recode(Egg_Stage, "D" = "diffuse"))
  }
  if ("d" %in% levels(df$Egg_Stage)) {
    df <- df %>% mutate(Egg_Stage = fct_recode(Egg_Stage, "D" = "d"))
  }
  df$Egg_Stage <- factor(df$Egg_Stage,levels = es_levels)
  
  # Compact diffuse
  if ("Compact" %in% levels(df$Compact_Diffuse)) {
    df <- df %>% mutate(Compact_Diffuse = fct_recode(Compact_Diffuse, "C" = "Compact"))
  }
  if ("compact" %in% levels(df$Compact_Diffuse)) {
    df <- df %>% mutate(Compact_Diffuse = fct_recode(Compact_Diffuse, "C" = "compact"))
  }
  if ("c" %in% levels(df$Compact_Diffuse)) {
    df <- df %>% mutate(Compact_Diffuse = fct_recode(Compact_Diffuse, "C" = "c"))
  }
  if ("Diffuse" %in% levels(df$Compact_Diffuse)) {
    df <- df %>% mutate(Compact_Diffuse = fct_recode(Compact_Diffuse, "D" = "Diffuse"))
  }
  if ("diffuse" %in% levels(df$Compact_Diffuse)) {
    df <- df %>% mutate(Compact_Diffuse = fct_recode(Compact_Diffuse, "D" = "diffuse"))
  }
  if ("d" %in% levels(df$Compact_Diffuse)) {
    df <- df %>% mutate(Compact_Diffuse = fct_recode(Compact_Diffuse, "D" = "d"))
  }
  df$Compact_Diffuse <- factor(df$Compact_Diffuse, levels = cd_levels)
  
  # Pigment
  if ("Yes" %in% levels(df$Pigment)) {
    df <- df %>% mutate(Pigment = fct_recode(Pigment, "Y" = "Yes"))
  }
  if ("yes" %in% levels(df$Pigment)) {
    df <- df %>% mutate(Pigment = fct_recode(Pigment, "Y" = "yes"))
  }
  if ("y" %in% levels(df$Pigment)) {
    df <- df %>% mutate(Pigment = fct_recode(Pigment, "Y" = "y"))
  }
  if ("No" %in% levels(df$Pigment)) {
    df <- df %>% mutate(Pigment = fct_recode(Pigment, "N" = "No"))
  }
  if ("no" %in% levels(df$Pigment)) {
    df <- df %>% mutate(Pigment = fct_recode(Pigment, "N" = "np"))
  }
  if ("n" %in% levels(df$Pigment)) {
    df <- df %>% mutate(Pigment = fct_recode(Pigment, "N" = "n"))
  }
  df$Pigment <- factor(df$Pigment, levels = yn_levels)
  
  # Sticky debris
  if ("Yes" %in% levels(df$Sticky_Debris)) {
    df <- df %>% mutate(Sticky_Debris = fct_recode(Sticky_Debris, "Y" = "Yes"))
  }
  if ("yes" %in% levels(df$Sticky_Debris)) {
    df <- df %>% mutate(Sticky_Debris = fct_recode(Sticky_Debris, "Y" = "yes"))
  }
  if ("y" %in% levels(df$Sticky_Debris)) {
    df <- df %>% mutate(Sticky_Debris = fct_recode(Sticky_Debris, "Y" = "y"))
  }
  if ("No" %in% levels(df$Sticky_Debris)) {
    df <- df %>% mutate(Sticky_Debris = fct_recode(Sticky_Debris, "N" = "No"))
  }
  if ("no" %in% levels(df$Sticky_Debris)) {
    df <- df %>% mutate(Sticky_Debris = fct_recode(Sticky_Debris, "N" = "no"))
  }
  if ("n" %in% levels(df$Sticky_Debris)) {
    df <- df %>% mutate(Sticky_Debris = fct_recode(Sticky_Debris, "N" = "n"))
  }
  df$Sticky_Debris <- factor(df$Sticky_Debris, levels = yn_levels)
  
  # Deflated
  if ("Yes" %in% levels(df$Deflated)) {
    df <- df %>% mutate(Deflated = fct_recode(Deflated, "Y" = "Yes"))
  }
  if ("yes" %in% levels(df$Deflated)) {
    df <- df %>% mutate(Deflated = fct_recode(Deflated, "Y" = "yes"))
  }
  if ("y" %in% levels(df$Deflated)) {
    df <- df %>% mutate(Deflated = fct_recode(Deflated, "Y" = "y"))
  }
  if ("No" %in% levels(df$Deflated)) {
    df <- df %>% mutate(Deflated = fct_recode(Deflated, "N" = "No"))
  }
  if ("no" %in% levels(df$Deflated)) {
    df <- df %>% mutate(Deflated = fct_recode(Deflated, "N" = "no"))
  }
  if ("n" %in% levels(df$Deflated)) {
    df <- df %>% mutate(Deflated = fct_recode(Deflated, "N" = "n"))
  }
  df$Deflated <- factor(df$Deflated, levels = yn_levels)
  
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
  
  # Prepare the data for the plot
  plot_data <-
    data.frame(
      "Family" = rf_results$Family_Pred,
      "Genus" = rf_results$Genus_Pred,
      "Species" = rf_results$Species_Pred
    ) %>%
    pivot_longer(names_to = "taxa",
                 values_to = "pred",
                 cols = everything()) %>%
    count(taxa, pred) %>%
    arrange(taxa, n) %>%
    mutate(order = factor(row_number()))
  
  # Create the plot
  plot_data %>%
    ggplot(aes(x = order, y = n, label = n)) + 
    geom_bar(stat = "identity", fill = "grey75") +
    geom_text(aes(y = 0), nudge_y = 0.5) +
    facet_wrap(. ~ taxa, scales = "free_y") + 
    coord_flip() + 
    scale_x_discrete(
      breaks = plot_data$order,
      labels = plot_data$pred,
      expand = c(0,0)
    ) +
    theme_bw() + 
    theme(
      axis.title.y = element_blank(),
      strip.background = element_rect(fill = "white", color = "white")
    ) +
    labs(
      y = "Total number of eggs",
      title = "Frequency of taxonomic level predictions"
    )
  
}

# Function to create plot of random forest probabilities for an individual egg
rf_prob_plot <- function(rf_results, idx) {
  
  # Prepare the plot data
  plot_data <- 
    rf_results %>% 
    slice(idx) %>%
    select(contains("_prob_")) %>% 
    pivot_longer(
      names_to = "level",
      values_to = "prob",
      cols = everything()
    ) %>%
    separate(level, c("taxa", "nada", "level"), "_") %>%
    select(-nada) %>%
    mutate(level = stringr::str_replace(level, "\\.", " ")) %>%
    arrange(taxa, prob) %>%
    mutate(order = factor(row_number()))
  
  # Create the plots
  plot_data %>%
    ggplot(aes(x = prob, y = order, label = prob)) + 
    geom_col(fill = "grey75") + 
    geom_text(aes(x = 0), nudge_x = 0.1) +
    facet_wrap(. ~ taxa, nrow = 1, scales = "free_y") + 
    scale_y_discrete(
      breaks = plot_data$order,
      labels = plot_data$level,
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

rf_pred_vars <-
  c(
    "Egg_ID",
    "Month",
    "Julian_Day",
    "Temperature",
    "Conductivity",
    "Deflated",
    "Pigment",
    "Egg_Stage",
    "Compact_Diffuse",
    "Sticky_Debris",
    "Membrane_Ave",
    "Membrane_SD",
    "Membrane_CV",
    "Yolk_Ave",
    "Yolk_SD",
    "Yolk_CV",
    "Yolk_to_Membrane_Ratio",
    "Larval_Length"
  )

# Function to sort variables
sort_vars <- function(df) {
  df %>%
    select(
      "Egg_ID",
      "Year",
      "Month",
      "Day",
      "Julian_Day",
      "Temperature",
      "Conductivity",
      "Deflated",
      "Pigment",
      "Egg_Stage",
      "Compact_Diffuse",
      "Sticky_Debris",
      "Membrane_Ave",
      "Membrane_SD",
      "Membrane_CV",
      "Yolk_Ave",
      "Yolk_SD",
      "Yolk_CV",
      "Yolk_to_Membrane_Ratio",
      "Larval_Length",
      everything()
    )
}

check_for_vars <- function(df) {
  necessary_vars <- 
    c(
      "Egg_ID",
      "Year",
      "Month",
      "Day",
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
  return(sum(necessary_vars %in% names(df)) == 16)
}

# Function for checking for the correct factor levels
check_fct_levels <- function(df) {
  
  # Create vectors of required factor levels
  es_levels = c("1", "2", "3", "4", "5", "6", "7", "8", "BROKEN", "D")
  cd_levels = c("C", "D", "c", "d", "Compact", "Diffuse", "Compact", "Diffuse")
  yn_levels = c("N", "Y", "n", "y", "No", "Yes", "no", "yes")
  
  # Identify the wrong levels
  es_wrong = !(unique(na.omit(df$Egg_Stage)) %in% es_levels)
  cd_wrong = !(unique(na.omit(df$Compact_Diffuse)) %in% cd_levels)
  pg_wrong = !(unique(na.omit(df$Pigment)) %in% yn_levels)
  sd_wrong = !(unique(na.omit(df$Sticky_Debris)) %in% yn_levels)
  df_wrong = !(unique(na.omit(df$Deflated)) %in% yn_levels)
  
  # Return TRUE if all levels are correct/acceptable
  sum(c(es_wrong, cd_wrong, pg_wrong, sd_wrong, df_wrong)) == 0
  
}

get_missing_vars <- function(df) {
  necessary_vars <- 
    c(
      "Egg_ID",
      "Year",
      "Month",
      "Day",
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

get_wrong_fct_levels <- function(df) {
  
  # Create vectors of required factor levels
  es_levels = c("1", "2", "3", "4", "5", "6", "7", "8", "BROKEN", "D")
  cd_levels = c("C", "D", "c", "d", "Compact", "Diffuse", "Compact", "Diffuse")
  yn_levels = c("N", "Y", "n", "y", "No", "Yes", "no", "yes")
  
  # Get the unique levels from each factor (excluding NAs)
  es_levels_obs = na.omit(unique(df$Egg_Stage))
  cd_levels_obs = na.omit(unique(unique(df$Compact_Diffuse)))
  pg_levels_obs = na.omit(unique(unique(df$Pigment)))
  sd_levels_obs = na.omit(unique(unique(df$Sticky_Debris)))
  df_levels_obs = na.omit(unique(unique(df$Deflated)))
  
  # Identify the wrong levels
  es_wrong = !(es_levels_obs %in% es_levels)
  cd_wrong = !(cd_levels_obs %in% cd_levels)
  pg_wrong = !(pg_levels_obs %in% yn_levels)
  sd_wrong = !(sd_levels_obs %in% yn_levels)
  df_wrong = !(df_levels_obs %in% yn_levels)
  
  # Create a vector with the factors and their wrong levels
  wrong_levels <-
    c(
      ifelse(TRUE %in% es_wrong, paste("Egg_Stage:", es_levels_obs[es_wrong]), NA),
      ifelse(TRUE %in% cd_wrong, paste("Compact_Diffuse:", cd_levels_obs[cd_wrong]), NA),
      ifelse(TRUE %in% pg_wrong, paste("Pigment:", pg_levels_obs[pg_wrong]), NA),
      ifelse(TRUE %in% sd_wrong, paste("Sticky_Debris:", sd_levels_obs[sd_wrong]), NA),
      ifelse(TRUE %in% df_wrong, paste("Deflated:", df_levels_obs[df_wrong]), NA)
    )
  
  # Return the factors with wrong levels and the corresponding wrong levels
  return(wrong_levels[!is.na(wrong_levels)])
  
}
