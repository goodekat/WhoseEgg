## This file contains helper functions used by the WhoseEgg R Shiny app
## Created by Katherine Goode
## Last Updated: May 7, 2021

#### Variable Vectors ####

# Vector of Egg ID plus predictor variables
rf_pred_vars_plus_id <-
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
    "Embryo_Ave",
    "Embryo_SD",
    "Embryo_CV",
    "Embryo_to_Membrane_Ratio",
    "Larval_Length"
  )

#### Data Manipulation #### 

# Function for computing variables based on given input values
compute_variables <- function(df) {
  df %>% 
    mutate(Date = lubridate::make_date(Year, Month, Day)) %>%
    mutate(Julian_Day = lubridate::yday(Date),
           Membrane_CV = Membrane_SD / Membrane_Ave,
           Embryo_CV = Embryo_SD / Embryo_Ave, 
           Embryo_to_Membrane_Ratio = 1 - (Embryo_Ave / Membrane_Ave)) %>%
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
        "Embryo_to_Membrane_Ratio",
        "Embryo_Ave",
        "Embryo_SD",
        "Embryo_CV"
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
      "Embryo_Ave",
      "Embryo_SD",
      "Embryo_CV",
      "Embryo_to_Membrane_Ratio",
      "Larval_Length",
      everything()
    )
}

#### Random Forest Predictions #### 

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
  max_n <- max(plot_data$n)
  plot_data %>%
    ggplot(aes(x = order, y = n, label = n)) + 
    geom_bar(stat = "identity", fill = "grey75") +
    geom_text(aes(y = max_n), nudge_y = max_n * 0.1, vjust = 0) +
    facet_wrap(. ~ taxa, scales = "free_y") + 
    coord_flip() + 
    scale_x_discrete(
      breaks = plot_data$order,
      labels = plot_data$pred,
      expand = c(0,0)
    ) +
    ylim(c(0, max_n + (max_n * 0.2))) +
    theme_bw(base_size = 16) + 
    theme(
      axis.title.y = element_blank(),
      strip.background = element_rect(fill = "white", color = "white")
    ) +
    labs(y = "Number of eggs")
  
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
    geom_text(aes(x = 1), hjust = 0, nudge_x = 0.01) +
    facet_wrap(. ~ taxa, nrow = 1, scales = "free_y") + 
    scale_y_discrete(
      breaks = plot_data$order,
      labels = plot_data$level,
      expand = c(0,0)
    ) +
    scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.2), limits = c(0, 1.2)) +
    theme_bw(base_size = 16) +
    theme(
      axis.title.y = element_blank(),
      strip.background = element_rect(fill = "white", color = "white")
    ) + 
    labs(x = "Random forest probability")
  
}

#### Checks #### 

# Function for making sure that all necessary variables have been input
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
      "Embryo_Ave",
      "Embryo_SD",
      "Larval_Length"
    )
  return(sum(necessary_vars %in% names(df)) == 16)
}

# Check to make sure all eggs have an ID
check_for_egg_ids <- function(df) {
  return(sum(is.na(df$Egg_ID)) == 0)
}

# Function that checks for the correct factor levels
check_fct_levels <- function(df) {
  
  # Create vectors of required factor levels
  es_levels = c("1", "2", "3", "4", "5", "6", "7", "8", "BROKEN", "D")
  cd_levels = c("C", "D", "c", "d", "Compact", "Diffuse", "Compact", "Diffuse")
  yn_levels = c("N", "Y", "n", "y", "No", "Yes", "no", "yes")
  mn_levels = 1:12
  dy_levels = 1:365
  
  # Identify the wrong levels
  es_wrong = !(unique(na.omit(df$Egg_Stage)) %in% es_levels)
  cd_wrong = !(unique(na.omit(df$Compact_Diffuse)) %in% cd_levels)
  pg_wrong = !(unique(na.omit(df$Pigment)) %in% yn_levels)
  sd_wrong = !(unique(na.omit(df$Sticky_Debris)) %in% yn_levels)
  df_wrong = !(unique(na.omit(df$Deflated)) %in% yn_levels)
  mn_wrong = !(unique(na.omit(df$Month)) %in% mn_levels)
  dy_wrong = !(unique(na.omit(df$Day)) %in% dy_levels)
  
  # Return TRUE if all levels are correct/acceptable
  sum(c(es_wrong, cd_wrong, pg_wrong, sd_wrong, df_wrong, mn_wrong, dy_wrong)) == 0
  
}

# Function that checks if the numeric variables are within the ranges
# of the training data
check_var_ranges <- function(df, eggdata) {
  
  checks <- c(
    # Identify any values below the observed min
    min(df$Conductivity, na.rm = TRUE) < min(eggdata$Conductivity, na.rm = TRUE),
    min(df$Temperature, na.rm = TRUE) < min(eggdata$Temperature, na.rm = TRUE),
    min(df$Membrane_Ave, na.rm = TRUE) < min(eggdata$Membrane_Ave, na.rm = TRUE),
    min(df$Membrane_SD, na.rm = TRUE) < min(eggdata$Membrane_SD, na.rm = TRUE),
    min(df$Embryo_Ave, na.rm = TRUE) < min(eggdata$Embryo_Ave, na.rm = TRUE),
    min(df$Embryo_SD, na.rm = TRUE) < min(eggdata$Embryo_SD, na.rm = TRUE),
    min(df$Larval_Length, na.rm = TRUE) < min(eggdata$Larval_Length, na.rm = TRUE),
    
    # Identify any values above the observed max
    max(df$Conductivity, na.rm = TRUE) > max(eggdata$Conductivity, na.rm = TRUE),
    max(df$Temperature, na.rm = TRUE) > max(eggdata$Temperature, na.rm = TRUE),
    max(df$Membrane_Ave, na.rm = TRUE) > max(eggdata$Membrane_Ave, na.rm = TRUE),
    max(df$Membrane_SD, na.rm = TRUE) > max(eggdata$Membrane_SD, na.rm = TRUE),
    max(df$Embryo_Ave, na.rm = TRUE) > max(eggdata$Embryo_Ave, na.rm = TRUE),
    max(df$Embryo_SD, na.rm = TRUE) > max(eggdata$Embryo_SD, na.rm = TRUE),
    max(df$Larval_Length, na.rm = TRUE) > max(eggdata$Larval_Length, na.rm = TRUE),
    max(df$Julian_Day, na.rm = TRUE) > max(eggdata$Julian_Day, na.rm = TRUE),
    
    # Identify any months not in training data
    !(unique(na.omit(df$Month)) %in% unique(eggdata$Month))
  )
  
  # Return TRUE if all variables fall within the training data ranges
  return(sum(checks) == 0)
  
}

# Function that checks if it is possible to compute Julian day
check_dates <- function(df) {
  
  # Determine if any NAs are computed when applying lubridate
  date_computed_nas <- 
    df %>% 
    mutate(Date = lubridate::make_date(Year, Month, Day)) %>%
    select(Date) %>% 
    is.na()
  
  # Return TRUE if all levels are correct/acceptable
  return(sum(date_computed_nas) == 0)
  
}

# Function that checks if all dates are in the past
check_for_historical_dates <- function(df) {
  
  # Determine if any Dates are in the future
  future_dates <-
    df %>%
    mutate(Date = lubridate::make_date(Year, Month, Day),
           Current_Date = Sys.Date()) %>%
    select(Egg_ID, Date, Current_Date) %>%
    filter(Date > Current_Date)
  
  # Return TRUE if all levels are correct/acceptable
  return(length(future_dates$Egg_ID) == 0)
  
}

#### Extract Problem Observations #### 

# Function that extracts the necessary input variables that are missing 
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
      "Embryo_Ave",
      "Embryo_SD",
      "Larval_Length"
    )
  return(necessary_vars[!(necessary_vars %in% names(df))])
}

# Function that extracts the levels in factor variables that are not allowed
get_wrong_fct_levels <- function(df) {
  
  # Create vectors of required factor levels
  es_levels = c("1", "2", "3", "4", "5", "6", "7", "8", "BROKEN", "D")
  cd_levels = c("C", "D", "c", "d", "Compact", "Diffuse", "Compact", "Diffuse")
  yn_levels = c("N", "Y", "n", "y", "No", "Yes", "no", "yes")
  yr_levels = 1000:9999
  mn_levels = 1:12
  dy_levels = 1:31
  
  # Get the unique levels from each factor (excluding NAs)
  es_levels_obs = na.omit(unique(df$Egg_Stage))
  cd_levels_obs = na.omit(unique(unique(df$Compact_Diffuse)))
  pg_levels_obs = na.omit(unique(unique(df$Pigment)))
  sd_levels_obs = na.omit(unique(unique(df$Sticky_Debris)))
  df_levels_obs = na.omit(unique(unique(df$Deflated)))
  yr_levels_obs = na.omit(unique(unique(df$Year)))
  mn_levels_obs = na.omit(unique(unique(df$Month)))
  dy_levels_obs = na.omit(unique(unique(df$Day)))
  
  # Identify the wrong levels
  es_wrong = !(es_levels_obs %in% es_levels)
  cd_wrong = !(cd_levels_obs %in% cd_levels)
  pg_wrong = !(pg_levels_obs %in% yn_levels)
  sd_wrong = !(sd_levels_obs %in% yn_levels)
  df_wrong = !(df_levels_obs %in% yn_levels)
  yr_wrong = !(yr_levels_obs %in% yr_levels)
  mn_wrong = !(mn_levels_obs %in% mn_levels)
  dy_wrong = !(dy_levels_obs %in% dy_levels)
  
  # Create a vector with the factors and their wrong levels
  wrong_levels <-
    c(
      ifelse(TRUE %in% es_wrong, paste("Egg_Stage:", es_levels_obs[es_wrong]), NA),
      ifelse(TRUE %in% cd_wrong, paste("Compact_Diffuse:", cd_levels_obs[cd_wrong]), NA),
      ifelse(TRUE %in% pg_wrong, paste("Pigment:", pg_levels_obs[pg_wrong]), NA),
      ifelse(TRUE %in% sd_wrong, paste("Sticky_Debris:", sd_levels_obs[sd_wrong]), NA),
      ifelse(TRUE %in% df_wrong, paste("Deflated:", df_levels_obs[df_wrong]), NA),
      ifelse(TRUE %in% yr_wrong, paste("Year:", yr_levels_obs[yr_wrong]), NA),
      ifelse(TRUE %in% mn_wrong, paste("Month:", mn_levels_obs[mn_wrong]), NA),
      ifelse(TRUE %in% dy_wrong, paste("Day:", dy_levels_obs[dy_wrong]), NA)
    )
  
  # Return the factors with wrong levels and the corresponding wrong levels
  return(wrong_levels[!is.na(wrong_levels)])
  
}

# Function that extracts the variables have have observations outside 
# the range of the training data variables
get_vars_outside_ranges <- function(df, eggdata) {
  
  # Determine which variables have values outside of training data ranges
  cond_check = df$Conductivity < min(eggdata$Conductivity) | df$Conductivity > max(eggdata$Conductivity)
  temp_check = df$Temperature < min(eggdata$Temperature) | df$Temperature > max(eggdata$Temperature)
  meav_check = df$Membrane_Ave < min(eggdata$Membrane_Ave) | df$Membrane_Ave > max(eggdata$Membrane_Ave)
  mesd_check = df$Membrane_SD < min(eggdata$Membrane_SD) | df$Membrane_SD > max(eggdata$Membrane_SD)
  ykav_check = df$Embryo_Ave < min(eggdata$Embryo_Ave) | df$Embryo_Ave > max(eggdata$Embryo_Ave)
  yksd_check = df$Embryo_SD < min(eggdata$Embryo_SD) | df$Embryo_SD > max(eggdata$Embryo_SD)
  lvln_check = df$Larval_Length < min(eggdata$Larval_Length) | df$Larval_Length > max(eggdata$Larval_Length)
  mnth_check = !(df$Month %in% unique(eggdata$Month))
  judy_check = df$Julian_Day < min(eggdata$Julian_Day) | df$Julian_Day > max(eggdata$Julian_Day)
  
  # Specify the ranges of training data variables
  cond_range = paste0("Conductivity (", min(eggdata$Conductivity), " to ", max(eggdata$Conductivity), " microS/cm)")
  temp_range = paste0("Temperature (", min(eggdata$Temperature), " to ", max(eggdata$Temperature), " C)")
  meav_range = paste0("Membrane_Ave (", min(eggdata$Membrane_Ave), " to ", max(eggdata$Membrane_Ave), " mm)")
  mesd_range = paste0("Membrane_SD (", min(eggdata$Membrane_SD), " to ", max(eggdata$Membrane_SD), " mm)")
  ykav_range = paste0("Embryo_Ave (", min(eggdata$Embryo_Ave), " to ", max(eggdata$Embryo_Ave), " mm)")
  yksd_range = paste0("Embryo_SD (", min(eggdata$Embryo_SD), " to ", max(eggdata$Embryo_SD), " mm)")
  lvln_range = paste0("Larval_Length (", min(eggdata$Larval_Length), " to ", max(eggdata$Larval_Length), " mm)")
  mnth_range = paste0("Month (", min(eggdata$Month), " to ", max(eggdata$Month), ")")
  judy_range = paste0("Julian_Day (", min(eggdata$Julian_Day), " to ", max(eggdata$Julian_Day), ")")
  
  # Create a vector with the variables and their wrong levels
  messages <-
    c(
      ifelse(TRUE %in% cond_check, cond_range, NA),
      ifelse(TRUE %in% temp_check, temp_range, NA),
      ifelse(TRUE %in% meav_check, meav_range, NA),
      ifelse(TRUE %in% mesd_check, mesd_range, NA),
      ifelse(TRUE %in% ykav_check, ykav_range, NA),
      ifelse(TRUE %in% yksd_check, yksd_range, NA),
      ifelse(TRUE %in% lvln_check, lvln_range, NA),
      ifelse(TRUE %in% mnth_check, mnth_range, NA),
      ifelse(TRUE %in% judy_check, judy_range, NA)
    )
  
  # Return the factors with wrong levels and the corresponding wrong levels
  return(messages[!is.na(messages)])
  
}

# Function that extracts the egg IDs with variables that have values 
# outside the range of the training data
get_obs_outside_var_ranges <- function(df, eggdata) {
  
  # Determine which variables have values outside of training data ranges
  cond_check = df$Conductivity < min(eggdata$Conductivity) | df$Conductivity > max(eggdata$Conductivity)
  temp_check = df$Temperature < min(eggdata$Temperature) | df$Temperature > max(eggdata$Temperature)
  meav_check = df$Membrane_Ave < min(eggdata$Membrane_Ave) | df$Membrane_Ave > max(eggdata$Membrane_Ave)
  mesd_check = df$Membrane_SD < min(eggdata$Membrane_SD) | df$Membrane_SD > max(eggdata$Membrane_SD)
  ykav_check = df$Embryo_Ave < min(eggdata$Embryo_Ave) | df$Embryo_Ave > max(eggdata$Embryo_Ave)
  yksd_check = df$Embryo_SD < min(eggdata$Embryo_SD) | df$Embryo_SD > max(eggdata$Embryo_SD)
  lvln_check = df$Larval_Length < min(eggdata$Larval_Length) | df$Larval_Length > max(eggdata$Larval_Length)
  mnth_check = !(df$Month %in% unique(eggdata$Month))
  judy_check = df$Julian_Day < min(eggdata$Julian_Day) | df$Julian_Day > max(eggdata$Julian_Day)
  
  # Create a vector with the variables and their wrong levels
  ids_outside <-
    c(df$Egg_ID[cond_check],
      df$Egg_ID[temp_check],
      df$Egg_ID[meav_check], 
      df$Egg_ID[mesd_check],
      df$Egg_ID[ykav_check], 
      df$Egg_ID[yksd_check], 
      df$Egg_ID[lvln_check], 
      df$Egg_ID[mnth_check],
      df$Egg_ID[judy_check]
    ) %>% unique()
  
  # Return the egg IDs with observations outside ranges
  return(ids_outside)
  
}

# Function that extracts the egg IDs with missing values
get_missing_vals <- function(df) {
  
  # Identify rows in data with NAs
  rows_with_missing = rowSums(is.na(df)) > 0
  
  # Return eggs IDs with missing values
  df[rows_with_missing,]$Egg_ID
  
}

# Function that extracts the egg IDs where it is not possible 
# to compute Julian day
get_na_dates <- function(df){
  
  # Return Egg IDs where Date becomes NA
  df %>% 
    mutate(Date = lubridate::make_date(Year, Month, Day)) %>%
    filter(is.na(Date)) %>%
    pull(Egg_ID)
  
}

# Function that extracts the egg IDs where date is in the futures
get_any_future_dates <- function(df){
  
  # Return Egg IDs where Date is in the future
  df %>% 
    mutate(
      Date = lubridate::make_date(Year, Month, Day),
      Current_Date = Sys.Date()
    ) %>%
    select(Egg_ID, Date, Current_Date) %>%
    filter(Date > Current_Date) %>% 
    pull(Egg_ID)
  
}
