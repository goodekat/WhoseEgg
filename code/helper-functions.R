# 
# example_vars <-
#   data.frame(
#     Month = 4,
#     Julian_Day = 120,
#     Temperature = 16,
#     Conductivity = 435,
#     Larval_Length = 0,
#     Membrane_Ave = 1.393,
#     Membrane_SD = 0.12109,
#     Membrane_CV = 0.08692,
#     Yolk_to_Membrane_Ratio = 0.57986,
#     Yolk_Ave = 0.80777,
#     Yolk_SD = 0.1685,
#     Yolk_CV = 0.2086,
#     Egg_Stage = 1,
#     Compact_Diffuse = "Compact",
#     Pigment = "Yes",
#     Sticky_Debris = "No",
#     Deflated = "No"
#   )

#write.csv(example_vars, file = "data/example.csv", row.names = FALSE)

# 
# rf_inputs <- 
#   example_vars %>%
#   adjust_variable_types() %>%
#   adjust_factor_levels()
# 
# data.frame(predict(rfs$Family_ACGC, rf_inputs))

# Function for putting input values into a data frame
inputs_to_df <- function(input) {
  data.frame(
    "Month" = input$Month,
    "Julian_Day" = input$Julian_Day,
    "Temperature" = input$Temperature,
    "Conductivity" = input$Conductivity,
    "Membrane_Ave" = input$Membrane_Ave,
    "Membrane_SD" = input$Membrane_SD,
    "Membrane_CV" = input$Membrane_CV,
    "Yolk_Ave" = input$Yolk_Ave,
    "Yolk_SD" = input$Yolk_SD,
    "Yolk_CV" = input$Yolk_CV,
    "Yolk_to_Membrane_Ratio" = input$Yolk_to_Membrane_Ratio,
    "Larval_Length" = input$Larval_Length,
    "Deflated" = input$Deflated,
    "Pigment" = input$Pigment,
    "Egg_Stage" = input$Egg_Stage,
    "Compact_Diffuse" = input$Compact_Diffuse,
    "Sticky_Debris" = input$Sticky_Debris
  )
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
        "Yolk_CV",
        "Egg_Stage"
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

