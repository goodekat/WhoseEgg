
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
  if (sum(levels(df$Egg_Stage) != es_levels) > 0) {
    if (!(sum(levels(df$Egg_Stage) %in% es_levels))) {
      stop ("Level in Egg_Stage that is not in training data.")
    }
    df$Egg_Stage <- factor(df$Egg_Stage,levels = es_levels)
  }
  
  # Compact diffuse
  if (sum(levels(df$Compact_Diffuse) != cd_levels) > 0) {
    if (!(sum(levels(df$Compact_Diffuse) %in% cd_levels))) {
      stop ("Level in Compact_Diffuse that is not in training data.")
    }
    df$Compact_Diffuse <- factor(df$Compact_Diffuse, levels = cd_levels)
  }
  
  # Pigment
  if (sum(levels(df$Pigment) != yn_levels) > 0) {
    if (!(sum(levels(df$Pigment) %in% yn_levels))) {
      stop ("Level in Pigment that is not in training data.")
    }
    df$Pigment <- factor(df$Pigment, levels = yn_levels)
  }
  
  # Sticky debris
  if (sum(levels(df$Sticky_Debris) != yn_levels) > 0) {
    if (!(sum(levels(df$Sticky_Debris) %in% yn_levels))) {
      stop ("Level in Sticky_Debris that is not in training data.")
    }
    df$Sticky_Debris <- factor(df$Sticky_Debris, levels = yn_levels)
  }
  
  # Deflated
  if (sum(levels(df$Deflated) != yn_levels) > 0) {
    if (!(sum(levels(df$Deflated) %in% yn_levels))) {
      stop ("Level in Deflated that is not in training data.")
    }
    df$Deflated <- factor(df$Deflated, levels = yn_levels)
  }
  
  # Return the data frame
  return(df) 
  
}

