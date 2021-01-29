
# Shiny Carp App

Repository to store the code associated with the random forest app for
predicting fish species

## Changes made

  - Switch all text to egg characteristics to predictor variables
  - Move all random forest code to R script
  - Change ACGC to invasive carp in data
  - Removed manual input option (temporarily)
  - Changed format of prediction page

## To do list for app

  - Egg characteristics page:
      - **Add option to download template**
      - **Add warning about missing observations**
      - **Add checks to app and/or Excel**
      - **Allow options for multiple qualitative levels**
      - **Update instructional text**
      - Option to leave the rows blank where we fill them in
      - Look into research on data input in apps
      - Return a warning if input variables are outside of training data
      - Add visualizations of new data compared to training data (to
        determine if their observations fall within reasonable ranges to
        avoid extrapolation)
  - Prediction page:
      - **Order the levels in the plot by highest to lowest
        probabilities**
      - **Update to work for more than one prediction at a time**
      - **Add tabs for different plots**
      - Figure out how to compute prediction intervals
  - Download page:
      - **Add ability to download the prepared data and resulting
        predictions**
  - Info page:
      - Write text
  - Other:
      - Add image during loading of swimming fish
