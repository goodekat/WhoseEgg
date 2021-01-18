
# Shiny Carp App

Repository to store the code associated with the random forest app for
predicting fish species

## To do list for app

  - Improve on input of data:
      - Switch to using an Excel spreadsheet
      - Reorder variables in data set and input options by category
        (group as: month to conductivity, qualitative egg
        characteristics, numeric egg characteristics)
      - Change ACGC to invasive carp in data
      - Change to date (instead of month and Julian day)
      - Add download button for prepared data
      - Change to computing membrane CV, yolk CV, and ratio
      - Add column for egg ID
      - Add checks to app and/or Excel
      - Allow options for multiple qualitative levels
      - Make a few versions of the app
      - Option to leave the rows blank where we fill them in
      - Look into research on data input in apps
      - Add option for input of individual membrane and yolk
        measurements
      - Could I create an option to input one predictor at a time with
        arrow buttons
  - Update to work for more than one prediction at a time
  - Add ability to download the resulting predictions
  - Create info page
  - Figure out how to compute prediction intervals
  - Return a warning if input variables are outside of training data
  - Add visualizations of new data compared to training data (to
    determine if their observations fall within reasonable ranges to
    avoid extrapolation)
