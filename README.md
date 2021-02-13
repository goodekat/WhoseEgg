
# Shiny Carp App

Repository to store the code associated with the random forest app for
predicting fish species

## Changes made

  - Prepared Excel template:
      - Included data validation
      - Added tab with units and such
      - Added drop down options for categorical variables
      - Changed to inputs of year, month, and day
  - Added warning about missing observations
  - Updated warnings about missing variables and wrong file type
  - Added comments saying that blank space will be filled in with
    predictions
  - Cleaned up “check” code
  - Allowed options for multiple qualitative levels in R (but not in
    template)
  - Returned a warning if input factors have incorrect levels
  - Return a warning if input variables are outside of training data
  - Added error is missing an egg id

## To do list for app

  - Overview page:
    
      - Write text providing an overview of how to use the app
      - Add picture of fish
      - Create and add video showing how to use the app

  - Egg characteristics page:
    
      - **Update instructional text**
      - Add visualizations of new data compared to training data (to
        determine if their observations fall within reasonable ranges to
        avoid extrapolation)
      - Look into research on data input in apps

  - Prediction page:
    
      - Figure out how to compute prediction intervals

  - Download page:

  - Help page
    
      - Write variable definitions
      - Add figures from Carlos

  - Other:
    
      - **Start writing the manuscript**
      - Add image during loading of swimming fish
      - Organize helper functions
