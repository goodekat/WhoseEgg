
# Shiny Carp App

Repository to store the code associated with the random forest app for
predicting fish species

## Completed

  - Emailed IT about time frame
  - Determined how to host the app (IT or RStudio server)
  - Changed to not having link to data validation explanation
  - Changed the question mark to caption below the images
  - Added next buttons to go from input data to predictions to downloads
    (or add get predictions to the instructions)
  - Changed error and warning messages to red/orange
  - Emphasized ‘note’
  - Listed app authors/contributors some where (me, Mike, Philip, and
    Carlos)
  - Emailed copyright people about using the figures and added statement
  - Added an FAQ to help page
  - Made the figures taller not to overlap

## To do list for app

  - Meeting with collaborators:
      - **Make a dataset with 5 errors for people to fix**
      - **Send email about trying out the app ahead of time (by March 18
        or 19)**
      - Create a list of questions for the group
      - Prepare slides for meeting

<br>

  - Manuscript:
      - **Create an outline and send to Mike and Philip**

<br>

  - Overview page:
      - **Convert text in tabs to html or R code**
      - Create and add video showing how to use the app

<br>

  - Data input page:
      - **Add button to jump to help page**
      - **Highlight the search feature (especially for helping to find
        observations with typos)**
      - **Print lists in the errors with row breaks**
      - Add visualizations of new data compared to training data
      - Look into research on data input in apps
      - Try using Google forms for input options – could even embed it
        in the app
      - Could use a conditional panel to create click through options
        for manual input
      - Allow option for inputting Julian day

<br>

  - Prediction page:
    
      - **Change header names to not be underscored**
      - **Add a download image button**
      - **Add visualizations of the data (both input data and training
        data with input data included)**
      - **Add interpretation of the random forest probabilities (look up
        a nice interpretation)**
      - **Discuss interpretation of probabilities near 0.5-0.6**
      - **Move all numbers on figures to right in summary plot**
      - **Add option to click on plot and show more plots (summary to
        input data features and individual to training data with
        observation)**
      - **Change wording on how the zooming can be done**
      - Figure out how to compute prediction intervals

  - Download page:

  - Help page:
    
      - **Edit definitions**
      - **Have tab with information on random forest**
      - **Add buttons to easily get to the help page for the egg
        characteristics**
      - **Fill in and add to FAQ**

  - Warnings/Errors:
    
      - **Add warning if the data is in the future**
      - **Add a check for when a new dataset is uploaded**
      - **If you try to upload a non-csv file after uploading a file,
        you get a weird error on the prediction page**
      - **Need to make sure all error/warning messages match – such as
        including the egg ID for factor levels**

<br>

  - Testing:
      - Need to test that different computer types work with Excel and
        csv uploads
    
      - ``` 
        Some people were getting errors with the Excel file saying something about “subscript out of bounds”
        ```

<br>

  - Other:
      - Add image during loading of swimming fish
      - Is it possible to scroll through the tables? See if this option
        exists in DT.
      - Can we fix the WhoseEgg color text when hovered over?
      - Organize helper functions
      - The fish bioenergetics Shiny app updates frequently with more
        data and fish species
      - Check out
        [ISOFAST](https://analytics.iasoybeans.com/cool-apps/ISOFAST/)
        for ideas
      - Watch these talks:
        [styling](https://rstudio.com/resources/rstudioconf-2020/styling-shiny-apps-with-sass-and-bootstrap-4/)
        and [code
        optimization](https://rstudio.com/resources/webinars/scaling-shiny-apps-with-asynchronous-programming/)
