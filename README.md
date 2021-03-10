
# Shiny Carp App

Repository to store the code associated with the random forest app for
predicting fish species

## Changes made

## To do list for app

  - Meeting with collaborators:
      - **Email IT about time frame**
      - **Determine how to host the app (IT or RStudio server)**
      - **Make a dataset with 5 errors for people to fix**
      - **Send email about trying out the app ahead of time (by March 18
        or 19)**
      - Create a list of questions for the group
      - Prepare slides for meeting

<br>

  - Manuscript:
      - **Create an outline**
          - Think of it as an advertisement
          - Emphasize the need to make the model more broadly available
            via the app
          - Intro: explaining identification of carp eggs and fish eggs
            in general
          - WhoseEgg: explain app, model, data, etc.
          - Example – include a YouTube video link
      - Send outline their way

<br>

  - Overview page:
      - **Convert text in tabs to html or R code**
      - Create and add video showing how to use the app
      - Allow option for inputting Julian day

<br>

  - Data input page:
      - **Highlight the search feature (especially for helping to find
        observations with typos)**
      - **Change to not having link to data validation explanation**
      - **Print lists in the errors with row breaks**
      - Add visualizations of new data compared to training data
      - Look into research on data input in apps
      - Try using Google forms for input options – could even embed it
        in the app
      - Could use a conditional panel to create click through options
        for manual input

<br>

  - Prediction page:
      - **Add visualizations of the data (both input data and training
        data with input data included)**
      - **Add a download image button**
      - **Change the question mark to caption below the images**
      - **Add interpretation of the random forest probabilities (look up
        a nice interpretation)**
      - **Discuss interpretation of probabilities near 0.5-0.6**
      - **Change header names to not be underscored**
      - **Make the figures taller not to overlap**
      - **Move all numbers on figures to right in summary plot**
      - **Add option to click on plot and show more plots (summary to
        input data features and individual to training data with
        observation)**
      - **Change wording on how the zooming can be done**
      - Figure out how to compute prediction intervals

<br>

  - Download page:

<br>

  - Help page:
      - **Email copyright people about using the figures**
      - **Edit definitions**
      - **Have tab with information on random forest**
      - **Add an FAQ to help page**
          - **Add a new tab for “what if I have data from different
            locations?”**
          - **Include comments about what to do if data is from a
            different location or contains different species**
          - **Why don’t by extra variables show up in the processed data
            tab?**
      - **Add buttons to easily get to the help page for the egg
        characteristics**

<br>

  - Warnings/Errors:
      - **Change error and warning messages to red/orange**
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
          - Some people were getting errors with the Excel file saying
            something about “subscript out of bounds”

<br>

  - Other:
      - **List app authors/contributors some where (me, Mike, Philip,
        and Carlos)**
      - **Add next buttons to go from input data to predictions to
        downloads (or add get predictions to the instructions)**
      - **Change “note” colors**
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
