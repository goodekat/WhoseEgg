
# Shiny Carp App

Repository to store the code associated with the random forest app for
predicting fish species

## Changes made

## To do list for app

  - Overview page:
      - Allow option for inputting Julian day
      - Create and add video showing how to use the app
      - Convert text in tabs to html or R code

<br>

  - Data input page:
      - Highlight the search feature (especially for helping to find
        observations with typos)
      - Change to not having link to data validation explanation
      - Print lists in the errors with row breaks
      - Add visualizations of new data compared to training data
      - Look into research on data input in apps
      - Try using Google forms for input options – could even embed it
        in the app
      - Could use a conditional panel to create click through options
        for manual input

<br>

  - Prediction page:
      - Figure out how to compute prediction intervals
      - Add visualizations of the data (both input data and training
        data with input data included)
      - Add a download image button
      - Change the question mark to caption below the images
      - Add interpretation of the random forest probabilities (look up a
        nice interpretation)
      - Discuss interpretation of probabilities near 0.5-0.6
      - Change header names to not be underscored
      - Make the figures taller not to overlap
      - Move all numbers on figures to right in summary plot
      - Add option to click on plot and show more plots (summary to
        input data features and individual to training data with
        observation)
      - Change wording on how the zooming can be done

<br>

  - Download page:

<br>

  - Help page:
      - Write variable definitions (include short definitions and
        additional details for more complicated variables)
      - Have tab with information on random forest
      - Add an FAQ to help page
          - Add a new tab for “what if I have data from different
            locations?”
          - Include comments about what to do if data is from a
            different location or contains different species
          - Why don’t by extra variables show up in the processed data
            tab?
      - Add buttons to easily get to the help page for the egg
        characteristics
      - Add information on how random forests work

<br>

  - Warnings/Errors:
      - Change error and warning messages to red/orange
      - Add warning if the data is in the future
      - Add a check for when a new dataset is uploaded
      - If you try to upload a non-csv file after uploading a file, you
        get a weird error on the prediction page
      - Need to make sure all error/warning messages match – such as
        including the egg ID for factor levels

<br>

  - Testing:
      - Need to test that different computer types work with Excel and
        csv uploads
          - Some people were getting errors with the Excel file saying
            something about “subscript out of bounds”

<br>

  - Other:
      - List app authors/contributors some where (me, Mike, Philip, and
        Carlos)
      - **Start writing the manuscript (see fisheries magazine and
        bioenergetics model)**
      - Add next buttons to go from input data to predictions to
        downloads (or add get predictions to the instructions)
      - The fish bioenergetics Shiny app updates frequently with more
        data and fish species
      - Is it possible to scroll through the tables? See if this option
        exists in DT.
      - Change “note” colors
      - Look into getting the copyright for the textbook diagram (the
        Kelso reference)
      - Can we fix the WhoseEgg color text when hovered over?
      - Figure out how to host app (Dr. Dixon)
      - Prepare slides for collaborator meeting
      - Add image during loading of swimming fish
      - Organize helper functions
      - Check out
        [ISOFAST](https://analytics.iasoybeans.com/cool-apps/ISOFAST/)
        for ideas
      - Watch these talks:
        [styling](https://rstudio.com/resources/rstudioconf-2020/styling-shiny-apps-with-sass-and-bootstrap-4/)
        and [code
        optimization](https://rstudio.com/resources/webinars/scaling-shiny-apps-with-asynchronous-programming/)
