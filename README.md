
# Shiny Carp App

Repository to store the code associated with the random forest app for
predicting fish species

## Changes made

  - Added comments about funding (XXX need to make sure I wrote this
    correctly)
  - Added a link to Carlos’s paper (actual paper or ISU digital
    repository)
  - Clarify opening sentence and include upper Mississippi river basin
    (XXX have Mike and Philip proof read)
  - Add text about where the data is collected and to be careful using
    it in other locations
  - Include statement about data privacy
  - Checked how month acts if incorrectly added to app - all good -
    shows that a wrong level has been provided
  - Fixed so I’m only considering the missing values for the key
    variables and not all variables
  - Increased the font size

## To do list for app

  - Overview page:
      - Create and add video showing how to use the app

<br>

  - Data input page:
      - **Add warning about data that fall outside of training data**
      - Add visualizations of new data compared to training data
      - Look into research on data input in apps
      - Add a comment in app describing how to turn “helpers” off
      - Add an error in R if day entered does not agree with month
      - For lists in the errors – try printing as a table/data frame –
        or try <br> instead of 
      - Try using Google forms for input options – could even embed it
        in the app
      - Could use a conditional panel to create click through options
        for manual input

<br>

  - Prediction page:
      - **Add text explaining what is in the tables and figures**:
          - **Guide users through the results**
          - **Have a key message below each graphic**
      - **Add a note about the percent of missing data discarded**
      - Add text about being able to zoom in on figures
      - Figure out how to compute prediction intervals

<br>

  - Download page:
      - **Add option to download csv or excel spreadsheet**
      - Add text explaining what will be downloaded

<br>

  - Help page
      - **Write variable definitions (include short definitions and
        additional details for more complicated variables)**
      - **Add figures from Carlos**
      - **Have tab with information on random forest**

<br>

  - Other:
      - **Prepare to meet with Mike’s students**
      - **Start writing the manuscript (see fisheries magazine and
        bioenergetics model)**
      - **Try out [shinyhelper](https://github.com/cwthom/shinyhelper)
        for adding question marks with popup messages**
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
