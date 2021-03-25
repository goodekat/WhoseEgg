
# WhoseEgg To Do List

## Completed

## To do

Overview page:

- Create and add video showing how to use the app

Data input page:

- Add visualizations of new data compared to training data
- Allow option for inputting Julian day

Prediction page:

- Add visualizations of the data (both input data and training data with input data included)
- Add interpretation of the random forest probabilities (look up a nice interpretation)
- Add a download image button
- Add option to click on plot and show more plots (summary to input data features and individual to training data with observation)
- Figure out how to compute prediction intervals

Download page:

Help page:

- Add guidance on how to interpret random forest probabilities (especially probabilities near 0.5-0.6)
- Add link to validation paper to FAQ on validation

Warnings/Errors:

- Print lists in the errors with row breaks
- Add warning if the data is in the future
- Add a check for when a new dataset is uploaded
- If you try to upload a non-csv file after uploading a file, you get a weird error on the prediction page
- Need to make sure all error/warning messages match – such as including the egg ID for factor levels

Manuscript:
  
- **Start writing**

Testing:
  
- Need to test that different computer types work with Excel and csv uploads
    - Some people were getting errors with the Excel file saying something about “subscript out of bounds”

Other:
  
- Add image during loading of swimming fish
- Can we fix the WhoseEgg color text when hovered over?
- Organize helper functions
- Check out [ISOFAST](https://analytics.iasoybeans.com/cool-apps/ISOFAST/) for ideas
- Watch these talks: [styling](https://rstudio.com/resources/rstudioconf-2020/styling-shiny-apps-with-sass-and-bootstrap-4/) and [code optimization](https://rstudio.com/resources/webinars/scaling-shiny-apps-with-asynchronous-programming/)
- Organize github for future developers

Ideas for future versions: 

- Try using Google forms for input options – could even embed it in the app or could use a conditional panel to create click through options for manual input
- Look into research on data input in apps
- Switch to using random forests with reduced features
- Try using weighting in random forests to account for imbalance in classes
- Look into the process the fish bioenergetics Shiny app uses to update frequently with more data and fish species