
# WhoseEgg To Do List

## Completed

## To do

Manuscript:
  
- **Start writing**
- Check out reference formats of other articles in magazine and copy their formats

Overview page:

- Create and add video showing how to use the app

Data input page:

- **Create an MDS plot with training and new data**
  - **Switch to computing MDS on training data first and then projecting input data on the training data space**
  - **Scale the variables first**
  - **Switch to using Gower distance and all variables**
- **Try computing Cook's D and leverage**
- **Add visualizations of new data compared to training data**
- Could email to ask the woman about the extra spaces to see if fixed now
- Allow option for inputting Julian day

Prediction page:

- **Add visualizations of the data (both input data and training data with input data included)**
- **Add interpretation of the random forest probabilities (look up a nice interpretation)**
- **Add option to click on plot and show more plots (summary to input data features and individual to training data with observation)**
- Figure out how to compute prediction intervals
- Add a download image button

Download page:

- **Add variable flagging observations that are outside the range of the training data**

Help page:

- **Add comments on the interpretations of random forests**
- **Look into how to compute empirical probabilities and try out the model calibration technique**
- **Figure out issue with larval length and egg stage (which doesn't match up)**
- **Add guidance on how to interpret random forest probabilities (especially probabilities near 0.5-0.6)**
- **Check to see if any fisheries papers on random forests**

Warnings/Errors:

- **Print lists in the errors with row breaks**
- **Add warning if the data is in the future**
- **Add a check for when a new dataset is uploaded**
- **If you try to upload a non-csv file after uploading a file, you get a weird error on the prediction page**
- **Need to make sure all error/warning messages match – such as including the egg ID for factor levels**

Testing:
  
- Need to test that different computer types work with Excel and csv uploads (ask Heike how to do this)
    - Some people were getting errors with the Excel file saying something about “subscript out of bounds”
- Try out app on various computers and browsers to see if it works

Other:
  
- **Prepare for lunchinatoRs**
- **Proofread app**
- Add image during loading of swimming fish
- Can we fix the WhoseEgg color text when hovered over?
- Organize helper functions
- Organize github for future developers

Ideas for future versions: 

- Try using Google forms for input options – could even embed it in the app or could use a conditional panel to create click through options for manual input
- Look into research on data input in apps
- Switch to using random forests with reduced features
- Try using weighting in random forests to account for imbalance in classes
- Look into the process the fish bioenergetics Shiny app uses to update frequently with more data and fish species