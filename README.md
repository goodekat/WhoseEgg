
# WhoseEgg

![](www/eggs-in-a-row.jpeg)

Repository to store the code associated with the WhoseEgg R Shiny app
for classifying invasive carp eggs using a random forest model. The app
is available at <https://whoseegg.stat.iastate.edu/>.

## Completed

-   Fixed flagged variable
-   Added warning if the data is in the future
-   Fixed issues with uploading non- csv, xlsx, or xls files
-   Organized helper functions

## To do

-   Organize github for future developers
-   Create video showing how to use the app
-   Add video to home page of app
-   Add image during loading of swimming fish

## Repository Structure

Folders in the WhoseEgg GitHub repository:

-   [`data`](data/): Folder that contains the datasets used by WhoseEgg
    (or used to prepare the data used by WhoseEgg)
-   [`prep`](prep/): Folder that contains the code used to prepare the
    [training data](prep/01-data-for-app.md) and [random
    forests](prep/02-rfs-for-app.md) for WhoseEgg. There are also files
    for creating an animation of a [swimming
    fish](prep/03-animation-for-app.md), investigating the use of [MDS
    to identify new observations outside of the training
    data](prep/04-mds-for-app.md), and [testing the
    functions](99-testing-app-functions.md) used by WhoseEgg. (Look at
    the .md files for easy viewing on GitHub.)
-   [`text`](text/): Folder with R markdown files accessed by WhoseEgg
    to incorporate text in the app
-   [`www`](www/): Contains the figures used by WhoseEgg

Files in the WhoseEgg GitHub repository:

-   [`app.R`](app.R): Main R script that contains the server and UI for
    the app
-   [`helper-functions.R`](helper-functions.R): R script that contains
    the helper functions used by WhoseEgg
-   [`matomo.txt`](matomo.txt): Text file that is necessary for Matomo
    to collect user information from the WhoseEgg server (DO NOT REMOVE)
-   [`r-requirements.txt`](r-requirements.txt): Text file with a list of
    R packages used by WhoseEgg that is necessary for the server to work
    correctly (DO NOT REMOVE)
-   [`README.md` and `README.Rmd`](README.md): README file for WhoseEgg
    GitHub repository with lots of helpful information
-   [`references.bib`](references.bib): File with BibTex citations used
    in WhoseEgg

## Updating the App

## Checking Matomo User

## Ideas for Future Versions

Data Input:

-   Add option for manual input of one observation (could try using a
    Google form embedding in the app)
-   Look into research on data input in apps

Methods:

-   Add more visualizations:
    -   Comparing input data to training data
    -   Create an MDS plot with training and new data
    -   Make plots interactive
    -   Add plots comparing predictions with egg characteristics
-   Interpretations of random forest probabilities
    -   Add a nice interpretation
    -   Add comments on the interpretations of probabilities near
        0.5-0.6
    -   Look into how to compute empirical probabilities and try out the
        model calibration technique
    -   Read papers on model calibration:
        -   <https://onlinelibrary.wiley.com/doi/abs/10.1002/sam.11446>
        -   <https://pubmed.ncbi.nlm.nih.gov/29290291/>
        -   <https://www.sciencedirect.com/science/article/pii/S1532046415000027>
-   Add random forest prediction intervals
-   Switch to using random forests with reduced features
-   Try using weighting in random forests to account for imbalance in
    classes

Format:

-   Figure out a better way to fix the header so it doesn’t cover
    material when the screen size changes
-   Fix the WhoseEgg color text when hovered over
-   Add a download image button

Other:

-   Add a check for when a new dataset is uploaded
-   Add formal tests
-   Connect with GitHub actions
-   Look into the process the fish bioenergetics Shiny app uses to
    update frequently with more data and fish species
-   Update functions and code to make more efficient
