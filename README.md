
# WhoseEgg

![](www/eggs-in-a-row.jpeg)

Repository to store the code associated with the WhoseEgg R Shiny app
for classifying invasive carp eggs using a random forest model. The app
is available at <https://whoseegg.stat.iastate.edu/>.

## Repository Structure

**Folders**

-   [`data`](data/): Folder that contains the datasets used by WhoseEgg
    (or used to prepare the data used by WhoseEgg)
    -   [`2016 eggs with incorrect data_MW.csv`](data/2016%20eggs%20with%20incorrect%20data_MW.csv):
        Contains the problematic eggs with corrections by Mike
    -   [`eggdata_for_app.csv`](data/eggdata_for_app.csv): The data used
        to train the random forest models used by WhoseEgg
    -   [`problematic-eggs.csv`](data/problematic-eggs.csv): The dataset
        with identified problematic eggs
    -   [`rfs_for_app.rds`](data/rfs_for_app.rds): The random forests
        used by WhoseEgg to make predictions
    -   [`template.xlsx`](data/template.xlsx): The template for users of
        WhoseEgg that is downloable via the app
-   [`prep`](prep/): Folder that contains the code used to prepare the
    [training data](prep/01-data-for-app.md) and [random
    forests](prep/02-rfs-for-app.md) for WhoseEgg. There are also files
    for creating an animation of a [swimming
    fish](prep/03-animation-for-app.md), investigating the use of [MDS
    to identify new observations outside of the training
    data](prep/04-mds-for-app.md), and [testing the
    functions](prep/99-testing-app-functions.md) used by WhoseEgg. (Look
    at the .md files for easy viewing on GitHub.)
-   [`text`](text/): Folder with R markdown files accessed by WhoseEgg
    to incorporate text in the app
-   [`www`](www/): Contains the figures used by WhoseEgg

**Files**

-   [`app.R`](app.R): Main R script that contains the server and UI for
    the app
-   [`helper-functions.R`](helper-functions.R): R script that contains
    the helper functions used by WhoseEgg
-   [`matomo.txt`](matomo.txt): Text file that is necessary for Matomo
    to collect user information from the WhoseEgg server (DO NOT REMOVE)
-   [`r-requirements.txt`](r-requirements.txt): Text file with a list of
    R packages used by WhoseEgg that is necessary for the server to work
    correctly (DO NOT REMOVE)
-   [`README.md`](README.md) and [`README.Rmd`](README.Rmd): README file
    for WhoseEgg GitHub repository with lots of helpful information
-   [`references.bib`](references.bib): File with BibTex citations used
    in WhoseEgg

## Updating the App

1.  **Clone WhoseEgg Repository into R Studio**: See
    [this](https://happygitwithr.com/rstudio-git-github.html) book
    chapter for help if this is new to you.

2.  **Edit code and/or files as needed**: If the only changes that need
    to be made are updating the training data and random forests, you
    only need to adjust the code in the files
    [`01-data-for-app.Rmd`](prep/01-data-for-app.Rmd) and
    [`02-rfs-for-app.Rmd`](prep/02-rfs-for-app.Rmd) and save the new
    versions of the data and random forests. The code in app.R will
    automatically use whatever versions of
    [`eggdata_for_app.csv`](data/eggdata_for_app.csv) and
    [`rfs_for_app.rds`](data/rfs_for_app.rds) are in the repository
    [data](data/) folder.

3.  **Commit and push the updates to the main branch of the
    repository**: You could also create a new branch while working on
    updating the app and then merge the new branch with the main branch.
    The WhoseEgg server uses the files in the main branch.

4.  **Wait for the server to update**: This may take a while.

*If any new R packages are added to the app, their name must be added to
the [`r-requirements.txt`](r-requirements.txt) file.*

## ISU Developers Only

### Checking Matomo User Data

In order to access the Matomo user data associated with WhoseEgg, log in
at <https://trends.ent.iastate.edu/>. Currently, only Katherine Goode
has access. Contact <websupport@iastate.edu> to be added to the users
who can access the Matomo data. (Must also have approval from Mike Weber
and Philip Dixon.)

### Check Log for WhoseEgg Server

If there is an error with the WhoseEgg server, you can check the log by
going through these steps (must have the appropriate access):

1.  Go [here](https://console.apps.nimbus.las.iastate.edu/) and login
    with okta (must be connected to the ISU VPN).

2.  Also make sure ‘Developer’ is selected from the dropdown in the top
    left rather than ‘Administrator’.

3.  If you see a dropdown near the top, choose ‘rit-pdixon-lab-carp’.

4.  Click ‘Topology’ on the left.

5.  Then click the big circle.

6.  Then under the ‘Pods’ heading, click ‘View’ Logs’.

If you need to get access, contact <researchit@iastate.edu>. Currently,
only Katherine and Philip have access. (Must also have approval from
Philip Dixon.)

## Ideas for Future Versions

**Data Input**

-   Add option for manual input of one observation (could try using a
    Google form embedding in the app)
-   Look into research on data input in apps

**Methods**

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

**Format**

-   Figure out a better way to fix the header so it doesn’t cover
    material when the screen size changes
-   Fix the WhoseEgg color text when hovered over
-   Add a download image button

**Other**

-   Add a check for when a new dataset is uploaded
-   Add formal tests
-   Connect with GitHub actions
-   Look into the process the fish bioenergetics Shiny app uses to
    update frequently with more data and fish species
-   Update functions and code to make more efficient
