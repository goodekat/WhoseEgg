Applying MDS to the Training Data for WhoseEgg Shiny App
================
Katherine Goode <br>
Last Updated: April 07, 2021

This document contains code that applies multidimensional scaling (MDS)
to the random forest training data used in WhoseEgg. The results are
used to compare the input data points to the training data points to
determine if there are any observations in the input data that are very
different from the training data. If there observations that differ,
then the random forest may have to extrapolate to make predictions for
these observations leading to untrustworthy predictions.

Note that this method is used as opposed considering each predictor
variable individually, because there is moderate to high correlation
between some of the variables. The correlation between predictor
variables can lead to scenarios where an observation may fall within the
range of the marginal distributions of variables but not within the
range of the joint distributions. Without MDS, observations that fit in
this scenario may be overlooked.

# Setup

Load packages:

``` r
library(dplyr)
library(randomForest)
library(purrr)
```

Make a vector of the predictor variables:

``` r
vars_pred = c(
  "Month",
  "Julian_Day",
  "Temperature",
  "Conductivity",
  "Larval_Length",
  "Membrane_Ave",
  "Membrane_SD",
  "Membrane_CV",
  "Yolk_to_Membrane_Ratio",
  "Yolk_Ave",
  "Yolk_SD",
  "Yolk_CV",
  "Egg_Stage",
  "Compact_Diffuse",
  "Pigment",
  "Sticky_Debris",
  "Deflated"
)
```

Load the egg data:

``` r
eggdata <- read.csv("../data/eggdata_for_app.csv")
```

# MDS Application

Save the MDS results:

``` r
# saveRDS(
#   object = rfs_for_app, 
#   file = "../data/rfs_for_app.rds"
# )
```
