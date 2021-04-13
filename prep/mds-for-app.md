Applying MDS to the Training Data for WhoseEgg Shiny App
================
Katherine Goode <br>
Last Updated: April 12, 2021

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
library(ggplot2)
library(gower)
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

Select just the predictor variables and scale the numeric variables:

``` r
eggdata_preds <-
  eggdata %>% 
  select(all_of(vars_pred)) %>%
  mutate_if(.predicate = is.numeric, .funs = scale)
```

# MDS with Gower Distance

``` r
n = dim(eggdata_preds)[1]
n
```

    ## [1] 1978

## MDS on Training Data

Compute the Gower distance between each observation and all other
observations in the data and put in a list (where each element contains
the distances between one observation and all the other observations.)

``` r
list_of_distances <-
  map(
    .x = 1:n,
    .f = function(obs) {
      gower_dist(eggdata_preds, eggdata_preds[obs,])
    }
  )
```

Convert the list of distances to a distance matrix:

``` r
dist_matrix = matrix(unlist(list_of_distances), ncol = n)
```

Apply MDS (specify that the eigenvalues are returned):

``` r
mds <- cmdscale(dist_matrix, eig = TRUE, k = 2)
```

Plot the eigenvalues:

``` r
data.frame(eig = mds$eig) %>%
  mutate(num = 1:n(), prop = eig / sum(eig)) %>%
  ggplot(aes(x = num, y = prop)) + 
  geom_point()
```

![](mds-for-app_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Plot the first two dimensions:

``` r
data.frame(mds$points) %>%
  ggplot(aes(x = X1, y = X2)) + 
  geom_point() + 
  theme(aspect.ratio = 1)
```

![](mds-for-app_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Save the MDS results:

``` r
saveRDS(
  object = mds,
  file = "../data/mds_for_app.rds"
)
```

## MDS on Testing Data

Based on discussion
[here](https://stats.stackexchange.com/questions/368331/project-new-point-into-mds-space)

-   ![n](https://latex.codecogs.com/png.latex?n "n") = number of
    observations
-   ![\\delta\_n](https://latex.codecogs.com/png.latex?%5Cdelta_n "\delta_n")
    = matrix of squared distances between all observations
-   ![L\_k](https://latex.codecogs.com/png.latex?L_k "L_k") = a
    ![k\\times n](https://latex.codecogs.com/png.latex?k%5Ctimes%20n "k\times n")
    matrix of the MDS low dimensional embedding space
-   ![L\_k^{\\\#}](https://latex.codecogs.com/png.latex?L_k%5E%7B%5C%23%7D "L_k^{\#}")
    = the transpose of the pseudo-inverse of
    ![L\_k](https://latex.codecogs.com/png.latex?L_k "L_k")
-   ![\\delta\_i](https://latex.codecogs.com/png.latex?%5Cdelta_i "\delta_i")
    = the ![i](https://latex.codecogs.com/png.latex?i "i")th column of
    ![\\delta\_n](https://latex.codecogs.com/png.latex?%5Cdelta_n "\delta_n")
    containing the squared distances from point
    ![i](https://latex.codecogs.com/png.latex?i "i") to all other points
-   ![\\overset{\\rightarrow}{\\delta}\_{\\mu}=\\frac{1}{n}\\sum\_{i=1}^n\\overset{\\rightarrow}{\\delta}\_i](https://latex.codecogs.com/png.latex?%5Coverset%7B%5Crightarrow%7D%7B%5Cdelta%7D_%7B%5Cmu%7D%3D%5Cfrac%7B1%7D%7Bn%7D%5Csum_%7Bi%3D1%7D%5En%5Coverset%7B%5Crightarrow%7D%7B%5Cdelta%7D_i "\overset{\rightarrow}{\delta}_{\mu}=\frac{1}{n}\sum_{i=1}^n\overset{\rightarrow}{\delta}_i")
    = mean of the columns of the distance matrix
-   ![a](https://latex.codecogs.com/png.latex?a "a") = a new observation
    which we want to put in low dimensional space
-   ![\\overset{\\rightarrow}{\\delta}\_a](https://latex.codecogs.com/png.latex?%5Coverset%7B%5Crightarrow%7D%7B%5Cdelta%7D_a "\overset{\rightarrow}{\delta}_a")
    = vector containing the square distance from
    ![a](https://latex.codecogs.com/png.latex?a "a") to every other
    training point

Then the projection of the new point
![a](https://latex.codecogs.com/png.latex?a "a") into the lower
dimensional space is

![\\overset{\\rightarrow}{x}\_a=-\\frac{1}{2}L\_k^{\\\#}\\left(\\overset{\\rightarrow}{\\delta}\_a-\\overset{\\rightarrow}{\\delta}\_\\mu\\right)](https://latex.codecogs.com/png.latex?%5Coverset%7B%5Crightarrow%7D%7Bx%7D_a%3D-%5Cfrac%7B1%7D%7B2%7DL_k%5E%7B%5C%23%7D%5Cleft%28%5Coverset%7B%5Crightarrow%7D%7B%5Cdelta%7D_a-%5Coverset%7B%5Crightarrow%7D%7B%5Cdelta%7D_%5Cmu%5Cright%29 "\overset{\rightarrow}{x}_a=-\frac{1}{2}L_k^{\#}\left(\overset{\rightarrow}{\delta}_a-\overset{\rightarrow}{\delta}_\mu\right)")

Put a new observation in the low dimensional space of the training data
MDS:

``` r
delta_n = (dist_matrix)^2
L2 = t(mds$points)
L2inv <- t(MASS::ginv(L2))
delta_mu <- colSums(delta_n) / n
a = eggdata_preds[1,]
delta_a = (gower_dist(eggdata_preds, a))^2
xa = -0.5 * L2inv %*% (delta_a - delta_mu)
```

``` r
data.frame(mds$points) %>%
  mutate(data = "training") %>%
  bind_rows(data.frame(t(xa), data = "new")) %>%
  ggplot(aes(x = X1, y = X2, color = data)) + 
  geom_point() + 
  theme(aspect.ratio = 1)
```

![](mds-for-app_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Put a new observation in the low dimensional space of the training data
MDS:

``` r
b = eggdata_preds[1:10,]
n_b = dim(b)[1]
delta_b <-
  map(
    .x = 1:n_b,
    .f = function(obs) {
      gower_dist(eggdata_preds, b[obs,])^2
    }
  ) %>% 
  unlist() %>%
  matrix(ncol = n_b)
xb = -0.5 * L2inv %*% (delta_b - delta_mu)
```

``` r
data.frame(mds$points) %>%
  mutate(data = "training") %>%
  bind_rows(data.frame(t(xb), data = "new")) %>%
  ggplot(aes(x = X1, y = X2, color = data)) + 
  geom_point() + 
  theme(aspect.ratio = 1)
```

![](mds-for-app_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
