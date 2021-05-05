Determining Issues with Egg Stages
================
Katherine Goode <br>
Last Updated: May 04, 2021

We discovered that the manner in which the larval lengths were measured
given the egg stage appear to differ between 2014-2015 and 2016. This
notebook contains code that looks more into this and saves a file with
all of the possibly problematic eggs.

Load R packages:

``` r
library(dplyr)
library(purrr)
library(randomForest)
library(stringr)
library(tidyr)
library(tools)
```

## Identify the possibly problematic eggs

Load the raw 2014-2016 data from the validation paper:

``` r
eggdata_raw <-
  read.csv(
    "https://raw.githubusercontent.com/goodekat/carp-egg-rf-validation/master/data/eggdata141516_raw.csv"
  )
```

Clean the data to match what was done to the data for the validation
paper (expect for removing extra variables):

``` r
eggdata_cleaned_all_var <- 
  eggdata_raw %>%
  rename_all(
    .funs = function(.) {
      str_replace_all(., "[.]", " ") %>% 
        tolower() %>% 
        toTitleCase() %>% 
        str_replace_all(" ", "_") %>% 
        str_replace("Sd", "SD") %>% 
        str_replace("Cv", "CV") %>% 
        str_replace("Acgc", "ACGC") %>%
        str_replace("Temp", "Temperature")
    }
  ) %>%
  # Rename variables using yolk to embryo
  rename(
      "Embryo_to_Membrane_Ratio" = "Yolk_to_Membrane_Ratio",
      "Embryo_Ave" = "Yolk_Ave",
      "Embryo_SD" = "Yolk_SD",
      "Embryo_CV" = "Yolk_CV",
  ) %>%
  # Only keep observations with identified genetics
  filter(Questionable_Genetics == "NO") %>%
  # Remove any observations with missing values
  na.omit() %>%
  # Fix some mistakes in site and river levels
  mutate(Site = as.character(Site), River = as.character(River)) %>%
  mutate(Site = ifelse(Site == "DMW", "DNW", Site),
         Site = ifelse(Site == "kqa", "KQA", Site),
         River = ifelse(River == "UNR", "UMR", River),
         River = ifelse(River == "UPR", "UMR", River),
         River = ifelse(River == "DMS", "DSM", River)) %>%
  mutate(Site = factor(Site), River = factor(River)) 
```

Print the dimensions of the cleaned data:

``` r
dim(eggdata_cleaned_all_var)
```

    ## [1] 1979   46

Create a table showing the proportion of observations with a larval
length of 0 for each egg stage and year – note that in 2014 and 2015,
all eggs that were not in egg stage 7 or 8 have a larval length of 0,
and all eggs in egg stage of 7 or 8 have a larval of 0 – this is not the
case in 2016:

``` r
eggdata_cleaned_all_var %>%
  group_by(Egg_Stage, Year) %>%
  summarise(
    n_zeros = sum(Larval_Length == 0),
    freq = n(), 
    .groups = "drop"
  ) %>%
  mutate(prop_of_zeros = round(n_zeros / freq, 4)) %>%
  select(-n_zeros, -freq) %>%
  pivot_wider(id_cols = Egg_Stage, names_from = Year, values_from = prop_of_zeros) %>%
  knitr::kable()
```

| Egg\_Stage | 2014 | 2015 |   2016 |
|:-----------|-----:|-----:|-------:|
| 1          |    1 |    1 | 1.0000 |
| 2          |    1 |    1 | 0.9000 |
| 3          |    1 |    1 | 0.9744 |
| 4          |    1 |    1 | 1.0000 |
| 5          |    1 |    1 | 0.9836 |
| 6          |    1 |    1 | 0.7975 |
| 7          |    0 |    0 | 0.1346 |
| 8          |    0 |    0 | 0.0345 |
| BROKEN     |    1 |   NA |     NA |
| D          |    1 |    1 | 0.9796 |

Identify the eggs from 2016 with non-zero larval lengths with an egg
stage other than 7 or 8:

``` r
eggs_wrong_non_zero_lls <-
  eggdata_cleaned_all_var %>%
  filter(Year == 2016, Egg_Stage %in% c(1:6, "D", "BROKEN"), Larval_Length > 0)
```

Identify the eggs from 2016 with larval lengths of zero with an egg
stage of 7 or 8:

``` r
eggs_wrong_zero_lls  <-
  eggdata_cleaned_all_var %>%
  filter(Year == 2016, Egg_Stage %in% c(7,8), Larval_Length == 0)
```

Join the possibly problematic eggs:

``` r
eggs16_problems <-
  bind_rows(eggs_wrong_non_zero_lls, eggs_wrong_zero_lls)
```

Print the dimensions of the possibly problematic eggs:

``` r
dim(eggs16_problems)
```

    ## [1] 29 46

Save the possibly problematic eggs:

``` r
write.csv(eggs16_problems, "../data/problematic-eggs.csv", row.names = FALSE)
```

## Comments from Mike

-   Egg stages from the problematic eggs all look okay

-   For the eggs in stages other than 7 and 8, we could change the
    larval lengths to 0 to match Carlos’s approach

-   For the eggs in stages 7-8, he can’t get the larval lengths

-   Should we take all of these eggs out?

-   Should we remove only the eggs in stages 7 and 8 with non-zero
    larval lengths

-   Try fitting models and see how it affects the results from the
    various options

-   Mike’s email with updated eggs: “A lot of the rest I looked at I
    changed to stage 7, so the length was valid. I put a few comments in
    the column at the end.” Updated data is in the file “2016 eggs with
    incorrect data\_MW.csv”

## Comparing Models

Make a list of the response variables:

``` r
vars_resp = c(
  "Family_ACGC",
  "Genus_ACGC",
  "Common_Name_ACGC"
)
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
  "Embryo_to_Membrane_Ratio",
  "Embryo_Ave",
  "Embryo_SD",
  "Embryo_CV",
  "Egg_Stage",
  "Compact_Diffuse",
  "Pigment",
  "Sticky_Debris",
  "Deflated"
)
```

### Reconstruct Data from Validation Paper

``` r
eggdata_val_paper <-
  # Access the data from the validation paper that will be used for WhoseEgg
  # The difference from the data accessed earlier is the lack of some extra 
  # variables that are not needed for the validation paper or WhoseEgg
  read.csv(
    paste0(
      "https://raw.githubusercontent.com/goodekat/",
      "carp-egg-rf-validation/master/data/eggdata141516.csv"
    )
  ) %>%
  # Convert necessary variables to factors
  mutate_at(
    .vars = c(
      "Egg_Stage",
      "Compact_Diffuse",
      "Pigment",
      "Sticky_Debris",
      "Deflated",
      all_of(vars_resp)
    ),
    .funs = factor
  ) %>%
  # Change the level of ACGC to Invasive Carp for easier terminology in the app
  mutate(
    Family_ACGC = forcats::fct_recode(Family_ACGC, "Invasive Carp" = "ACGC"),
    Genus_ACGC = forcats::fct_recode(Genus_ACGC, "Invasive Carp" = "ACGC"),
    Common_Name_ACGC = forcats::fct_recode(Common_Name_ACGC, "Invasive Carp" = "ACGC")
  ) %>%
  # Make sure the Invasive Carp level is still the first factor level
  # (like ACGC was)
  # Otherwise, the random forest results will change slightly
  mutate(
    Family_ACGC = forcats::fct_relevel(Family_ACGC, "Invasive Carp"),
    Genus_ACGC = forcats::fct_relevel(Genus_ACGC, "Invasive Carp"),
    Common_Name_ACGC = forcats::fct_relevel(Common_Name_ACGC, "Invasive Carp")
  ) %>% 
  mutate(Egg_ID = 1:n()) %>%
  select(Egg_ID, everything())
```

### Create Data with All Problem Observation Removed

Identify the eggs from 2016 with non-zero larval lengths with an egg
stage other than 7 or 8 and the eggs from 2016 with larval lengths of
zero with an egg stage of 7 or 8:

``` r
problem_ids_not78 <-
  eggdata_val_paper %>%
  filter(Year == 2016, Egg_Stage %in% c(1:6, "D", "BROKEN"), Larval_Length > 0) %>%
  pull(Egg_ID)

problem_ids_78 <-
  eggdata_val_paper %>%
  filter(Year == 2016, Egg_Stage %in% c(7,8), Larval_Length == 0) %>%
  pull(Egg_ID)
```

Join the problem eggs IDs and print the number problem eggs:

``` r
problem_ids = c(problem_ids_not78, problem_ids_78)
length(problem_ids)
```

    ## [1] 29

Remove the problem observations:

``` r
eggdata_problems_removed <- 
  eggdata_val_paper %>%
  filter(!(Egg_ID %in% problem_ids))
```

Check that the correct number of observations have been removed:

``` r
(dim(eggdata_val_paper)[1] - dim(eggdata_problems_removed)[1]) == length(problem_ids)
```

    ## [1] TRUE

### Create Data with Egg Stages Corrected by Mike

``` r
eggs_corrected_MW <- read.csv("../data/2016 eggs with incorrect data_MW.csv")

eggs_cor_same_vars <- 
  eggs_corrected_MW %>%
  select(all_of(eggdata_val_paper %>% select(-Egg_ID, -Dataset) %>% names()))
```

``` r
eggdata_joined <-
  left_join(
    eggdata_val_paper %>% 
      rename("Egg_Stage_Old" = "Egg_Stage", "Larval_Length_Old" = "Larval_Length"),
    eggs_cor_same_vars %>% 
      rename("Egg_Stage_New" = "Egg_Stage", "Larval_Length_New" = "Larval_Length"),
    by = eggdata_val_paper %>% select(-Egg_ID,-Dataset,-Egg_Stage,-Larval_Length) %>% names()
  )

eggdata_corrected <-
  eggdata_joined %>%
  mutate(
    Egg_Stage_New = as.character(Egg_Stage_New),
    Egg_Stage_Old = as.character(Egg_Stage_Old)
  ) %>%
  mutate(
    Egg_Stage = ifelse(is.na(Egg_Stage_New), Egg_Stage_Old, Egg_Stage_New),
    Larval_Length = ifelse(is.na(Larval_Length_New), Larval_Length_Old, Larval_Length_New)) %>%
  # Convert necessary variables to factors
  mutate_at(
    .vars = c(
      "Egg_Stage",
      "Compact_Diffuse",
      "Pigment",
      "Sticky_Debris",
      "Deflated",
      all_of(vars_resp)
    ),
    .funs = factor
  ) %>%
  # Make sure the Invasive Carp level is still the first factor level
  # (like ACGC was)
  # Otherwise, the random forest results will change slightly
  mutate(
    Family_ACGC = forcats::fct_relevel(Family_ACGC, "Invasive Carp"),
    Genus_ACGC = forcats::fct_relevel(Genus_ACGC, "Invasive Carp"),
    Common_Name_ACGC = forcats::fct_relevel(Common_Name_ACGC, "Invasive Carp")
  )

# Check that the number of observations is correct
dim(eggdata_corrected)[1] == dim(eggdata_val_paper)[1]
```

    ## [1] TRUE

### Create Data with Egg Stages Corrected by Mike and Larval Lengths Set to 0

Something isn’t right here…

``` r
eggdata_corrected %>%
  filter(!(Egg_Stage %in% c(7,8)) & Larval_Length > 0)
```

    ##   Egg_ID    Dataset Site River Year Month Julian_Day Temperature Conductivity
    ## 1   1534 validation  DNI   UMR 2016     5        151        23.8          484
    ## 2   1630 validation  DNS   UMR 2016     5        151        24.6          442
    ## 3   1723 validation  MTH   SKK 2016     5        151        22.0          380
    ##   Larval_Length_Old Membrane_Ave Membrane_SD Membrane_CV
    ## 1          3.512027      4.26567     0.14681     0.03442
    ## 2          2.878461      2.99814     0.34258     0.11426
    ## 3          3.152629      3.39052     0.18432     0.05436
    ##   Embryo_to_Membrane_Ratio Embryo_Ave Embryo_SD Embryo_CV Egg_Stage_Old
    ## 1                  0.38653    1.64882   0.30430   0.18455             6
    ## 2                  0.54115    1.62246   0.13205   0.08139             6
    ## 3                  0.53063    1.79910   0.17028   0.09465             6
    ##   Compact_Diffuse Pigment Sticky_Debris Deflated              Genus Common_Name
    ## 1               C       N             N        Y Hypophthalmichthys Silver Carp
    ## 2               C       N             N        Y Hypophthalmichthys Silver Carp
    ## 3               C       N             N        Y   Ctenopharyngodon  Grass Carp
    ##     Family_ACGC    Genus_ACGC Common_Name_ACGC Larval_Length_New Egg_Stage_New
    ## 1 Invasive Carp Invasive Carp    Invasive Carp                NA          <NA>
    ## 2 Invasive Carp Invasive Carp    Invasive Carp                NA          <NA>
    ## 3 Invasive Carp Invasive Carp    Invasive Carp                NA          <NA>
    ##   Egg_Stage Larval_Length
    ## 1         6      3.512027
    ## 2         6      2.878461
    ## 3         6      3.152629

### Fit All Random Forests

Function for fitting a random forest model given a response variable,
predictor variables, and a dataset (uses the same seed to fit the random
forests as Camacho et al. (2019) and Goode et al. (2021)):

``` r
fit_rf <- function(resp, preds, data) {
  
  # Fit the random forest
  set.seed(808)
  rf <- randomForest(
    data %>% pull(resp) ~ .,
    data = data %>% select(all_of(preds)),
    importance = T,
    ntree = 1000
  )
  
  # Put model in a named list
  rf_list = list(rf)
  names(rf_list) = resp
  
  # Return the named list
  return(rf_list)
  
}
```

Fit the random forest models:

``` r
rfs_val_data <-
  map(
    .x = vars_resp,
    .f = fit_rf,
    preds = vars_pred,
    data = eggdata_val_paper
  ) %>%
  flatten()

rfs_prob_remv <-
  map(
    .x = vars_resp,
    .f = fit_rf,
    preds = vars_pred,
    data = eggdata_problems_removed
  ) %>%
  flatten()

rfs_prob_corr <-
  map(
    .x = vars_resp,
    .f = fit_rf,
    preds = vars_pred,
    data = eggdata_corrected
  ) %>%
  flatten()
```

Check to make sure the random forests agree (note that these random
forests are available on GitHub:
<https://github.com/goodekat/carp-egg-rf-validation/blob/master/results/rfs141516.rds>)

``` r
rfs141516 <- readRDS("../../../validation/results/rfs141516.rds")
c(
  identical(rfs141516$Family_ACGC$forest, rfs_val_data$Family_ACGC$forest),
  identical(rfs141516$Genus_ACGC$forest, rfs_val_data$Genus_ACGC$forest),
  identical(rfs141516$Common_Name_ACGC$forest, rfs_val_data$Common_Name_ACGC$forest)
)
```

    ## [1] TRUE TRUE TRUE

``` r
data.frame(
  model_validation = rfs_val_data$Family_ACGC$confusion[,9],
  model_removed = rfs_prob_remv$Family_ACGC$confusion[,9],
  model_corrected = rfs_prob_corr$Family_ACGC$confusion[,9]
)
```

    ##               model_validation model_removed model_corrected
    ## Invasive Carp       0.02685422    0.02824134      0.02685422
    ## Catostomidae        0.16666667    0.16666667      0.20000000
    ## Clupeidae           1.00000000    1.00000000      1.00000000
    ## Cyprinidae          0.15151515    0.14578005      0.14898990
    ## Hiodontidae         0.28571429    0.28571429      0.42857143
    ## Moronidae           0.88888889    0.83333333      0.94444444
    ## Percidae            1.00000000    1.00000000      1.00000000
    ## Sciaenidae          0.02303523    0.02231520      0.02303523

``` r
data.frame(
  model_validation = rfs_val_data$Genus_ACGC$confusion[,17],
  model_removed = rfs_prob_remv$Genus_ACGC$confusion[,17],
  model_corrected = rfs_prob_corr$Genus_ACGC$confusion[,17]
)
```

    ##               model_validation model_removed model_corrected
    ## Invasive Carp       0.02429668    0.02182285      0.02685422
    ## Alosa               1.00000000    1.00000000      1.00000000
    ## Aplodinotus         0.02168022    0.02231520      0.02303523
    ## Carpiodes           0.70000000    0.60000000      0.70000000
    ## Cyprinella          0.66666667    0.66666667      0.66666667
    ## Dorosoma            1.00000000    1.00000000      0.50000000
    ## Etheostoma          1.00000000    1.00000000      1.00000000
    ## Hiodon              0.28571429    0.28571429      0.28571429
    ## Ictiobus            0.10000000    0.10000000      0.15000000
    ## Luxilus             1.00000000    1.00000000      1.00000000
    ## Macrhybopsis        0.75000000    0.75000000      0.76562500
    ## Morone              0.77777778    0.77777778      0.77777778
    ## Notropis            0.09375000    0.08571429      0.09375000
    ## Percina             1.00000000    1.00000000      1.00000000
    ## Pimephales          1.00000000    1.00000000      1.00000000
    ## Sander              1.00000000    1.00000000      1.00000000
