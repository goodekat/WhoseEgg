Computing variable levels and extreme values
================
Katherine Goode <br>
Last Updated: April 14, 2021

``` r
# Load packages
library(dplyr)
library(tidyr)
```

``` r
# Load egg data
eggdata <- read.csv("../data/eggdata_for_app.csv")
```

Date variables:

``` r
unique(eggdata$Month)
```

    ## [1] 7 8 6 5 4

``` r
sort(unique(eggdata$Julian_Day))
```

    ##  [1] 113 114 116 120 122 123 125 127 131 132 133 140 141 142 143 144 146 147 148
    ## [20] 150 151 152 153 154 155 156 160 161 162 163 164 170 171 172 173 175 180 181
    ## [39] 190 191 192 200 201 202 203 208 209 210 211 212 217 218 219 221 222 224 225
    ## [58] 226 227 228 231 232 234 236 240 241 243

Categorical variables:

``` r
unique(eggdata$Deflated)
```

    ## [1] "N" "Y"

``` r
unique(eggdata$Pigment)
```

    ## [1] "Y" "N"

``` r
unique(eggdata$Egg_Stage)
```

    ##  [1] "D"      "5"      "4"      "6"      "8"      "3"      "7"      "1"     
    ##  [9] "BROKEN" "2"

``` r
unique(eggdata$Compact_Diffuse)
```

    ## [1] "D" "C"

``` r
unique(eggdata$Sticky_Debris)
```

    ## [1] "N" "Y"

Continuous variables:

``` r
eggdata %>%
  select(
    Temperature,
    Conductivity,
    Larval_Length,
    Membrane_Ave,
    Membrane_SD,
    Membrane_CV,
    Embryo_to_Membrane_Ratio,
    Embryo_Ave,
    Embryo_SD,
    Embryo_CV
  ) %>%
  pivot_longer(
    names_to = "variable",
    values_to = "value",
    cols = everything()
  ) %>%
  group_by(variable) %>%
  summarise(
    min = round(min(value), 3),
    max = round(max(value), 3)
  )
```

    ## # A tibble: 10 x 3
    ##    variable                     min     max
    ##    <chr>                      <dbl>   <dbl>
    ##  1 Conductivity             274     781    
    ##  2 Embryo_Ave                 0.434   4.37 
    ##  3 Embryo_CV                  0.003   0.724
    ##  4 Embryo_SD                  0.005   1.38 
    ##  5 Embryo_to_Membrane_Ratio   0.257   1.14 
    ##  6 Larval_Length              0       5.09 
    ##  7 Membrane_Ave               0.728   5.49 
    ##  8 Membrane_CV                0.001   0.52 
    ##  9 Membrane_SD                0.001   1.47 
    ## 10 Temperature               11      30.7
