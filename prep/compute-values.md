Computing values for WhoseEgg
================
Katherine Goode
Last Updated: March 18, 2021

``` r
# Load packages
library(dplyr)
library(tidyr)
```

``` r
# Load egg data
eggdata <- read.csv("../data/eggdata_for_app.csv")
```

## Species table

``` r
# Make table
eggdata %>% 
  count(Family_ACGC, Genus_ACGC, Common_Name_ACGC) %>% 
  rename(
    "Family" = "Family_ACGC",
    "Genus" = "Genus_ACGC",
    "Common Name" = "Common_Name_ACGC",
    "Number of Eggs in Training Data" = "n"
  ) %>%
  knitr::kable(align = "lllc") %>%
  kableExtra::column_spec(
    column = 1:4,
    width = "3cm"
  ) %>%
  kableExtra::collapse_rows(
    columns = 1:3,
    valign = "top"
  )
```

<table>

<thead>

<tr>

<th style="text-align:left;">

Family

</th>

<th style="text-align:left;">

Genus

</th>

<th style="text-align:left;">

Common Name

</th>

<th style="text-align:center;">

Number of Eggs in Training Data

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;width: 3cm; vertical-align: top !important;" rowspan="7">

Catostomidae

</td>

<td style="text-align:left;width: 3cm; vertical-align: top !important;" rowspan="3">

Carpiodes

</td>

<td style="text-align:left;width: 3cm; ">

Carpsuckers sp.

</td>

<td style="text-align:center;width: 3cm; ">

1

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Quillback

</td>

<td style="text-align:center;width: 3cm; ">

1

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

River Carpsucker

</td>

<td style="text-align:center;width: 3cm; ">

8

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; vertical-align: top !important;" rowspan="4">

Ictiobus

</td>

<td style="text-align:left;width: 3cm; ">

Bigmouth Buffalo

</td>

<td style="text-align:center;width: 3cm; ">

7

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Black Buffalo

</td>

<td style="text-align:center;width: 3cm; ">

1

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Buffalo sp.

</td>

<td style="text-align:center;width: 3cm; ">

10

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Smallmouth Buffalo

</td>

<td style="text-align:center;width: 3cm; ">

2

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; vertical-align: top !important;" rowspan="2">

Clupeidae

</td>

<td style="text-align:left;width: 3cm; ">

Alosa

</td>

<td style="text-align:left;width: 3cm; ">

Skipjack Shad

</td>

<td style="text-align:center;width: 3cm; ">

1

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Dorosoma

</td>

<td style="text-align:left;width: 3cm; ">

Gizzard Shad

</td>

<td style="text-align:center;width: 3cm; ">

2

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; vertical-align: top !important;" rowspan="10">

Cyprinidae

</td>

<td style="text-align:left;width: 3cm; ">

Cyprinella

</td>

<td style="text-align:left;width: 3cm; ">

Spotfin Shiner

</td>

<td style="text-align:center;width: 3cm; ">

6

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Luxilus

</td>

<td style="text-align:left;width: 3cm; ">

Common Shiner

</td>

<td style="text-align:center;width: 3cm; ">

1

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; vertical-align: top !important;" rowspan="2">

Macrhybopsis

</td>

<td style="text-align:left;width: 3cm; ">

Silver Chub

</td>

<td style="text-align:center;width: 3cm; ">

36

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Speckled Chub

</td>

<td style="text-align:center;width: 3cm; ">

28

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; vertical-align: top !important;" rowspan="5">

Notropis

</td>

<td style="text-align:left;width: 3cm; ">

Channel Shiner

</td>

<td style="text-align:center;width: 3cm; ">

32

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Emerald Shiner

</td>

<td style="text-align:center;width: 3cm; ">

201

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

River Shiner

</td>

<td style="text-align:center;width: 3cm; ">

16

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Sand Shiner

</td>

<td style="text-align:center;width: 3cm; ">

1

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Shiner sp.

</td>

<td style="text-align:center;width: 3cm; ">

70

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Pimephales

</td>

<td style="text-align:left;width: 3cm; ">

Fathead Minnow

</td>

<td style="text-align:center;width: 3cm; ">

5

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Hiodontidae

</td>

<td style="text-align:left;width: 3cm; ">

Hiodon

</td>

<td style="text-align:left;width: 3cm; ">

Goldeye

</td>

<td style="text-align:center;width: 3cm; ">

7

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Invasive Carp

</td>

<td style="text-align:left;width: 3cm; ">

Invasive Carp

</td>

<td style="text-align:left;width: 3cm; ">

Invasive Carp

</td>

<td style="text-align:center;width: 3cm; ">

782

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; vertical-align: top !important;" rowspan="2">

Moronidae

</td>

<td style="text-align:left;width: 3cm; vertical-align: top !important;" rowspan="2">

Morone

</td>

<td style="text-align:left;width: 3cm; ">

Striped Bass

</td>

<td style="text-align:center;width: 3cm; ">

17

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

White Bass

</td>

<td style="text-align:center;width: 3cm; ">

1

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; vertical-align: top !important;" rowspan="3">

Percidae

</td>

<td style="text-align:left;width: 3cm; ">

Etheostoma

</td>

<td style="text-align:left;width: 3cm; ">

Banded Darter

</td>

<td style="text-align:center;width: 3cm; ">

1

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Percina

</td>

<td style="text-align:left;width: 3cm; ">

Common Logperch

</td>

<td style="text-align:center;width: 3cm; ">

1

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Sander

</td>

<td style="text-align:left;width: 3cm; ">

Walleye

</td>

<td style="text-align:center;width: 3cm; ">

2

</td>

</tr>

<tr>

<td style="text-align:left;width: 3cm; ">

Sciaenidae

</td>

<td style="text-align:left;width: 3cm; ">

Aplodinotus

</td>

<td style="text-align:left;width: 3cm; ">

Freshwater Drum

</td>

<td style="text-align:center;width: 3cm; ">

738

</td>

</tr>

</tbody>

</table>

## Computing variable levels and extreme values

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
    Yolk_to_Membrane_Ratio,
    Yolk_Ave,
    Yolk_SD,
    Yolk_CV
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
    ##    variable                   min     max
    ##    <chr>                    <dbl>   <dbl>
    ##  1 Conductivity           274     781    
    ##  2 Larval_Length            0       5.09 
    ##  3 Membrane_Ave             0.728   5.49 
    ##  4 Membrane_CV              0.001   0.52 
    ##  5 Membrane_SD              0.001   1.47 
    ##  6 Temperature             11      30.7  
    ##  7 Yolk_Ave                 0.434   4.37 
    ##  8 Yolk_CV                  0.003   0.724
    ##  9 Yolk_SD                  0.005   1.38 
    ## 10 Yolk_to_Membrane_Ratio   0.257   1.14
