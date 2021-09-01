
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IxPopDyMod: A framework for Ixodidae Population Dynamics Models

## Install source code

First install the source code off of this page. This includes the three
`.R` files. You will also need to load the necessary packages.

``` r
source('core_functions.R')
source('graphing_functions.R')
source('testing_functions.R')

library(tidyverse)
library(tibble)
library(readr)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(igraph)
library(knitr)
```

## Specifying the model

This package is designed to help the user specify, run, and then
visualize and analyze the results of Ixodidae (hard-bodied ticks)
population dynamics models. Such population dynamics models exist in the
literature, but the source code to run them is not always available. We
wanted to provide an easy way for these models to be written and shared.

To specify the model the user must provide two `.csv` files. One giving
the life stage transitions and one giving parameters for the functions
describing those transitions.

``` r
tick_transitions <- read_csv('inputs/2021-03-02_Dave_test/tick_transitions.csv')
tick_params <- read_csv('inputs/2021-03-02_Dave_test/tick_parameters.csv') %>% arrange(host_spp)
life_stages <- tick_transitions %>% pull(from) %>% unique()
```

### The tick transtions file

``` r
kable(tick_transitions)
```

| from  | to    | transition\_fun | delay | source            | pred1 | pred2 |
|:------|:------|:----------------|------:|:------------------|:------|:------|
| \_\_e | q\_l  | expo\_fun       |     1 | Ogden et al. 2004 | temp  | NA    |
| \_\_e | m     | constant\_fun   |     1 | Ogden et al. 2005 | NA    | NA    |
| q\_l  | m     | constant\_fun   |     0 | Ogden et al. 2005 | NA    | NA    |
| q\_n  | m     | constant\_fun   |     0 | Ogden et al. 2005 | NA    | NA    |
| q\_a  | m     | constant\_fun   |     0 | Ogden et al. 2005 | NA    | NA    |
| e\_l  | m     | constant\_fun   |     1 | Ogden et al. 2005 | NA    | NA    |
| e\_n  | m     | constant\_fun   |     1 | Ogden et al. 2005 | NA    | NA    |
| e\_a  | m     | constant\_fun   |     1 | Ogden et al. 2005 | NA    | NA    |
| r\_a  | m     | constant\_fun   |     0 | Ogden et al. 2005 | NA    | NA    |
| q\_l  | e\_l  | constant\_fun   |     0 | NA                | NA    | NA    |
| e\_l  | q\_n  | expo\_fun       |     1 | NA                | temp  | NA    |
| q\_n  | e\_n  | constant\_fun   |     0 | NA                | NA    | NA    |
| e\_n  | q\_a  | expo\_fun       |     1 | NA                | temp  | NA    |
| q\_a  | e\_a  | constant\_fun   |     0 | NA                | NA    | NA    |
| e\_a  | r\_a  | expo\_fun       |     1 | NA                | temp  | NA    |
| r\_a  | \_\_e | constant\_fun   |     0 | NA                | NA    | NA    |

The life stage is specified with a three character string. The final
character is for `e` = egg, `l` = larva, `n` = nymph, and `a` = adult.
The middle character is for infected or uninfected. Here we ignore
infecation so leave it as a `_`. The first character is the sub-stage
here we have `q` = questing, `e` = engorged, and `r` = reproductive.
Although additional sub-stages are possible. Each row of the transition
table represents a separate transition between life stages. The
transitions which go `to` the stage `m` stand for mortality. These
transitions can take either one time step (`delay` = 0) or have some
delay (`delay` = 1). A function describes either the probability of that
transition (if delay = 0) or the daily rate at which the transition
happens (if delay = 1). This function can have 0-2 predictor variables,
which are specified in the `pred1` and `pred2` columns.

So, for example, here we have a transition from questing larvae to
engorged larvae (i.e., host finding and successful feeding), which in
this case is a constant function which takes place in one day. In more
complicated models this step could be a function of the host community
and include the delay of how long feeding takes. The next transition
from engorged larvae to questing nymphs (i.e., development and molting)
has a delay and the amount of time it takes is given by the `expo_fun`
which is driven by temperature.

### Transition functions

### Parameters for these functions

### Troubleshooting

The three testing functions allow you to see whether everythign is
expected

``` r
print_all_params()
test_transitions()
test_lifecycles()
```

## Running the model
