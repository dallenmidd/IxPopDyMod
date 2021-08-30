
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
| :---- | :---- | :-------------- | ----: | :---------------- | :---- | :---- |
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
infection so leave it as a `_`. The first character is the sub-stage
here we have `q` = questing, `e` = engorged, and `r` = reproductive.
Although additional sub-stages are possible. Each row of the transition
table represents a separate transition between life stages. These
transitions can take either one time step (`delay` = 0) or have some
delay (`delay` = 1). A function describes either the probability of that
transition (if `delay` = 0) or the daily rate at which the transition
happens (if `delay` = 1). This function can have 0-2 predictor
variables, which are specified in the `pred1` and `pred2` columns.

So, for example, here we have a transition from questing larvae to
engorged larvae (i.e., host finding and successful feeding), which in
this case is a constant function which takes place in one day. In more
complicated models this step could be a function of the host community
and include the delay of how long feeding takes. The next transition
from engorged larvae to questing nymphs (i.e., development and molting)
has a delay and the amount of time it takes is given by the `expo_fun`
which is driven by temperature.

### Mortality

The transitions which go `to` the stage `m` stand for mortality. If a
transition does not have a delay, then individuals in that life stage
are assumed to *stay* in that life stage if they do not move to the next
life stage or die. So if `q_l` to `e_l` was 0.25 and `q_l` to `m` was
0.05, this would mean 70% of `q_l` stayed in that life stage.

On the other hand if a transition does have a delay it behaves
differently. All individuals are assumed to move from that life stage to
the next, but with a daily rate give. Mortality is added HOW???. So for
example if `e_l` to `q_n` is XXX and `e_l` to `m` is XXX, then XXX.

### Transition functions

Each transition is described by a transition function. These functions
must be defined by the user. For example:

``` r
expo_fun <- function(x, y, a, b) ifelse(x>0, a*x^b, 0)
```

Here `x` and `y` are possible predictor variables. In this case
`expo_fun` takes just one predictors variable, so `y` is not included in
the actual functions. `a` and `b` are parameters which will be given
later. These functions allow the user to specify exactly how each
transition varies with predictors (e.g., how tick development varies
with temperature, how tick questing varies with relative humidity, how
host finding varies with host density,…). Here we load these functions
from an R script:

``` r
source('user_defined_functions.R')
```

### Parameters for these functions

Now that we have life stage transitions and functions which describe
them, we need parameters for those functions. They are given in
`tick_params`.

``` r
kable(tick_params)
```

| from  | to    | param\_name | host\_spp | param\_value | param\_ci\_low | param\_ci\_high | source            |
| :---- | :---- | :---------- | :-------- | -----------: | :------------- | :-------------- | :---------------- |
| …     | m     | a           | NA        |     1.20e-01 | NA             | NA              | NA                |
| \_\_e | q\_l  | a           | NA        |     2.04e-04 | NA             | NA              | Ogden et al. 2004 |
| \_\_e | q\_l  | b           | NA        |     2.27e+00 | NA             | NA              | Ogden et al. 2004 |
| e\_l  | q\_n  | a           | NA        |     6.92e-05 | NA             | NA              | Ogden et al. 2005 |
| e\_l  | q\_n  | b           | NA        |     2.55e+00 | NA             | NA              | Ogden et al. 2005 |
| e\_n  | q\_a  | a           | NA        |     4.38e-03 | NA             | NA              | Ogden et al. 2005 |
| e\_n  | q\_a  | b           | NA        |     1.21e+00 | NA             | NA              | Ogden et al. 2005 |
| e\_a  | r\_a  | a           | NA        |     5.38e-03 | NA             | NA              | Ogden et al. 2004 |
| e\_a  | r\_a  | b           | NA        |     1.42e+00 | NA             | NA              | Ogden et al. 2004 |
| r.a   | \_\_e | a           | NA        |     3.00e+03 | NA             | NA              | Ogden et al. 2005 |
| q\_.  | e\_.  | a           | NA        |     1.00e-01 | NA             | NA              | NA                |

TALK ABOUT how the . works.

### Troubleshooting

The three testing functions allow you to see whether everythign is
expected

``` r
print_all_params()
test_transitions()
test_lifecycles()
```

## Running the model
