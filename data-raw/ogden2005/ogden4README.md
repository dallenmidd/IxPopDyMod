Replicating the Ogden et al. (2005) model
================

This page describes how we replicating the [Ogden et
al. (2005)](https://www.sciencedirect.com/science/article/abs/pii/S0020751905000044)
*Ixodes scapularis* population dynamics model.

## Differences with Odgen et al. (2005)

We were able to largely replicate Odgen et al.’s model directly with our
model structure. Here are the few changes we had to make. - Ogden et
al. has density-dependent reduction fecundity based on the number of
adults on deer. We take fecundity (number of eggs produced) as a
constant. - Ogden et al. handles host finding differently - They give a
weekly probability of host-finding. We translate this into a daily
probability. - They use a curve (See Figure 3) to determine the
probability an active tick quests based on temperature. We approximate
these curves with Briere functions.

## Ogden et al. (2005) Figure 4

Here we show that even with this changes we are able to largely
replicate the output of Odgen et al. (2005)’s model, starting here with
Figure 4.

<img src="https://raw.githubusercontent.com/dallenmidd/IxPopDyMod/master/data-raw/ogden2005/make_ogden_fig4/ogden_fig4a.png" alt="Ogden 4a" width="572"/>

<img src="https://raw.githubusercontent.com/dallenmidd/IxPopDyMod/master/data-raw/ogden2005/make_ogden_fig4/ogden_fig4b.png" alt="Ogden 4b" width="572"/>

<img src="https://raw.githubusercontent.com/dallenmidd/IxPopDyMod/master/data-raw/ogden2005/make_ogden_fig4/ogden_fig4c.png" alt="Alt text" width="572"/>

### Installation

Install the package needed with:

``` r
    library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.8
    ## ✓ tidyr   1.2.0     ✓ stringr 1.4.0
    ## ✓ readr   2.1.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
    pacman::p_load(gridExtra)
    library(IxPopDyMod)
    library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

also install:

     install.packages("IxPopDyMod")

### Create the Tables

We used [PlotDigitizer](https://apps.automeris.io/wpd/) to extract the
observed larvae, nymphs, and adults from Figure 4.

-   observed larvae `ol`
-   observed nymphs `on`
-   observed adults `oa`
-   Ogden et al. predicted larvae `pl`
-   Ogden et al. predicted larvae `pn`
-   Ogden et al. predicted larvae `pa`

``` r
ol <- read_csv('make_ogden_fig4/observed_larvae.csv',
               col_names = c('jday', 'n')) %>% mutate(group = 'ol')
```

    ## Rows: 48 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (2): jday, n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
on <- read_csv('make_ogden_fig4/observed_nymphs.csv',
               col_names = c('jday', 'n')) %>% mutate(group = 'on')
```

    ## Rows: 48 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (2): jday, n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
oa <- read_csv('make_ogden_fig4/observed_adults.csv',
               col_names = c('jday', 'n')) %>% mutate(group = 'oa')
```

    ## Rows: 46 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (2): jday, n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pl <- read_csv('make_ogden_fig4/predicted_larvae.csv',
               col_names = c('jday', 'n')) %>% mutate(group = 'pl')
```

    ## Rows: 48 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (2): jday, n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pn <- read_csv('make_ogden_fig4/predicted_nymphs.csv',
               col_names = c('jday', 'n')) %>% mutate(group = 'pn')
```

    ## Rows: 48 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (2): jday, n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pa <- read_csv('make_ogden_fig4/predicted_adults.csv',
               col_names = c('jday', 'n')) %>% mutate(group = 'pa')
```

    ## Rows: 46 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (2): jday, n
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### Looking At What This Does

`col_names = c('jday', 'n')) %>% mutate(group =' '`

`col_names` specifies what columns to make with the data. Here, `jday`
(Julian Date) and `n` () are what we want to assign to the data already
in the `.csv`

`%>% mutate(group = '')` is saying mutate, or create a new table while
preserving the old one, and add on the column group that labels these
rows as on of the variable names (`on`, `pn`, ect.).

### Putting Together The Tibbles Into One

``` r
    # merge all data
    ogden_fig4_data <- rbind(pl, pn, pa, ol, on, oa) %>%
      arrange(jday, group) %>%
      mutate(jday = round(jday))
```

`rbind` takes all the groups and “glues” them together so they end up in
the in the same columns : jday, n, group. Then this is put into the
variable `df` (for data frame).

`arrange` takes the data and arranges the data in the columns by `jday`
from least to greatest and then by `group`.

lastly, we `mutate` `jday` so that the day is rounded to a whole number

### Run our version of the model

Now we run our version of the model to get the predicted number of
ticks.

``` r
output <- run(ogden2005)
```

    ## [1] "day 100"
    ## [1] "day 200"
    ## [1] "day 300"
    ## [1] "day 400"
    ## [1] "day 500"
    ## [1] "day 600"
    ## [1] "day 700"
    ## [1] "day 800"
    ## [1] "day 900"
    ## [1] "day 1000"
    ## [1] "day 1100"
    ## [1] "day 1200"
    ## [1] "day 1300"
    ## [1] "day 1400"
    ## [1] "day 1500"
    ## [1] "day 1600"
    ## [1] "day 1700"
    ## [1] "day 1800"
    ## [1] "day 1900"
    ## [1] "day 2000"
    ## [1] "day 2100"
    ## [1] "day 2200"
    ## [1] "day 2300"
    ## [1] "day 2400"
    ## [1] "day 2500"
    ## [1] "day 2600"
    ## [1] "day 2700"
    ## [1] "day 2800"
    ## [1] "day 2900"
    ## [1] "day 3000"
    ## [1] "day 3100"
    ## [1] "day 3200"
    ## [1] "day 3300"
    ## [1] "day 3400"

``` r
questing <- output %>%
  # look at data after the model has run for a while and stabilized
  mutate(post_eq_jday = day - 365 * 6) %>%
  filter(post_eq_jday > 0,
         post_eq_jday < 365 * 2,
         process == 'a')
```

``` r
ogden_sampling_days <- ogden_fig4_data %>%
  pull(jday) %>%
  unique()

to_merge <- questing %>%
  filter(post_eq_jday %in% ogden_sampling_days) %>%
  group_by(age_group) %>%
  mutate(pop_fraction = pop/sum(pop)) %>%
  ungroup() %>%
  transmute(jday = post_eq_jday,
            n = pop_fraction * 7, # multiply by constant just to scale so similar to Ogden
            group = str_c(process, age_group))

merged_fig4 <- rbind(ogden_fig4_data, to_merge)

merged4a <- merged_fig4 %>%
  filter(group %in% c('pl', 'al')) %>%
  ggplot(aes(jday, n, shape = group, linetype = group)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  xlab("Day of year") +
  ylab("Questing larvae")

merged4b <- merged_fig4 %>%
  filter(group %in% c('pn', 'an')) %>%
  ggplot(aes(jday, n, shape = group, linetype = group)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  scale_color_viridis_d() +
  xlab("Day of year") +
  ylab("Questing nymphs")

merged4c <- merged_fig4 %>%
  filter(group %in% c('pa', 'aa')) %>%
  ggplot(aes(jday, n, shape = group, linetype = group)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  scale_color_viridis_d() +
  xlab("Day of year") +
  ylab("Questing adults")

grid.arrange(merged4a, merged4b, merged4c)
```

![](ogden4README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->