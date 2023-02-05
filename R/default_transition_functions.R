# Default transition functions. Users may use custom functions by loading them
# into the environment, e.g. by sourcing a file with function definitions.

#' Exponential function
#'
#' @param x Predictor 1 in transitions table.
#' @param a Parameter `a` in parameters table.
#' @param b Parameter `b` in parameters table.
#'
#' @return Numeric vector of length 1
#'
#' @examples
#' expo_fun(.5, NULL, .1, .3)
#'
#' @export
expo_fun <- function(x, a, b) ifelse(x > 0, a * x^b, 0)

#' Constant function
#' @param a Parameter `a` in parameters table.
#'
#' @return Numeric vector of length 1 equal to input parameter `a`
#'
#' @examples
#' constant_fun(NULL, NULL, 1)
#' @export
constant_fun <- function(a) a

#' Probability of finding a host and successfully feeding on it
#' @param x Predictor 1 in transitions table. Numeric vector indicating host
#'   density for each of the host species. Length should be equal to the number
#'   of host species.
#' @param y Predictor 2 in transitions table. Not used in this function.
#' @param a Parameter `a` in parameters table.
#' @param pref Parameters named `pref` in parameters table. Numeric vector of
#'   length equal to the number of host species. Values are the preference for
#'   ticks in a given transition for each host species.
#' @param feed_success Parameters named `feed success` in parameters table.
#'   Numeric vector of length equal to the number of host species. Values are
#'   the feeding success rate for ticks in a given transition while feeding on
#'   each host species.
#'
#' @return Numeric vector of length 1 indicating probability that ticks find any
#'   host and then successfully feed on that host.
#'
#' @examples
#' find_n_feed(10, NULL, .1, 1, .5)
#' find_n_feed(runif(2) * 10, NULL, .1, runif(2), runif(2))
#'
#' @export
find_n_feed <- function(x, y, a, pref, feed_success) {
  if (length(pref) %% length(x) != 0) {
    print(paste("error in find_n_feed, x:", length(x), "pref:", length(pref)))
  }

  (1 - (1 - a)^(sum(x * pref) / sum(pref))) *
    sum(x * pref * feed_success / sum(x * pref))
}

#' Probability of actively questing and then finding a host
#'
#' @details
#' Product of binomial and Briere functions
#' (prob of finding a host) * (prob of active questing)
#'
#' @param x Predictor 1 in transitions table. Numeric vector indicating host
#'   density for each of the host species. Length should be equal to the number
#'   of host species.
#' @param y Predictor 2 in transitions table. Numeric vector of length 1
#'   indicating temperature.
#' @param a Parameter `a` in parameters table.
#' @param pref Parameters named `pref` in parameters table. Numeric vector of
#'   length equal to the number of host species. Values are the preference for
#'   ticks in a given transition for each host species.
#' @param q Parameter `q` in parameters table. Used in Briere function.
#' @param tmin Parameter `tmin` in parameters table. Indicates minimum
#'   temperature at which ticks actively quest.
#' @param tmax Parameter `tmax` in parameters table. Indicates maximum
#'   temperature at which ticks actively quest.
#'
#' @return Numeric vector of length 1
#'
#' @examples
#' feed_fun(10, 30, .001, .1, .5, 20, 40)
#'
#' @export
feed_fun <- function(x, y, a, pref, q, tmin, tmax) {
  if (length(pref) %% length(x) != 0) {
    print(paste("error in feed_fun, x:", length(x), "pref:", length(pref)))
  }

  (1 - (1 - a)^(sum(x * pref))) *
    ifelse(y > tmin & y < tmax, q * y * (y - tmin) * sqrt(tmax - y), 0)
}

#' Probability of actively questing times constant host finding probability
#' @details
#'  (const prob of finding a host) * (prob of active questing)
#'
#' @seealso
#' Based on Ogden et al. (2005) \doi{10.1016/j.ijpara.2004.12.013}
#'
#' @param x Predictor 1 in transitions table. Numeric vector of length 1
#'   indicating temperature.
#' @param a Parameter `a` in parameters table.
#' @param q Parameter `q` in parameters table. Used in Briere function.
#' @param tmin Parameter `tmin` in parameters table. Indicates minimum
#'   temperature at which ticks actively quest.
#' @param tmax Parameter `tmax` in parameters table. Indicates maximum
#'   temperature at which ticks actively quest.
#'
#' @return Numeric vector of length 1
#'
#' @examples
#' ogden_feed_fun(30, NULL, .03, .01, 10, 35)
#'
#' @export
ogden_feed_fun <- function(x, a, q, tmin, tmax) {
  a * ifelse(x > tmin & x < tmax, q * x * (x - tmin) * sqrt(tmax - x), 0)
}

#' Probability that a feeding tick becomes engorged infected or uninfected
#'
#' @details
#' Since density dependent mortality is subtracted later, in this
#' function we assume that all feeding ticks feed successfully and become
#' engorged.
#'
#' @param x Predictor 1 in transitions table. Numeric vector indicating host
#'   density for each of the host species. Length should be equal to the number
#'   of host species.
#' @param y Predictor 2 in transitions table. Not used in this function.
#' @param from_infected Parameter `from_infected` in parameters table. Value
#'   should be 1 if transition is from an infected tick stage, 0 otherwise.
#' @param to_infected Parameter `to_infected` in parameters table. Value should
#'   be 1 if transition is to an infected tick stage, 0 otherwise.
#' @param host_rc Parameters named `host_rc` in parameters table. Numeric vector
#'  of length equal to the number of host species. Values are the host reservoir
#'  competence for each host species.
#' @param pref Parameters named `pref` in parameters table. Numeric vector of
#'   length equal to the number of host species. Values are the preference for
#'   ticks in a given transition for each host species.
#'
#'
#' @return Numeric vector of length 1
#'
#' @examples
#' infect_fun(10, NULL, 0, 0, .3, 1)
#' infect_fun(10, NULL, 0, 1, .3, 1)
#' infect_fun(10, NULL, 1, 1, .3, 1)
#'
#' @export
infect_fun <- function(x, y, from_infected, to_infected, host_rc, pref) {
  if (length(pref) %% length(x) != 0) {
    print(paste("error in infect_fun, x:", length(x), "pref:", length(pref)))
  }

  n_host_spp <- length(host_rc)

  sum(ifelse(rep(from_infected, n_host_spp),
    1, # stay infected
    (ifelse(rep(to_infected, n_host_spp),
      host_rc, # become infected
      1 - host_rc
    ))
  ) * # stay uninfected
    (x * pref) / sum(x * pref)) # chance a tick is feeding on each host type
}

#' Density dependent mortality
#' @param x Predictor 1 in transitions table. Numeric vector indicating host
#'   density for each of the host species. Length should be equal to the number
#'   of host species.
#' @param y Predictor 2 in transitions table. Number of feeding ticks in life
#'   stages specified by predictor 2.
#' @param a Parameter `a` in parameters table.
#' @param b Parameter `b` in parameters table.
#' @param c Parameter `c` in parameters table.
#' @param pref Parameters named `pref` in parameters table. Numeric vector of
#'   length equal to the number of host species. Values are the preference for
#'   ticks in a given transition for each host species.
#'
#' @return Numeric vector of length 1, indicating mortality rate
#'
#' @examples
#' density_fun(c(10, 20), 100, .1, .3, .2, c(.5, .8))
#'
#' @export
density_fun <- function(x, y, a, b, c, pref) {
  sum((a + (b * log((c + y * pref * x / sum(pref * x)) / x)))
  * pref * x / sum(pref * x))
}

#' Mortality as a function of whether there is a snow on the ground
#'
#' @param x amount of snow on ground
#' @param y not used in this transition function
#' @param no_snow_mort mortality with no snow on the ground
#' @param snow_mort mortality with snow on the ground
snow_cover_fun <- function(x, y, no_snow_mort, snow_mort) {
  # only get the snow cover for day 1
  x <- x[1]

  if (is.na(x)) {
    stop("x must not be NA")
  }

  if (x > 0) {
    snow_mort
  } else {
    no_snow_mort
  }
}
