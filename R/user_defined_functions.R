# user_defined_functions.R
# TODO rename to something like `preset_transition_functions.R`? We could have
# one file (this one) for transition functions that we supply and then allow
# users to source their own files with functions that would not be part of the
# package source

# 02 functional forms for transition probabilities
## DA Note: in the simple example can we show a simpiler verison of a host comm?

#' Exponential function
#'
#' @param x Predictor 1 in transitions table.
#' @param y Predictor 2 in transitions table. Not used in this function.
#' @param a Parameter `a` in parameters table.
#' @param b Parameter `b` in parameters table.
#'
#' @return Numeric vector of length 1
#'
#' @export
expo_fun <- function(x, y, a, b) ifelse(x>0, a*x^b, 0)

#' Constant function
#' @param x Predictor 1 in transitions table. Not used in this function
#' @param y Predictor 2 in transitions table. Not used in this function
#' @param a Parameter `a` in parameters table.
#'
#' @return Numeric vector of length 1 equal to input parameter `a`
#'
#' @export
constant_fun <- function(x, y, a) a

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
#' @export
find_n_feed <- function(x, y, a, pref, feed_success) {

  if (length(pref) %% length(x) != 0) {
    print(paste('error in find_n_feed, x:', length(x), 'pref:', length(pref)))
  }

  (1 - (1-a)^(sum(x * pref)/sum(pref))) *
    sum(x * pref * feed_success/sum(x*pref))
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
#' @export
feed_fun <- function(x, y, a, pref, q, tmin, tmax) {

  if (length(pref) %% length(x) != 0) {
    print(paste('error in feed_fun, x:', length(x), 'pref:', length(pref)))
  }

  (1 - (1-a)^(sum(x * pref))) *
    ifelse(y>tmin & y<tmax, q*y*(y-tmin)*sqrt(tmax-y), 0)
}

#' Probability of actively questing times constant host finding probability
#' @details
#'  (const prob of finding a host) * (prob of active questing)
#'
#' @param x Predictor 1 in transitions table. Numeric vector of length 1
#'   indicating temperature.
#' @param y Predictor 2 in transitions table. Not used in this function.
#' @param a Parameter `a` in parameters table.
#' @param q Parameter `q` in parameters table. Used in Briere function.
#' @param tmin Parameter `tmin` in parameters table. Indicates minimum
#'   temperature at which ticks actively quest.
#' @param tmax Parameter `tmax` in parameters table. Indicates maximum
#'   temperature at which ticks actively quest.
#'
#' @return Numeric vector of length 1
#'
#' @export
ogden_feed_fun <- function(x, y, a, q, tmin, tmax)
  a * ifelse(x>tmin & x<tmax, q*x*(x-tmin)*sqrt(tmax-x), 0)

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
#' @return Numeric vector of length 1
#'
#' @export
infect_fun <- function(x, y, from_infected, to_infected, host_rc, pref) {

  if (length(pref) %% length(x) != 0) {
    print(paste('error in infect_fun, x:', length(x), 'pref:', length(pref)))
  }

  n_host_spp <- length(host_rc)

  sum(ifelse(rep(from_infected, n_host_spp),
             1, # stay infected
             (ifelse(rep(to_infected, n_host_spp),
                     host_rc, # become infected
                     1 - host_rc))) * # stay uninfected
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
#' @export
density_fun <- function(x, y, a, b, c, pref)
  sum((a + (b * log((c + y * pref * x / sum(pref * x)) / x)))
      * pref * x / sum(pref * x))
