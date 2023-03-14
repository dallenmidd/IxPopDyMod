#' Calculate multiplicative growth rate of population
#'
#' @param out Model output data frame
#' @return Numeric vector of length one representing growth rate
#'
#' @examples
#' out <- run(config_ex_1)
#' growth_rate(out)
#'
#' @export
growth_rate <- function(out) {
  days <- unique(out$day)
  pops <- sapply(days, function(x) sum(out$pop[out$day == x]))
  lambdas <- pops/stats::lag(pops)
  exp(mean(log(lambdas), na.rm = TRUE))
}
