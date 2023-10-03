#' Calculate multiplicative growth rate of population
#'
#' @param out Model output data frame
#' @returns Numeric vector of length one representing daily growth rate.
#'
#' @examples
#' out <- run(config_ex_1)
#' growth_rate(out)
#'
#' @export
growth_rate <- function(out) {
  days <- unique(out$day)
  pops <- sapply(days, function(x) sum(out$pop[out$day == x]))
  lambdas <- pops / c(NA, pops[1:(length(pops) - 1)])
  exp(mean(log(lambdas), na.rm = TRUE))
}


#' Calculate annual growth rate
#'
#' @param out Model output data frame
#' @returns Numeric vector of length one representing the annual factor by
#' which the total tick population changes. To use this function, it is best
#' to run the model for at least three years.
#'
#' @examples
#' out <- run(config_ex_1)
#' annual_growth_rate(out)
#'
#' @export
annual_growth_rate <- function(out) {
  required_cols <- c("day", "pop")
  if (!all(required_cols %in% names(out))) {
    missing_cols <- setdiff(required_cols, names(out))
    stop("Missing required colunns: ", paste(missing_cols, collapse = ", "))
  }
  daily_data <- data.frame(
    day = unique(out$day)
  )
  daily_data$totalpop <- sapply(daily_data$day, function(x) sum(out$pop[out$day == x]))
  daily_data$yr <- ceiling(daily_data$day / 365)
  years <- unique(daily_data$yr)
  if (length(years) < 2) {
    stop(
      "At least 2 years of data are required to calculate an annual growth rate",
      ", but the input data only has ", length(years), " years"
    )
  }
  maxpop <- sapply(years, function(x) max(daily_data$totalpop[daily_data$yr == x]))
  ratechange <- numeric(length = length(maxpop) - 1)
  for (i in seq_along(ratechange)) ratechange[i] <- maxpop[i + 1] / maxpop[i]
  exp(mean(log(ratechange[1:(length(ratechange) - 1)])))
}
