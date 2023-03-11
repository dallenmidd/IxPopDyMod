#' Calculate multiplicative growth rate of population
#'
#' @importFrom dplyr group_by summarise filter mutate
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
  out %>%
    group_by(.data$day) %>%
    summarise(tot = sum(.data$pop)) %>%
    mutate(lambda = .data$tot / lag(.data$tot)) %>%
    filter(is.finite(.data$lambda), .data$lambda > 0) %>%
    summarise(lambda = exp(mean(log(.data$lambda)))) %>%
    as.numeric()
}
