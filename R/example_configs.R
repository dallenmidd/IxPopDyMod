#' Simple model configuration example
#'
#' This model configuration uses only non-delay transitions, and no transitions
#' depend on predictors (e.g. weather or host community). Parameter values are
#' selected so that the population is stable over time.
#'
#' @format A \code{\link{config}}
#'
#' @examples TODO
#'
"config_ex_1"

#' Simple model configuration example using delays
#'
#' This model configuration uses delay transitions for all transitions except
#' the adult to eggs transition. As in `config_ex_1`, no transitions depend
#' on predictors, and the population is stable over time.
#'
#' @format A \code{\link{config}}
#'
#' @examples TODO
#'
"config_ex_2"
