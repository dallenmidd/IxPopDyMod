#' Simple model configuration example
#'
#' This model configuration uses only non-delay transitions, and no transitions
#' depend on predictors (e.g. weather or host community). Parameter values are
#' selected so that the population is stable over time.
#'
#' @format A \code{\link{config}}
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
"config_ex_2"

#' Configuration for Ixodes scapularis population dynamics model from Ogden et al. 2005
#'
#' This model configuration recreates the Ixodes scaularis (blacklegged tick)
#' population dynamics model from Ogden et al. 2005. This is a relatively complete
#' model of tick population dynamics, including the effects of both temperature and
#' the host community on tick life-stage transitions. We include this configuration
#' to show that our package can be used to recreate existing models.
#'
#' @format A \code{\link{config}}
#'
#' @seealso
#' Based on Ogden et al. (2005) \doi{10.1016/j.ijpara.2004.12.013}
#'
"ogden2005"
