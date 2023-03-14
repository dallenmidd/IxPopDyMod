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

#' Configuration for Ixodes scapularis population dynamics model from Ogden et
#' al. 2005
#'
#' This model configuration recreates the Ixodes scapularis (blacklegged tick)
#' population dynamics model from Ogden et al. 2005. This is a relatively
#' complete model of tick population dynamics, including the effects of both
#' temperature and the host community on tick life-stage transitions. We include
#' this configuration to show that our package can be used to recreate existing
#' models.
#'
#' @format A \code{\link{config}}
#'
#' \describe{
#'   \item{steps}{Number of time steps to run the model. Here each step
#'   corresponds to one day.}
#'   \item{initial_population}{Named vector of initial population size. Here
#'   the population starts with 10000 questing adults.}
#'   \item{transitions}{A \code{\link{tibble}} giving the transitions between
#'   tick life stages.}
#'   \item{parameters}{A \code{\link{tibble}} with the parameters to the
#'   life-stage transitions functions.}
#'   \item{predictors}{A \code{\link{tibble}} with the average temperature
#'   for each day, and density of hosts over the model run. Here the host
#'   community is stable with 20 deer and 200 rodents.}
#'   \item{max_delay}{The number of time units used for the delay functions.}
#' }
#'
#' @seealso Ogden et al. (2005) \doi{10.1016/j.ijpara.2004.12.013}
#' @examples
#' data(ogden2005)
#' \dontrun{
#' output <- run(ogden2005)
#' graph_population_each_group(output)
#' }
"ogden2005"

#' Configuration for showing how we can modify climate data
#'
#' @format A \code{\link{config}}
"temp_example_config"

#' Configuration for showing how we can modify host community data
#'
#' @format A \code{\link{config}}
"host_example_config"

#' Configuration for showing infection dynamics
#'
#' @format A \code{\link{config}}
"infect_example_config"



#' Configuration for winter tick population dynamics model
#'
#' This is a model configuration based on a literature search on the factors
#' affect the winter tick life cycle. Many of the transitions and parameters
#' in this configuration are drawn from Drew and Samuel (1986). We include this
#' configuration to show that our package is flexible for modeling multiple
#' tick species with different life histories.
#'
#' @seealso Drew and Samuel (1986) \doi{10.1139/z86-105}
#' @seealso Drew and Samuel (1985) \doi{10.7589/0090-3558-21.3.274}
#' @seealso Addison and McLaughlin (1988) \doi{10.2307/3282188}
#' @seealso Ogden et al. (2005) \doi{10.1016/j.ijpara.2004.12.013}
#'
#' @examples
#' data(winter_tick)
#' \dontrun{
#' output <- run(winter_tick)
#' graph_population_each_group(winter_tick)
#' }
"winter_tick"
