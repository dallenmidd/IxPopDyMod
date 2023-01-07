#' Create a `life_cycle` from a collection of `transition`s
#'
#' @param transitions a list of `transition`s
#' @returns a `life_cycle`
#' @noRd
new_life_cycle <- function(transitions) {

  checkmate::assert_list(transitions, types = "transition", unique = TRUE)

  life_cycle <- structure(
    transitions,
    class = "life_cycle"
  )

  return(life_cycle)

}
