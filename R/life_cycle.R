#' `life_cycle` constructor
#'
#' @inheritParams life_cycle
#' @returns a `life_cycle`
#' @noRd
new_life_cycle <- function(...) {

  cycle <- structure(
    list(...),
    class = "life_cycle"
  )

  checkmate::assert_list(cycle, types = "transition", unique = TRUE)

  return(cycle)

}

#' Create a `life_cycle` from a collection of `transition`s
#'
#' @param ... A set of `transition`s
#' @returns a `life_cycle`
#' @export
life_cycle <- function(...) {

}
