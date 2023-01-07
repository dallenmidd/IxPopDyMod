#' `life_cycle` constructor
#'
#' @inheritParams life_cycle
#' @returns a `life_cycle`
#' @noRd
new_life_cycle <- function(...) {

  life_cycle <- structure(
    list(...),
    class = "life_cycle"
  )

  checkmate::assert_list(life_cycle, types = "transition", unique = TRUE)

  return(life_cycle)

}

#' Create a `life_cycle` from a collection of `transition`s
#'
#' @param ... A set of `transition`s
#' @returns a `life_cycle`
#' @export
life_cycle <- function(...) {

}
