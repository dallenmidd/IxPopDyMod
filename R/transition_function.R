#' Constructor for transition functions
new_transition_function <- function(raw_function) {

  stopifnot(is.function(raw_function))

  stopifnot(length(formals(raw_function)) >= 2)

  structure(
    raw_function,
    class = "transition_function"
  )
}

#' Return whether a transition function is custom or defined in the package
transition_function_is_custom <- function(fun) {
  env <- environmentName(environment(fun))
  env != "IxPopDyMod"
}

#' Return the names of the predictor arguments for a transition function
#'
#' @param fun a transition function
#' @returns a character vector
#' @noRd
get_predictor_names <- function(fun) {
  # TODO is there a way to not use order to determine what is a predictor/param?
  names(formals(fun)[1:2])
}

#' Return the names of the parameter arguments for a transition function
#' @param fun a transition function
#' @returns a character vector
#' @noRd
get_parameter_names <- function(fun) {
  args <- names(formals(fun))
  # TODO is there a way to not use order to determine what is a predictor/param?
  if (length(args) > 2) {
    return(args[3:length(args)])
  }
  return()
}
