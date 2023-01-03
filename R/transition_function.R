#' Constructor for transition functions
new_transition_function <- function(raw_function) {

  checkmate::assert_function(raw_function)

  if (length(formals(raw_function)) < 2) {
    stop(
      "transition functions must have at least 2 arguments, but ",
      "only ", length(formals(raw_function)), " arg(s) were provided.",
      call. = FALSE
    )
  }

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
