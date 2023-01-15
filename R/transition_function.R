# TODO don't need this as a dedicated class anymore if the only check we're
# doing is whether the input is a function
#' Constructor for transition functions
new_transition_function <- function(fun) {

  checkmate::assert_function(fun)

  structure(
    fun,
    class = "transition_function"
  )
}

#' Return whether a transition function is custom or defined in the package
transition_function_is_custom <- function(fun) {
  env <- environmentName(environment(fun))
  env != "IxPopDyMod"
}