transition_example_a <- function() {
  new_transition(
    from = "a",
    to = "b",
    transition_type = "probability",
    mortality_type = NULL,
    fun = new_transition_function(constant_fun),
    predictors = c(x = "temp", y = "host_density"),
    parameters = new_parameters(a = 1)
  )
}

transition_example_b <- function() {
  b <- transition_example_a()
  b$from <- "b"
  b$to <- "a"
  return(b)
}

life_cycle_example_a <- function() {
  new_life_cycle(
    transition_example_a(),
    transition_example_b()
  )
}
