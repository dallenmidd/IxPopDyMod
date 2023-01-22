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

predictors_example_a <- function() {
  new_predictors(data.frame(
    pred = "temp",
    pred_subcategory = NA,
    j_day = NA,
    value = 1
  ))
}

# TODO there are also (old) example configs loaded with the package - need to
# think about what to put in data/ vs tests/helper.R
config_example_a <- function() {
  new_config(
    cycle = life_cycle_example_a(),
    preds = predictors_example_a(),
    initial_population = c(a = 1L, b = 0L),
    steps = 10L,
    max_delay = 365L
  )
}
