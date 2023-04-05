transition_example_a <- function() {
  f <- function(x, y, a) a
  environment(f) <- emptyenv()

  transition(
    from = "a",
    to = "b",
    transition_type = "probability",
    mortality_type = NULL,
    fun = new_transition_function(f),
    predictors = list(
      x = predictor_spec("temp"), y = predictor_spec("host_density")
    ),
    parameters = parameters(a = 1)
  )
}

transition_example_b <- function() {
  b <- transition_example_a()
  b$from <- "b"
  b$to <- "a"
  return(b)
}

life_cycle_example_a <- function() {
  life_cycle(
    transition_example_a(),
    transition_example_b()
  )
}

predictors_example_a <- function() {
  predictors(data.frame(
    pred = "temp",
    pred_subcategory = NA,
    j_day = NA,
    value = 1
  ))
}

predictors_example_b <- function() {
  predictors(data.frame(
    pred = c("host_density", "temp"),
    pred_subcategory = NA,
    j_day = NA,
    value = 1
  ))
}

# TODO there are also (old) example configs loaded with the package - need to
# think about what to put in data/ vs tests/helper.R
config_example_a <- function() {
  config(
    cycle = life_cycle_example_a(),
    preds = predictors_example_b(),
    initial_population = c(a = 1L, b = 0L),
    steps = 10L,
    max_duration = 365L
  )
}
