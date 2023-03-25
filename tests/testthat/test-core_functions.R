test_that("`life_stages()` works with `ogden2005` data", {
  expected_life_stages <- c(
    "__e", "e_l", "e_n", "a_l", "a_n", "h_l", "q_l", "e_a", "r_a", "a_a", "q_a",
    "q_n"
  )
  expect_equal(life_stages(ogden2005$cycle), expected_life_stages)
})

test_that("`life_stages()` works with `config_ex_1` data", {
  expected_life_stages <- c("__e", "__l", "__n", "__a")
  expect_equal(life_stages(config_ex_1$cycle), expected_life_stages)
})

test_that("`life_stages()` works with `config_ex_2` data", {
  expected_life_stages <- c("__e", "__l", "__n", "__a")
  expect_equal(life_stages(config_ex_2$cycle), expected_life_stages)
})

test_that("`get_pred_from_table()` works with constant predictors", {
  expect_equal(
    get_pred_from_table(1, "host_den", ogden2005$preds),
    c(deer = 20, mouse = 200)
  )
  expect_equal(
    get_pred_from_table(1:10, "host_den", ogden2005$preds),
    c(deer = 20, mouse = 200)
  )
})

test_that("`get_pred_from_table()` works with variable predictors", {
  expect_equal(
    get_pred_from_table(1, "temp", ogden2005$preds),
    0
  )
  expect_equal(
    get_pred_from_table(1:10, "temp", ogden2005$preds),
    rep(0, 10)
  )
})

test_that("`get_tick_den()` works", {

  # Arrange
  pop <- empty_population_matrix(life_stages = c("a", "b", "c"), steps = 3)
  pop[] <- 1:9

  # Act
  result <- get_tick_den(
    time = 2,
    pred = "a|b",
    population = pop,
    developing_population = pop
  )

  # Assert
  expect_equal(result, 18)
})

# TODO update get_pred() tests to test possible `predictors_spec_node` values
test_that("`get_pred()` works for host density data with or without delay", {
  # Arrange
  predictors <- data.frame(
    value = c(1, 2),
    j_day = NA,
    pred = "host_den",
    pred_subcategory = c("species a", "species b")
  )

  expected <- c("species a" = 1, "species b" = 2)

  # Act
  # host density predictor value should be same regardless of whether transition
  # is a delay
  result_delay <- get_pred(
    time = 1L, pred = predictors_spec_node("host_den"), is_delay = TRUE, population = matrix(),
    developing_population = matrix(), max_delay = 365L, predictors = predictors
  )

  result_no_delay <- get_pred(
    time = 1L, pred = predictors_spec_node("host_den"), is_delay = FALSE, population = matrix(),
    developing_population = matrix(), max_delay = 365L, predictors = predictors
  )

  # Assert
  expect_equal(result_delay, expected)
  expect_equal(result_no_delay, expected)
})

test_that("`get_pred()` works with tick density data with or without delay", {
  # Arrange
  population <- empty_population_matrix(c("a", "b", "c"), 2)
  population[] <- 1:6
  expected <- 6

  # Act
  # tick density predictor value should be same regardless of whether transition
  # is a delay
  result_delay <- get_pred(
    time = 1L, pred = predictors_spec_node("[ab]"), is_delay = TRUE, population = population,
    developing_population = population, max_delay = 365L, predictors = data.frame()
  )

  result_no_delay <- get_pred(
    time = 1L, pred = predictors_spec_node("[ab]"), is_delay = FALSE, population = population,
    developing_population = population, max_delay = 365L, predictors = data.frame()
  )

  # Assert
  expect_equal(result_delay, expected)
  expect_equal(result_no_delay, expected)
})

test_that("`get_pred()` works with predictors in table with no delay", {
  predictors <- data.frame(
    value = 11:20,
    j_day = 1:10,
    pred = "temp"
  )

  result <- get_pred(
    time = 5,
    pred = predictors_spec_node("temp"),
    is_delay = FALSE,
    population = matrix(),
    developing_population = matrix(),
    max_delay = 365L,
    predictors = predictors
  )

  expect_equal(result, 15)
})

test_that("`get_pred()` works with predictors in table with delay", {
  predictors <- data.frame(
    value = 11:20,
    j_day = 1:10,
    pred = "temp"
  )

  result <- get_pred(
    time = 5,
    pred = predictors_spec_node("temp", first_day_only = FALSE),
    is_delay = TRUE,
    population = matrix(),
    developing_population = matrix(),
    max_delay = 365L,
    predictors = predictors
  )

  expect_equal(result, 15:20)
})

test_that("`get_transition_value()` works with no predictors and probability-based transition", {
  # Arrange
  t <- transition(
    from = "a",
    to = "b",
    fun = function(c) c,
    transition_type = "probability",
    parameters = c("c" = 5)
  )

  # Act
  result <- get_transition_value(
    time = 1,
    transition = t,
    population = empty_population_matrix(c("a", "b"), 10L),
    developing_population = empty_population_matrix(c("a", "b"), 10L),
    max_duration = 365L,
    predictors = data.frame()
  )

  # Assert
  expect_equal(result, 5)
})

test_that("`get_transition_value()` works with no predictors and duration-based transition", {
  # Arrange
  t <- transition(
    from = "a",
    to = "b",
    fun = function(c) c,
    transition_type = "duration",
    parameters = c("c" = 5)
  )

  # Act
  result <- get_transition_value(
    time = 1,
    transition = t,
    population = empty_population_matrix(c("a", "b"), 10L),
    developing_population = empty_population_matrix(c("a", "b"), 10L),
    max_duration = 365L,
    predictors = data.frame()
  )

  # Assert
  # TODO should be a vector of length > 1
  expect_equal(result, 5)
})

test_that("`get_transition_value()` works with a predictor that varies over time
  and a duration-based transition", {
  # Arrange
  t <- transition(
    from = "a",
    to = "b",
    fun = function(x) x,
    transition_type = "duration",
    predictors = list(x = predictors_spec_node("temp", first_day_only = FALSE))
  )

  predictors <- new_predictors(data.frame(
    pred = "temp",
    pred_subcategory = NA,
    j_day = 1:10,
    value = 11:20
  ))


  # Act
  result <- get_transition_value(
    time = 1,
    transition = t,
    population = empty_population_matrix(c("a", "b"), 10L),
    developing_population = empty_population_matrix(c("a", "b"), 10L),
    max_duration = 365L,
    predictors = predictors
  )

  # Assert
  expect_equal(result, 11:20)
})

test_that("`get_transition_value()` works with a predictor that varies over time
 and a probability-based transition", {
  # Arrange
  t <- transition(
    from = "a",
    to = "b",
    fun = function(x) x,
    transition_type = "probability",
    predictors = list(x = predictors_spec_node("temp"))
  )

  predictors <- new_predictors(data.frame(
    pred = "temp",
    pred_subcategory = NA,
    j_day = 1:10,
    value = 11:20
  ))


  # Act
  result <- get_transition_value(
    time = 1,
    transition = t,
    population = empty_population_matrix(c("a", "b"), 10L),
    developing_population = empty_population_matrix(c("a", "b"), 10L),
    max_duration = 365L,
    predictors = predictors
  )

  # Assert
  expect_equal(result, 11)
})

test_that("parameters and predictors get reordered to same order", {

  t <- transition(
    from = "a",
    to = "b",
    fun = function(x, y) sum(x * y),
    transition_type = "probability",
    predictors = list(x = predictors_spec_node("host_den")),
    parameters = parameters(y = c("mouse" = 1, "deer" = 2, "squirrel" = 3))
  )

  predictors <- predictors(data.frame(
    pred = "host_den",
    pred_subcategory = c("deer", "squirrel", "mouse"),
    value = c(2, 3, 1),
    j_day = NA
  ))

  inputs <- get_transition_inputs_unevaluated(
    time = 1,
    transition = t,
    population = empty_population_matrix(c("a", "b"), 10L),
    developing_population = empty_population_matrix(c("a", "b"), 10L),
    max_duration = 365L,
    predictors = predictors
  )

  # in this case, we know that there's just one parameter and predictor element
  # each, so we unlist by getting the first element
  param_names <- names(inputs$parameters[[1]])
  pred_names <- names(inputs$predictors[[1]])

  # Parameter and predictor names (and in this case, values) are ordered the
  # same way, so calculations using them will be performed correctly
  expect_identical(pred_names, param_names)
})


test_that("`gen_transition_matrix() works with `config_ex_1`", {
  life_stages <- life_stages(config_ex_1$cycle)

  expected <- empty_transition_matrix(life_stages)
  expected["__a", "__e"] <- 1000
  expected["__e", "__l"] <- 1
  expected["__l", "__n"] <- 0.01
  expected["__n", "__a"] <- 0.1

  result <- gen_transition_matrix(
    1,
    empty_population_matrix(life_stages = life_stages, steps = 1),
    empty_population_matrix(life_stages = life_stages, steps = 1),
    config_ex_1$cycle,
    NULL
  )

  expect_equal(result, expected)
})

test_that("`gen_transition_matrix()` works with `config_ex_2`", {
  life_stages <- life_stages(config_ex_2$cycle)
  expected <- empty_transition_matrix(life_stages)
  expected["__a", "__e"] <- 489.3045

  result <- gen_transition_matrix(
    1,
    empty_population_matrix(life_stages = life_stages, steps = 1),
    empty_population_matrix(life_stages = life_stages, steps = 1),
    config_ex_2$cycle,
    NULL
  )

  expect_equal(result, expected)
})

test_that("`gen_transition_matrix()` works with `ogden2005`", {
  life_stages <- life_stages(ogden2005$cycle)

  population <- empty_population_matrix(life_stages, 200)
  population[] <- 1

  expect_snapshot(gen_transition_matrix(
    # Using a time in the middle of the year when temperature is higher and more
    # of the transitions will have nonzero values.
    time = 150,
    population = population,
    developing_population = population,
    tick_transitions = ogden2005$cycle,
    predictors = ogden2005$preds
  ))
})


test_that("`gen_transition_matrix()` works with life_cycle_example_a()", {
  life_stages <- life_stages(life_cycle_example_a())
  result <- gen_transition_matrix(
    time = 1,
    population = empty_population_matrix(life_stages = life_stages, steps = 1),
    developing_population = empty_population_matrix(life_stages = life_stages, steps = 1),
    tick_transitions = life_cycle_example_a(),
    predictors = predictors_example_b()
  )
  expect_identical(
    result,
    matrix(
      c(0, 1, 1, 0),
      ncol = 2,
      dimnames = list(c("a", "b"), c("a", "b")))
  )
})

test_that("model output for `config_ex_1` stays the same", {
  # testthat::skip("long running")
  expect_snapshot_value(run(config_ex_1, progress = FALSE), style = "serialize")
})

test_that("model output for `config_ex_2` stays the same", {
  # This test was producing different results on an M1 mac on R versions
  # greater than 4.1.1 (or 4.1.2?), versus on an intel Mac on R 4.1.1 and in
  # GitHub actions which used R 4.2.3. It appears to be due to a floating
  # point error.
  #
  # Specifically, the number of days that a duration-based transition
  # lasts is determined by the first day that the cumulative sum of the
  # daily transition probabilities > 1. This number was being calculated
  # differently on the different systems - specifically for the transition
  # to the life stage '__n'.

  # skipped on CRAN because it is long-running
  # testthat::skip("long running")
  testthat::skip_on_cran()
  expect_snapshot_value(run(config_ex_2, progress = FALSE), style = "serialize")
})

# NOTE tests on new core_functions.R methods ----------------------------------
test_that("empty_delay_array snapshot", {
  expect_snapshot(empty_delay_array(c("a", "b"), 1, 1))
})

test_that("empty_population_matrix snapshot", {
  expect_snapshot(empty_population_matrix(c("a", "b"), 3))
})

test_that("empty_transition_matrix snpashot", {
  expect_snapshot(empty_transition_matrix(life_stages = c("a", "b")))
})

test_that("set_initial_population snapshot", {
  population <- empty_population_matrix(c("a", "b"), 3)
  expect_snapshot(set_initial_population(population, c("b" = 10)))
})

test_that("model output for ogden config stays the same", {
  testthat::skip_on_cran()

  # reducing steps to a year to reduce run time
  cfg <- ogden2005
  cfg$steps <- 365
  expect_snapshot_value(run(cfg, progress = FALSE), style = "serialize")
})

test_that("update_delay_arr works", {
  cfg <- config_example_a()
  cfg$steps <- 2
  cfg$max_duration <- 2
  life_stages <- life_stages(cfg$cycle)
  expect_snapshot(update_delay_arr(
    time = 2,
    delay_arr = empty_delay_array(life_stages, cfg$steps, cfg$max_duration),
    population = empty_population_matrix(life_stages, cfg$steps),
    developing_population = empty_population_matrix(life_stages, cfg$steps),
    tick_transitions = cfg$cycle,
    max_delay = cfg$max_duration,
    predictors = cfg$predictors
  ))
})

test_that("population_matrix_to_output_df works", {
  matrix <- empty_population_matrix(life_stages = c("a", "b", "c"), steps = 2L)
  matrix[, ] <- 1:6

  expected <- data.frame(
    day = as.integer(c(1, 1, 1, 2, 2, 2)),
    stage = rep(c("a", "b", "c"), 2),
    pop = as.double(1:6)
  )

  expect_identical(
    as.data.frame(population_matrix_to_output_df(matrix)),
    expected
  )
})
