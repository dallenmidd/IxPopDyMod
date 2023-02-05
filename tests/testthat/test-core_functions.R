test_that("`age()` works", {
  expect_equal(age("__l"), "l")
  expect_equal(age("e_a"), "a")
  expect_equal(age("n"), "n")
  expect_equal(age("i"), "i")
})

test_that("`process()` works", {
  # TODO this should probably return ""
  expect_equal(process("e"), "e")
  expect_equal(process("eil"), "e")
})

test_that("`infected()` works", {
  expect_equal(infected("eil"), TRUE)
  expect_equal(infected("eul"), FALSE)
  expect_equal(infected("e_l"), FALSE)
  expect_equal(infected("e.l"), FALSE)

  # TODO probably don't want this case to pass
  expect_equal(infected("i"), TRUE)
})

test_that("`get_life_stages()` works with `ogden2005` data", {
  expected_life_stages <- c(
    "__e", "e_l", "e_n", "a_l", "a_n", "h_l", "q_l", "e_a", "r_a", "a_a", "q_a",
    "q_n"
  )
  expect_equal(get_life_stages(ogden2005$transitions), expected_life_stages)
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
    get_pred_from_table(1, "host_den", ogden2005$predictors),
    c(20, 200)
  )
  expect_equal(
    get_pred_from_table(1:10, "host_den", ogden2005$predictors),
    c(20, 200)
  )
})

test_that("`get_pred_from_table()` works with variable predictors", {
  expect_equal(
    get_pred_from_table(1, "temp", ogden2005$predictors),
    0
  )
  expect_equal(
    get_pred_from_table(1:10, "temp", ogden2005$predictors),
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

test_that("`get_pred()` works for host density data with or without delay", {
  # Arrange
  predictors <- data.frame(
    value = c(1, 2),
    j_day = NA,
    pred = "host_den",
    pred_subcategory = c("species a", "species b")
  )

  expected <- c(1, 2)

  # Act
  # host density predictor value should be same regardless of whether transition
  # is a delay
  result_delay <- get_pred(
    time = 1L, pred = "host_den", is_delay = TRUE, population = matrix(),
    developing_population = matrix(), max_delay = 365L, predictors = predictors
  )

  result_no_delay <- get_pred(
    time = 1L, pred = "host_den", is_delay = FALSE, population = matrix(),
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
    time = 1L, pred = "[ab]", is_delay = TRUE, population = population,
    developing_population = population, max_delay = 365L, predictors = data.frame()
  )

  result_no_delay <- get_pred(
    time = 1L, pred = "[ab]", is_delay = FALSE, population = population,
    developing_population = population, max_delay = 365L, predictors = data.frame()
  )

  # Assert
  expect_equal(result_delay, expected)
  expect_equal(result_no_delay, expected)
})

test_that("`get_pred()` works with no predictor with or without delay", {
  result_delay <- get_pred(
    time = 1, pred = NA, is_delay = TRUE, population = matrix(),
    developing_population = matrix(), max_delay = 365L, predictors = data.frame()
  )

  result_no_delay <- get_pred(
    time = 1, pred = NA, is_delay = FALSE, population = matrix(),
    developing_population = matrix(), max_delay = 365L, predictors = data.frame()
  )

  expect_equal(result_delay, NULL)
  expect_equal(result_no_delay, NULL)
})

test_that("`get_pred()` works with predictors in table with no delay", {
  predictors <- data.frame(
    value = 11:20,
    j_day = 1:10,
    pred = "temp"
  )

  result <- get_pred(
    time = 5,
    pred = "temp",
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
    pred = "temp",
    is_delay = TRUE,
    population = matrix(),
    developing_population = matrix(),
    max_delay = 365L,
    predictors = predictors
  )

  expect_equal(result, 15:20)
})

test_that("`get_transition_val()` works with no predictors and no delay", {
  # Arrange
  # TODO also test with a custom function?

  transition_row_with_parameters <- data.frame(
    from = "a",
    to = "b",
    transition_fun = "constant_fun",
    delay = FALSE,
    pred1 = NA,
    pred2 = NA
  )
  transition_row_with_parameters$params_list[1] <- list("a" = 5)

  # Act
  result <- get_transition_val(
    time = 1,
    transition_row_with_parameters = transition_row_with_parameters,
    population = matrix(),
    developing_population = matrix(),
    max_delay = 365L,
    predictors = data.frame()
  )

  # Assert
  expect_equal(result, 5)
})

test_that("`get_transition_val()` works with no predictors and delay", {
  # Arrange
  transition_row_with_parameters <- data.frame(
    from = "a",
    to = "b",
    transition_fun = "constant_fun",
    delay = TRUE,
    pred1 = NA,
    pred2 = NA
  )
  transition_row_with_parameters$params_list[1] <- list("a" = 5)

  # Act
  result <- get_transition_val(
    time = 1,
    transition_row_with_parameters = transition_row_with_parameters,
    population = matrix(),
    developing_population = matrix(),
    max_delay = 365L,
    predictors = data.frame()
  )

  # Assert
  expect_equal(result, 5)
})

test_that("`get_transition_val()` works with a predictor that varies over time
  and delay", {
  # Arrange

  transition_row_with_parameters <- data.frame(
    from = "a",
    to = "b",
    transition_fun = "expo_fun",
    delay = TRUE,
    pred1 = "temp",
    pred2 = NA
  )
  transition_row_with_parameters$params_list[1] <- list(c("a" = 2, "b" = 1))

  # TODO could be a fixture
  predictors <- data.frame(
    value = 11:20,
    j_day = 1:10,
    pred = "temp"
  )

  # Act
  result <- get_transition_val(
    time = 1,
    transition_row_with_parameters = transition_row_with_parameters,
    population = matrix(),
    developing_population = matrix(),
    max_delay = 365L,
    predictors = predictors
  )

  # Assert
  expect_equal(result, seq(22, 40, by = 2))
})

test_that("`get_transition_val()` works with a predictor that varies over time
 and no delay", {
  # Arrange

  transition_row_with_parameters <- data.frame(
    from = "a",
    to = "b",
    transition_fun = "expo_fun",
    delay = FALSE,
    pred1 = "temp",
    pred2 = NA
  )
  transition_row_with_parameters$params_list[1] <- list(c("a" = 2, "b" = 1))

  # TODO could be a fixture
  predictors <- data.frame(
    value = 11:20,
    j_day = 1:10,
    pred = "temp"
  )

  # Act
  result <- get_transition_val(
    time = 1,
    transition_row_with_parameters = transition_row_with_parameters,
    population = matrix(),
    developing_population = matrix(),
    max_delay = 365L,
    predictors = predictors
  )

  # Assert
  expect_equal(result, 22)
})

##################################### -----------------------------
test_that("`get_transition_value()` works with no predictors and probability-based transition", {
  # TODO duplicate test until core_functions.R is fully updated to reflect new config structure
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
    predictors = c(x = "temp")
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
    predictors = c(x = "temp")
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
  expect_snapshot(run(config_ex_1))
})

test_that("model output for `config_ex_2` stays the same", {
  # skipped on CRAN because it is long-running
  # testthat::skip("long running")
  testthat::skip_on_cran()
  expect_snapshot(run(config_ex_2))
})

# NOTE tests on new core_functions.R methods ----------------------------------
test_that("empty_delay_array snapshot", {
  expect_snapshot(empty_delay_array(c("a", "b"), 1, 1))
})

test_that("empty_population_matrix snapshot", {
  expect_snapshot(empty_population_matrix(c("a", "b"), 3))
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
  expect_snapshot(run(cfg))
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
