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

test_that("`get_life_stages()` works with `config_ex_1` data", {
  expected_life_stages <- c("__e", "__l", "__n", "__a")
  expect_equal(get_life_stages(config_ex_1$transitions), expected_life_stages)
})

test_that("`get_life_stages()` works with `config_ex_2` data", {
  expected_life_stages <- c("__e", "__l", "__n", "__a")
  expect_equal(get_life_stages(config_ex_2$transitions), expected_life_stages)
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
  # TODO too much work to setup test... evidence of refactor need, specifically
  # extracting pop matrix setup into function

  # Arrange
  steps <- 3
  life_stages <- c("a", "b", "c")
  num_life_stages <- length(life_stages)
  population <- matrix(1:9, nrow = num_life_stages, ncol = steps)
  rownames(population) <- life_stages

  # Act
  result <- get_tick_den(
    time = 2,
    N = population,
    N_developing = population,
    pred = "a|b",
    life_stages = life_stages
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
    time = 1L, pred = "host_den", is_delay = TRUE, N = matrix(),
    N_developing = matrix(), max_delay = 365L, life_stages = c("a", "b", "c"),
    predictors = predictors
  )

  result_no_delay <- get_pred(
    time = 1L, pred = "host_den", is_delay = FALSE, N = matrix(),
    N_developing = matrix(), max_delay = 365L, life_stages = c("a", "b", "c"),
    predictors = predictors
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
    time = 1L, pred = "[ab]", is_delay = TRUE, N = population,
    N_developing = population, max_delay = 365L, life_stages = c("a", "b", "c"),
    predictors = data.frame()
  )

  result_no_delay <- get_pred(
    time = 1L, pred = "[ab]", is_delay = FALSE, N = population,
    N_developing = population, max_delay = 365L, life_stages = c("a", "b", "c"),
    predictors = data.frame()
  )

  # Assert
  expect_equal(result_delay, expected)
  expect_equal(result_no_delay, expected)
})

test_that("`get_pred()` works with no predictor with or without delay", {
  result_delay <- get_pred(
    time = 1, pred = NA, is_delay = TRUE, N = matrix(),
    N_developing = matrix(), max_delay = 365L, life_stages = c("a", "b"),
    predictors = data.frame()
  )

  result_no_delay <- get_pred(
    time = 1, pred = NA, is_delay = FALSE, N = matrix(),
    N_developing = matrix(), max_delay = 365L, life_stages = c("a", "b"),
    predictors = data.frame()
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
    N = matrix(),
    N_developing = matrix(),
    max_delay = 365L,
    life_stages = c(),
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
    N = matrix(),
    N_developing = matrix(),
    max_delay = 365L,
    life_stages = c(),
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
    N = matrix(),
    N_developing = matrix(),
    max_delay = 365L,
    life_stages = c("a", "b", "c"),
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
    N = matrix(),
    N_developing = matrix(),
    max_delay = 365L,
    life_stages = c("a", "b", "c"),
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
    N = matrix(),
    N_developing = matrix(),
    max_delay = 365L,
    life_stages = c("a", "b", "c"),
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
    N = matrix(),
    N_developing = matrix(),
    max_delay = 365L,
    life_stages = c("a", "b", "c"),
    predictors = predictors
  )

  # Assert
  expect_equal(result, 22)

})

test_that("`gen_trans_matrix() works with `config_ex_1`", {
  life_stages <- get_life_stages(config_ex_1$transitions)
  expected <- matrix(
    0,
    4,
    4,
    dimnames = list(life_stages, life_stages)
  )
  expected["__a", "__e"] <- 1000
  expected["__e", "__l"] <- 1
  expected["__l", "__n"] <- 0.01
  expected["__n", "__a"] <- 0.1

  result <- gen_trans_matrix(
    1,
    NULL,
    NULL,
    life_stages,
    add_params_list(config_ex_1$transitions, config_ex_1$parameters),
    NULL
  )

  expect_equal(result, expected)
})

test_that("`gen_trans_matrix()` works with `config_ex_2`", {
  life_stages <- get_life_stages(config_ex_2$transitions)
  expected <- matrix(
    0,
    4,
    4,
    dimnames = list(life_stages, life_stages)
  )
  expected["__a", "__e"] <- 489.3045

  result <- gen_trans_matrix(
    1,
    NULL,
    NULL,
    life_stages,
    add_params_list(config_ex_2$transitions, config_ex_2$parameters),
    NULL
  )

  expect_equal(result, expected)
})

test_that("`gen_trans_matrix()` works with `ogden2005`", {
  life_stages <- get_life_stages(ogden2005$transitions)

  population <- empty_population_matrix(life_stages, 200)
  population[] <- 1

  transitions_with_parameters <- add_params_list(
    ogden2005$transitions, ogden2005$parameters
  )

  expect_snapshot(gen_trans_matrix(
    # Using a time in the middle of the year when temperature is higher and more
    # of the transitions will have nonzero values.
    time = 150,
    N = population,
    N_developing = population,
    life_stages = life_stages,
    tick_transitions = transitions_with_parameters,
    predictors = ogden2005$predictors
  ))
})


test_that("model output for `config_ex_1` stays the same", {
  expect_snapshot(run(config_ex_1))
})

test_that("model output for `config_ex_2` stays the same", {
  # skipped on CRAN because it is long-running
  testthat::skip_on_cran()
  expect_snapshot(run(config_ex_2))
})
