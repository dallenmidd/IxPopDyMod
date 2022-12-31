# new_transition --------------------------------------------------------------
test_that("produces expected output with valid input", {
  expect_snapshot(new_transition(
    from = "a",
    to = "b",
    transition_type = new_transition_type("probability"),
    mortality_type = new_mortality_type(NA),
    fun = constant_fun,
    predictors = "",
    parameters = new_parameters()
  ))
})

test_that("throws error with invalid from input", {
  expect_error(new_transition(
    from = "",
    to = "b",
    transition_type = new_transition_type("probability"),
    mortality_type = new_mortality_type(NA),
    fun = function() {},
    predictors = "",
    parameters = new_parameters()
  ))

  expect_error(new_transition(
    from = 1,
    to = "b",
    transition_type = new_transition_type("probability"),
    mortality_type = new_mortality_type(NA),
    fun = function() {},
    predictors = "",
    parameters = new_parameters()
  ))
})

test_that("throws error with invalid to input", {
  expect_error(new_transition(
    from = "a",
    to = "",
    transition_type = new_transition_type("probability"),
    mortality_type = new_mortality_type(NA),
    fun = function() {},
    predictors = "",
    parameters = new_parameters()
  ))

  expect_error(new_transition(
    from = "a",
    to = NULL,
    transition_type = new_transition_type("probability"),
    mortality_type = new_mortality_type(NA),
    fun = function() {},
    predictors = "",
    parameters = new_parameters()
  ))
})

test_that("ensures that either mortality_type or to is NA", {
  expect_error(new_transition(
    from = "a",
    to = NA,
    transition_type = new_transition_type("probability"),
    mortality_type = new_mortality_type(NA),
    fun = function() {},
    predictors = "",
    parameters = new_parameters()
  ))

  expect_error(new_transition(
    from = "a",
    to = NA,
    transition_type = new_transition_type("probability"),
    mortality_type = new_mortality_type("per_day"),
    fun = function() {},
    predictors = "",
    parameters = new_parameters()
  ), regexp = NA)

  expect_error(new_transition(
    from = "a",
    to = "b",
    transition_type = new_transition_type("probability"),
    mortality_type = new_mortality_type(NA),
    fun = function() {},
    predictors = "",
    parameters = new_parameters()
  ), regexp = NA)

})

# transition_is_mortality -----------------------------------------------------


# new_transition_type ---------------------------------------------------------
test_that("transition type cannot be NA", {
  expect_error(new_transition_type(NA))
})

# new_mortality_type ----------------------------------------------------------
test_that("mortality type can be NA", {
  expect_error(new_mortality_type(NA), regexp = NA)
})
