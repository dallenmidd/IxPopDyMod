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

test_that("probability transitions can only have per day mortality", {
  expect_error(
    new_transition(
      from = "a",
      to = NA,
      transition_type = new_transition_type("probability"),
      mortality_type = new_mortality_type("throughout_transition"),
      fun = constant_fun,
      predictors = "",
      parameters = new_parameters()
    )
  )
})

# transition_is_mortality -----------------------------------------------------
test_that("correctly identifies mortality", {
  transition <- new_transition(
    from = "a",
    to = NA,
    transition_type = new_transition_type("probability"),
    mortality_type = new_mortality_type("per_day"),
    fun = constant_fun,
    predictors = "",
    parameters = new_parameters()
  )
  expect_true(transition_is_mortality(transition))
})

test_that("correctly identifies no mortality", {
  transition <- new_transition(
    from = "a",
    to = "b",
    transition_type = new_transition_type("probability"),
    mortality_type = new_mortality_type(NA),
    fun = constant_fun,
    predictors = "",
    parameters = new_parameters()
  )
  expect_false(transition_is_mortality(transition))
})


# new_transition_type ---------------------------------------------------------
test_that("transition type cannot be NA", {
  expect_error(new_transition_type(NA))
})

test_that("works with allowed input", {
  expect_snapshot(new_transition_type("probability"))
})

# new_mortality_type ----------------------------------------------------------
test_that("mortality type can be NA", {
  expect_error(new_mortality_type(NA), regexp = NA)
})

test_that("works with allowed input", {
  expect_snapshot(new_mortality_type("per_day"))
})
