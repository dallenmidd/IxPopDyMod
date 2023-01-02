# new_transition --------------------------------------------------------------
test_that("produces expected output with valid input", {
  expect_snapshot(new_transition(
    from = "a",
    to = "b",
    transition_type = "probability",
    mortality_type = NULL,
    fun = new_transition_function(constant_fun),
    predictors = "",
    parameters = new_parameters()
  ))
})

test_that("throws error with invalid from input", {
  expect_error(
    new_transition(
      from = "",
      to = "b",
      transition_type = "probability",
      mortality_type = NULL,
      fun = new_transition_function(constant_fun),
      predictors = "",
      parameters = new_parameters()
    ),
    "All elements must have at least 1 characters"
  )

  expect_error(
    new_transition(
      from = NULL,
      to = "b",
      transition_type = "probability",
      mortality_type = NULL,
      fun = new_transition_function(constant_fun),
      predictors = "",
      parameters = new_parameters()
    ),
    "Must be of type 'string'"
  )
})

test_that("throws error with invalid to input", {
  expect_error(
    new_transition(
      from = "a",
      to = "",
      transition_type = "probability",
      mortality_type = NULL,
      fun = new_transition_function(constant_fun),
      predictors = "",
      parameters = new_parameters()
    ),
    "All elements must have at least 1 characters",
  )
})

test_that("probability transitions can only have per day mortality", {
  expect_error(
    new_transition(
      from = "a",
      to = NULL,
      transition_type = "probability",
      mortality_type = "throughout_transition",
      fun = constant_fun,
      predictors = "",
      parameters = new_parameters()
    ),
    "Must inherit from class 'transition_function'"
  )
})


# validate_transition() -------------------------------------------------------
test_that("ensures that either mortality_type or to is NULL", {
  expect_error(
    validate_transition(new_transition(
      from = "a",
      to = NULL,
      transition_type = "probability",
      mortality_type = NULL,
      fun = new_transition_function(constant_fun),
      predictors = "",
      parameters = new_parameters()
    )),
    "exactly 1 of `to` or `mortality_type` must be non-NULL"
  )

  expect_error(
    validate_transition(new_transition(
      from = "a",
      to = "b",
      transition_type = "probability",
      mortality_type = "per_day",
      fun = new_transition_function(constant_fun),
      predictors = "",
      parameters = new_parameters()
    )),
    "exactly 1 of `to` or `mortality_type` must be non-NULL"
  )

  expect_error(validate_transition(new_transition(
    from = "a",
    to = NULL,
    transition_type = "probability",
    mortality_type = "per_day",
    fun = new_transition_function(constant_fun),
    predictors = "",
    parameters = new_parameters()
  )), regexp = NA)

  expect_error(validate_transition(new_transition(
    from = "a",
    to = "b",
    transition_type = "probability",
    mortality_type = NULL,
    fun = new_transition_function(constant_fun),
    predictors = "",
    parameters = new_parameters()
  )), regexp = NA)
})


# transition_is_mortality -----------------------------------------------------
test_that("correctly identifies mortality", {
  transition <- new_transition(
    from = "a",
    to = NULL,
    transition_type = "probability",
    mortality_type = "per_day",
    fun = new_transition_function(constant_fun),
    predictors = "",
    parameters = new_parameters()
  )
  expect_true(transition_is_mortality(transition))
})

test_that("correctly identifies no mortality", {
  transition <- new_transition(
    from = "a",
    to = "b",
    transition_type = "probability",
    mortality_type = NULL,
    fun = new_transition_function(constant_fun),
    predictors = "",
    parameters = new_parameters()
  )
  expect_false(transition_is_mortality(transition))
})
