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
      predictors = c("x", "y"),
      parameters = new_parameters(a = 1)
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
      predictors = c("x", "y"),
      parameters = new_parameters(a = 1)
    )),
    "exactly 1 of `to` or `mortality_type` must be non-NULL"
  )

  expect_error(validate_transition(new_transition(
    from = "a",
    to = NULL,
    transition_type = "probability",
    mortality_type = "per_day",
    fun = new_transition_function(constant_fun),
    predictors = c("x", "y"),
    parameters = new_parameters(a = 1)
  )), regexp = NA)

  expect_error(validate_transition(new_transition(
    from = "a",
    to = "b",
    transition_type = "probability",
    mortality_type = NULL,
    fun = new_transition_function(constant_fun),
    predictors = c("x", "y"),
    parameters = new_parameters(a = 1)
  )), regexp = NA)
})

test_that("catches extra parameters not needed in transition function", {
  expect_error(
    validate_transition(new_transition(
      from = "a",
      to = NULL,
      transition_type = "probability",
      mortality_type = "per_day",
      fun = new_transition_function(constant_fun),
      predictors = c("x", "y"),
      parameters = new_parameters(a = 1, b = 2)
    )),
    regexp = "Must be a permutation of set {'a'}, but has extra elements {'b'}",
    fixed = TRUE
  )
})

test_that("catches missing parameters needed in transition function", {
  expect_error(
    validate_transition(new_transition(
      from = "a",
      to = NULL,
      transition_type = "probability",
      mortality_type = "per_day",
      fun = new_transition_function(constant_fun),
      predictors = c("x", "y"),
      parameters = new_parameters()
    )),
    regexp = "Must be setequal to {'a'}, but has different type.",
    fixed = TRUE
  )
})

test_that("allows transition with zero parameters", {
  expect_error(
    validate_transition(new_transition(
      from = "a",
      to = NULL,
      transition_type = "probability",
      mortality_type = "per_day",
      fun = new_transition_function(function(x, y) {}),
      predictors = c("x", "y"),
      parameters = new_parameters()
    )),
    regexp = NA
  )
})

test_that("catches extra predictors not needed in transition function", {
  expect_error(
    validate_transition(new_transition(
      from = "a",
      to = NULL,
      transition_type = "probability",
      mortality_type = "per_day",
      fun = new_transition_function(constant_fun),
      predictors = c("x", "y", "z"),
      parameters = new_parameters(a = 1)
    )),
    regexp = (
      "Must be a permutation of set {'x','y'}, but has extra elements {'z'}"
    ),
    fixed = TRUE
  )
})

test_that("catches missing predictors needed in transition function", {
  expect_error(
    validate_transition(new_transition(
      from = "a",
      to = NULL,
      transition_type = "probability",
      mortality_type = "per_day",
      fun = new_transition_function(constant_fun),
      predictors = c("x"),
      parameters = new_parameters(a = 1)
    )),
    regexp = "Must be a set equal to {'x','y'}, but is missing elements {'y'}.",
    fixed = TRUE
  )
})

# transition() ----------------------------------------------------------------
test_that("works with defaults", {

  # Need to set the environment so it doesn't change in snapshots
  f <- function(x, y) {}
  environment(f) <- emptyenv()

  expect_snapshot(transition(
    from = "a",
    to = "b",
    fun = f,
    transition_type = "probability"
  ))
})

test_that("can handle vector parameters input", {
  result <- transition(
    from = "a",
    to = "b",
    fun = new_transition_function(constant_fun),
    transition_type = "probability",
    parameters = c(a = 1)
  )

  expect_s3_class(result$parameters, "parameters")
})

test_that("can coerce input fun to transition_function", {
  result <- transition(
    from = "a",
    to = "b",
    fun = constant_fun,
    transition_type = "probability",
    parameters = new_parameters(a = 1)
  )

  expect_s3_class(result$fun, "transition_function")
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
