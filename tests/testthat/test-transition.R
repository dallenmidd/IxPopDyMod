# new_transition --------------------------------------------------------------
test_that("produces expected output with valid input", {

  trans <- new_transition(
    from = "a",
    to = "b",
    transition_type = "probability",
    mortality_type = NULL,
    fun = new_transition_function(constant_fun),
    predictors = NULL,
    parameters = new_parameters(a = 1)
  )

  # Comparing functions is hard and was failing in R CMD check. So we do it
  # separately, ignoring the bytecode attr, and compare the rest with a snapshot.
  expect_identical(
    object = trans$fun,
    expected = structure(constant_fun, class = "transition_function"),
    ignore_attr = "bytecode"
  )
  expect_snapshot(trans[names(trans) != "fun"])
})

test_that("throws error with invalid from input", {
  expect_error(
    new_transition(
      from = "a",
      to = "",
      transition_type = "probability",
      mortality_type = NULL,
      fun = new_transition_function(constant_fun),
      predictors = NULL,
      parameters = new_parameters(a = 1)
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
      predictors = NULL,
      parameters = new_parameters(a = 1)
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
      predictors = NULL,
      parameters = new_parameters(a = 1)
    ),
    "All elements must have at least 1 characters",
  )
})

# validate_transition() -------------------------------------------------------
test_that("probability transitions can only have per day mortality", {
  expect_error(
    validate_transition(new_transition(
      from = "a",
      to = NULL,
      transition_type = "probability",
      mortality_type = "throughout_transition",
      fun = new_transition_function(constant_fun),
      predictors = NULL,
      parameters = new_parameters(a = 1)
    )),
    "`probability` transitions only support `per_day` mortality"
  )
})

test_that("ensures that either mortality_type or to is NULL", {
  expect_error(
    validate_transition(new_transition(
      from = "a",
      to = NULL,
      transition_type = "probability",
      mortality_type = NULL,
      fun = new_transition_function(constant_fun),
      predictors = NULL,
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
      predictors = NULL,
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
    predictors = NULL,
    parameters = new_parameters(a = 1)
  )), regexp = NA)

  expect_error(validate_transition(new_transition(
    from = "a",
    to = "b",
    transition_type = "probability",
    mortality_type = NULL,
    fun = new_transition_function(constant_fun),
    predictors = NULL,
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
      predictors = NULL,
      parameters = new_parameters(a = 1, b = 2)
    )),
    regexp = "has extra elements {'b'}",
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
      fun = new_transition_function(function(a, b) a),
      predictors = NULL,
      parameters = new_parameters(b = 1)
    )),
    regexp = "is missing elements {'a'}.",
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
      fun = new_transition_function(function(x, y) NULL),
      predictors = new_predictors_spec(x = "temp", y = "host_density"),
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
      predictors = c(z = "extra"),
      parameters = new_parameters(a = 1)
    )),
    regexp = (
      "has extra elements {'z'}"
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
      fun = new_transition_function(function(a, y) a),
      predictors = NULL,
      parameters = new_parameters(a = 1)
    )),
    regexp = "is missing elements {'y'}.",
    fixed = TRUE
  )
})

test_that("catches duplicate names between parameters and predictors", {
  expect_error(
    validate_transition(new_transition(
      from = "a",
      to = NULL,
      transition_type = "probability",
      mortality_type = "per_day",
      fun = new_transition_function(constant_fun),
      predictors = c(a = "duplicate name"),
      parameters = new_parameters(a = 1)
    )),
    regexp = "Must be disjunct from {'a'}, but has elements {'a'}.",
    fixed = TRUE
  )
})

# transition() ----------------------------------------------------------------
test_that("works with defaults", {

  # Need to set the environment so it doesn't change in snapshots
  f <- function() NULL
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
    fun = new_transition_function(function(a) NULL),
    transition_type = "probability",
    parameters = c(a = 1)
  )

  expect_s3_class(result$parameters, "parameters")
})

test_that("can coerce input fun to transition_function", {
  result <- transition(
    from = "a",
    to = "b",
    fun = function(a) NULL,
    transition_type = "probability",
    parameters = new_parameters(a = 1)
  )

  expect_s3_class(result$fun, "transition_function")
})

# transition_is_mortality -----------------------------------------------------
test_that("correctly identifies mortality", {
  transition <- transition(
    from = "a",
    to = NULL,
    transition_type = "probability",
    mortality_type = "per_day",
    fun = function() NULL,
  )
  expect_true(transition_is_mortality(transition))
})

test_that("correctly identifies no mortality", {
  transition <- transition(
    from = "a",
    to = "b",
    transition_type = "probability",
    mortality_type = NULL,
    fun = function() NULL,
  )
  expect_false(transition_is_mortality(transition))
})
