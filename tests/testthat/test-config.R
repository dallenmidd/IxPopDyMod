# output tests ----------------------------------------------------------------
test_that("output test with NULL predictors", {

  # Need to set the environment so it doesn't change in snapshots
  f1 <- function() 0.1
  f2 <- function() 10
  environment(f1) <- emptyenv()
  environment(f2) <- emptyenv()

  expect_snapshot(
    new_config(
      cycle = life_cycle(
        transition("a", "b", f1, "probability"),
        transition("b", "a", f2, "probability")
      ),
      preds = NULL,
      initial_population = c(a = 1L, b = 0L),
      steps = 10L,
      max_duration = 365L
    )
  )
})

test_that("output test with non-NULL predictors", {

  # Need to set the environment so it doesn't change in snapshots
  f1 <- function() 0.1
  f2 <- function() 10
  environment(f1) <- emptyenv()
  environment(f2) <- emptyenv()

  expect_snapshot(
    new_config(
      cycle = life_cycle(
        transition("a", "b", f1, "probability"),
        transition("b", "a", f2, "probability")
      ),
      preds = predictors(data.frame(
        pred = "temp",
        pred_subcategory = NA,
        j_day = NA,
        value = 1
      )),
      initial_population = c(a = 1L, b = 0L),
      steps = 10L,
      max_duration = 365L
    )
  )
})

# test simple checks on vector inputs -----------------------------------------

# tests on `steps`
test_that("integerish steps value is coerced to integer", {
  cfg <- config_example_a()
  cfg$steps <- as.double(10)
  cfg <- do.call(config, cfg)
  expect_identical(cfg$steps, 10L)
})
test_that("catches decimal steps value", {
  cfg <- config_example_a()
  cfg$steps <- 10.5
  expect_error(do.call(config, cfg), "Must be of type 'integer'")
})
test_that("catches negative steps value", {
  cfg <- config_example_a()
  cfg$steps <- -1
  expect_error(do.call(config, cfg), "Element 1 is not >= 0")
})
test_that("catches missing steps value", {
  cfg <- config_example_a()
  cfg$steps <- NA
  expect_error(do.call(config, cfg), "Contains missing values")
})
test_that("catches steps of length > 1", {
  cfg <- config_example_a()
  cfg$steps <- c(10L, 10L)
  expect_error(do.call(config, cfg), "Must have length 1")
})

# tests on `max_duration`
test_that("integerish max_duration value is coerced to integer", {
  cfg <- config_example_a()
  cfg$max_duration <- as.double(10)
  cfg <- do.call(config, cfg)
  expect_identical(cfg$max_duration, 10L)
})
test_that("catches decimal max_duration value", {
  cfg <- config_example_a()
  cfg$max_duration <- 10.5
  expect_error(do.call(config, cfg), "Must be of type 'integer'")
})
test_that("catches max_duration value < 1", {
  cfg <- config_example_a()
  cfg$max_duration <- 0
  expect_error(do.call(config, cfg), "not >= 1")
})
test_that("catches missing max_duration value", {
  cfg <- config_example_a()
  cfg$max_duration <- NA
  expect_error(do.call(config, cfg), "Contains missing values")
})
test_that("catches max_duration of length > 1", {
  cfg <- config_example_a()
  cfg$max_duration <- c(10L, 10L)
  expect_error(do.call(config, cfg), "Must have length 1")
})

# tests on `initial_population`
test_that("works with integerish initial_population values", {
  cfg <- config_example_a()
  cfg$initial_population <- stats::setNames(as.double(c(1.0, 0.0)), c("a", "b"))
  cfg <- do.call(config, cfg)
  expect_identical(cfg$initial_population, c(a = 1L, b = 0L))
})
test_that("catches decimal initial_population values", {
  cfg <- config_example_a()
  cfg$initial_population <- c(a = 1.5, 0)
  expect_error(do.call(config, cfg), "Must be of type 'integer'")
})
test_that("catches negative initial_population values", {
  cfg <- config_example_a()
  cfg$initial_population <- c(a = 2, b = -1)
  expect_error(do.call(config, cfg), "Element 2 is not >= 0")
})
test_that("catches initial_population of length zero", {
  cfg <- config_example_a()
  cfg$initial_population <- as.integer()
  expect_error(do.call(config, cfg), "Must have length >= 1")
})
test_that("catches NA initial_population values", {
  cfg <- config_example_a()
  cfg$initial_population <- c(a = 1L, b = NA)
  expect_error(do.call(config, cfg), "Contains missing values")
})
test_that("catches duplicate life stage names in initial_population", {
  cfg <- config_example_a()
  cfg$initial_population <- c(a = 1, a = 2, b = 0)
  expect_error(do.call(config, cfg), "Must have unique names")
})

# test more complicated checks / checks depending on multiple inputs ----------

test_that("catches initial_population with no values > 0", {
  cfg <- config_example_a()
  cfg$initial_population <- c(a = 0, b = 0)
  expect_error(
    do.call(config, cfg),
    "must be greater than 0 for at least one life stage"
  )
})

test_that("catches initial_population names that are not valid life stages", {
  cfg <- config_example_a()
  cfg$initial_population <- c(a = 1, b = 0, c = 2)
  expect_error(
    do.call(config, cfg),
    "had names that are not valid life stages"
  )
})

test_that(
  "catches predictor data that does not extend to steps + max_duration", {
    cfg <- config_example_a()
    cfg$preds <- predictors(data.frame(
      pred = "temp",
      pred_subcategory = NA,
      j_day = 1:3,
      value = 1
    ))
    cfg$steps <- 2
    cfg$max_duration <- 2
    expect_error(do.call(config, cfg), "data should extend to at least 4")
  }
)

test_that(
  paste(
    "catches transitions with predictor values that don't correspond to a",
    "a pred in the predictors table or a tick life stage"
  ), {
    # start with a valid config
    cfg <- config_example_a()

    # replace a predictor with an invalid value
    cfg$cycle[[1]]$predictors[["y"]]$pred <- "not_in_predictors_table"

    expect_error(do.call(config, cfg), "invalid predictor names")
})

test_that("named predictors and no named parameters are allowed", {
  cfg <- config_example_a()
  named_param <- parameters(host_preference = c("mouse" = 1, "deer" = 2))
  cfg$cycle[[1]]$parameters <- c(cfg$cycle[[1]]$parameters, named_param)
  cfg$cycle[[1]]$fun <- function(x, y, a, host_preference) a
  # Note that no predictor for this transition has a `pred_subcategory`
  expect_error(do.call(config, cfg), regexp = NA)
})

test_that("named predictors and parameters with same names are allowed", {
  cfg <- config_example_a()
  named_param <- parameters(host_preference = c("mouse" = 1, "deer" = 2))
  cfg$cycle[[1]]$parameters <- c(cfg$cycle[[1]]$parameters, named_param)
  cfg$cycle[[1]]$fun <- function(x, y, a, host_preference) a

  cfg$preds <- data.frame(
    pred = c("temp", "host_density", "host_density"),
    pred_subcategory = c(NA, "mouse", "deer"),
    j_day = NA,
    value = 1:3
  )

  expect_error(do.call(config, cfg), regexp = NA)
})

test_that("works with multiple parameters with same names", {
  cfg <- config_example_a()
  # Note that here we're creating *two* parameters with names
  named_param <- parameters(
    host_preference = c("mouse" = 1, "deer" = 2),
    feeding_success = c("mouse" = 3, "deer" = 4)
  )
  cfg$cycle[[1]]$parameters <- c(cfg$cycle[[1]]$parameters, named_param)
  cfg$cycle[[1]]$fun <- function(x, y, a, host_preference, feeding_success) a

  cfg$preds <- data.frame(
    pred = c("temp", "host_density", "host_density"),
    pred_subcategory = c(NA, "mouse", "deer"),
    j_day = NA,
    value = 1:3
  )

  expect_error(do.call(config, cfg), regexp = NA)
})

test_that("catches predictors and parameters with different names", {
  cfg <- config_example_a()
  named_param <- parameters(host_preference = c("deer" = 1, "rodent" = 2))
  cfg$cycle[[1]]$parameters <- c(cfg$cycle[[1]]$parameters, named_param)
  cfg$cycle[[1]]$fun <- function(x, y, a, host_preference) a

  cfg$preds <- data.frame(
    pred = c("temp", "host_density", "host_density"),
    # Note that we have `mouse` instead of `rodent` here
    pred_subcategory = c(NA, "deer", "mouse"),
    j_day = NA,
    value = 1:3
  )

  expect_error(
    validate_config(cfg),
    regexp = "named parameters and predictors must have identical names"
  )
})

test_that("catches predictors that use tick density and `first_day_only = FALSE`", {
  cfg <- config_example_a()
  cfg$cycle[[2]] <- transition(
    from = "b",
    to = "a",
    transition_type = "duration",
    fun = function(x) 1,
    predictors = list(
      x = predictor_spec(pred = "a", first_day_only = FALSE)
    )

  )

  cfg$preds <- data.frame(
    pred = c("temp", "host_density"),
    pred_subcategory = NA,
    j_day = NA,
    value = 1:2
  )

  expect_error(
    validate_config(cfg),
    regexp = "must have the `first_day_only` field set to `TRUE`"
  )
})

test_that("catches transition function that doesn't evaluate to a numeric", {
  # TODO unsure if this is really feasible or should just be a runtime check
  testthat::skip("will fail until more validation is implemented")
  cfg <- config_example_a()
  # modify a transition function to take the same args but return a character
  cfg$cycle[[1]]$fun <- function(x, y, a) "5"
  expect_error(do.call(config, cfg))
})

test_that("catches transition function that evaluates to non-finite number", {
  # TODO unsure if this is really feasible or should just be a runtime check
  testthat::skip("will fail until more validation is implemented")
  cfg <- config_example_a()
  # modify a transition function to take the same args but return `Inf`
  cfg$cycle[[1]]$fun <- function(x, y, a) Inf
  expect_error(do.call(config, cfg))
})

test_that("initial_population is set to zero for any unspecified life stages", {
  # TODO may not want to implement this because it involves coercion using
  # both initial_population and life_cycle... tricky to do this on un-validated
  # inputs, and breaks the recommended S3 pattern to do this at the end of
  # helper method. Instead, could do this at model run time.
})
