test_that("output test", {
  expect_snapshot(
    predictors_spec(
      a = new_predictors_spec_node(name = "temp", first_day_only = FALSE),
      b = new_predictors_spec_node(name = "host_den", first_day_only = TRUE)
    )
  )
})

test_that("can coerce lists in predictors_spec to predictors_spec_node", {
  # NOTE this is just a convenience feature - support could be removed and it
  # would simplify the code. It could be useful though if we want to convert
  # to/from YAML, as the class attribute may be dropped in process.
  result <- predictors_spec(
    a = list(name = "temp", first_day_only = FALSE),
    b = list(name = "host_den", first_day_only = TRUE)
  )
  expected <- predictors_spec(
    a = predictors_spec_node(name = "temp", first_day_only = FALSE),
    b = predictors_spec_node(name = "host_den", first_day_only = TRUE)
  )
  expect_identical(expected, result)
})
