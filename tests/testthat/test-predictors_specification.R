test_that("output test", {
  expect_snapshot(
    predictors_spec(
      a = new_predictors_spec_node(
        name = "temp",
        first_day_only = FALSE
      ),
      b = new_predictors_spec_node(
        name = "host_den",
        first_day_only = TRUE
      )
    )
  )
})
