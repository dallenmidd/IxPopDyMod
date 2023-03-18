test_that("output test", {
  expect_snapshot(
    new_predictors_specification(
      a = new_predictors_specification_node(
        name = "temp",
        first_day_only = FALSE
      ),
      b = new_predictors_specification_node(
        name = "host_den",
        first_day_only = TRUE
      )
    )
  )
})
