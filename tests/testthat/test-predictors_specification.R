test_that("output test", {
  expect_snapshot(
    new_predictors_specification(
      a = new_predictors_specification_node(
        name = "temp",
        first_day_only = FALSE
      )
    )
  )
})
