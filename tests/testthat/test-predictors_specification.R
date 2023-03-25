test_that("output test", {
  expect_snapshot(
    list(
      a = new_predictors_spec_node(pred = "temp", first_day_only = FALSE),
      b = new_predictors_spec_node(pred = "host_den", first_day_only = TRUE)
    )
  )
})
