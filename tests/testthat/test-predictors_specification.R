test_that("output test", {
  expect_snapshot(
    list(
      a = new_predictor_spec(pred = "temp", first_day_only = FALSE),
      b = new_predictor_spec(pred = "host_den", first_day_only = TRUE)
    )
  )
})
