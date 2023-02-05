test_that("expo fun returns expected values", {
  expect_equal(expo_fun(0, 2, 3), 0)
  expect_equal(expo_fun(2, 1.5, 2), 6)
})
