test_that("expo fun returns expected values", {
  expect_equal(expo_fun(0, 1, 2, 3), 0)
  expect_equal(expo_fun(2, NULL, 1.5, 2), 6)
})
