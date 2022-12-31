test_that("new_parameters() works", {
  result <- new_parameters(a = 1, b = 2)
  expected <- list(a = 1, b = 2)
  class(expected) <- "parameters"
  expect_equal(result, expected)
})

test_that("new_parameters() throws eror when parameters are missing names", {
  expect_error(new_parameters(1, 2))
  expect_error(new_parameters(1, b = 2))
  expect_error(new_parameters(a = 1, b = c(d = 2, 2)))
})

test_that("new_parameters() throws error if names are duplicated", {
  expect_error(new_parameters(a = 1, a = 2))
})
