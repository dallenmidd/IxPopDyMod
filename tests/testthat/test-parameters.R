test_that("new_parameters() works", {
  result <- new_parameters(a = 1, b = 2)
  expected <- list(a = 1, b = 2)
  class(expected) <- "parameters"
  expect_equal(result, expected)
})

test_that("new_parameters() throws error when parameters are missing names", {
  expect_error(new_parameters(1, 2), regexp = "Must have names")
  expect_error(new_parameters(1, b = 2), "element 1 is empty")
  expect_error(new_parameters(a = 1, b = c(d = 2, 2)), "element 2 is empty")
})

test_that("new_parameters() allows zero parameters", {
  expect_equal(new_parameters(), structure(list(), class = "parameters"))
})

test_that("new_parameters() throws error if names are duplicated", {
  expect_error(new_parameters(a = 1, a = 2), "element 2 is duplicated")
})

test_that("parameters() output test", {
  result <- parameters(a = 1, b = 2)
  expected <- list(a = 1, b = 2)
  class(expected) <- "parameters"
  expect_equal(result, expected)
})

test_that("parameters() sorts named vector parameters by names", {
  result <- parameters(b = 1, a = c("y" = 1, "x" = 2))
  # Note that parameter order is not changed, just order *within* vector parameters
  expected <- list(b = 1, a = c("x" = 2, "y" = 1))
  class(expected) <- "parameters"
  expect_equal(result, expected)
})
