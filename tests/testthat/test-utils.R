test_that("all_elements_are_named() works with vectors", {
  expect_equal(all_elements_are_named(c(a = 1, b = 2)), TRUE)
  expect_equal(all_elements_are_named(c(1, b = 2)), FALSE)
  expect_equal(all_elements_are_named(1:2), FALSE)
})

test_that("all_elements_are_named() works with lists", {
  expect_equal(all_elements_are_named(list(a = 1, b = 2)), TRUE)
  expect_equal(all_elements_are_named(list(1, b = 2)), FALSE)
  expect_equal(all_elements_are_named(list(1:2)), FALSE)
})

test_that("has_duplicate_names() works", {
  expect_equal(has_duplicate_names(c(a = 1, a = 2)), TRUE)
  expect_equal(has_duplicate_names(c(a = 1, b = 2)), FALSE)
})
