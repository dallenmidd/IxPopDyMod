# new_transition_function -----------------------------------------------------
test_that("requires function input", {
  expect_error(
    new_transition_function(c("a character vector")),
    "Must be a function, not 'character'"
  )
  expect_error(
    new_transition_function(list(constant_fun, expo_fun)),
    "Must be a function, not 'list'"
  )
})

test_that("does not change function output", {
  expo_fun2 <- new_transition_function(expo_fun)
  expect_identical(expo_fun(1, 2, 3, 4), expo_fun2(1, 2, 3, 4))
})

# transition_function_is_custom -----------------------------------------------
test_that("returns true for custom function", {
  fun <- new_transition_function(function(x, y, a) {})
  expect_true(transition_function_is_custom(fun))
})

test_that("returns false for function in package", {
  expect_false(transition_function_is_custom(constant_fun))
})
