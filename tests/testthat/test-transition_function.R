# new_transition_function -----------------------------------------------------
test_that("requires function input", {
  expect_error(new_transition_function(c("a character vector")))
  expect_error(new_transition_function(c(constant_fun, expo_fun)))
})

test_that("requires >= 2 function arguments", {
  expect_error(new_transition_function(function(x) {}))
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

# get_predictor_names ---------------------------------------------------------
test_that("gets first two formals", {
  expect_identical(get_predictor_names(function(a, b) {}), c("a", "b"))
  expect_identical(get_predictor_names(function(a, b, c) {}), c("a", "b"))
})

# get_parameter_names ---------------------------------------------------------
test_that("gets formals starting at index 3", {
  expect_identical(get_parameter_names(function(a, b) {}), c())
  expect_identical(get_parameter_names(function(a, b, c) {}), c("c"))
})
