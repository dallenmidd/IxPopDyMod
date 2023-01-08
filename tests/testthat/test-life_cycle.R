# new_life_cycle() ------------------------------------------------------------
test_that("output test", {
  expect_snapshot(
     new_life_cycle(
       transition_example_a(),
       transition_example_b()
     )
  )
})

test_that("throws error with non-transition input", {
  expect_error(
    new_life_cycle(
      transition_example_a(),
      list()
    ),
    regexp = "May only contain the following types: {transition}",
    fixed = TRUE
  )
})

test_that("throws error with duplicate transitions", {
  expect_error(
    new_life_cycle(
      transition_example_a(),
      transition_example_a()
    ),
    "Contains duplicated values, position 2"
  )
})

# life_cycle() ----------------------------------------------------------------
test_that("handles transitions missing class attribute", {
  expected <- life_cycle(
    transition_example_a(),
    transition_example_b()
  )

  # transitions should be coerced to `transition` type
  result <- life_cycle(
    unclass(transition_example_a()),
    transition_example_b()
  )

  expect_identical(expected, result)
})

test_that("throws error with an invalid transition input", {
  transition_missing_element <- transition_example_b()
  transition_missing_element$parameters <- NULL
  expect_error(
    life_cycle(
      transition_example_a(),
      transition_missing_element
    ),
    regexp = "but is missing elements {'parameters'}",
    fixed = TRUE
  )
})

# validate_life_cycle() -------------------------------------------------------
# TODO


# query_transitions() ---------------------------------------------------------
test_that("works with transition_type field", {

  duration_transition <- transition_example_b()
  duration_transition$transition_type <- "duration"

  input <- life_cycle(
    transition_example_a(),
    duration_transition
  )

  # Note that `query_transitions()` strips class attribute
  expected <- unclass(life_cycle(
    duration_transition
  ))

  result <- query_transitions(input, "transition_type", "duration")

  expect_identical(result, expected)


})

test_that("works with from field", {
  input <- life_cycle(
    transition_example_a(),
    transition_example_b()
  )

  expected <- unclass(life_cycle(
    transition_example_a()
  ))

  result <- query_transitions(input, "from", "a")

  expect_identical(result, expected)
})
