# new_life_cycle() ------------------------------------------------------------
test_that("output test", {
  expect_snapshot(
     new_life_cycle(list(
       transition_example_a(),
       transition_example_b()
     ))
  )
})
