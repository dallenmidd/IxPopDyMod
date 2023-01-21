test_that("output test with NULL predictors", {

  # Need to set the environment so it doesn't change in snapshots
  f1 <- function() 0.1
  f2 <- function() 10
  environment(f1) <- emptyenv()
  environment(f2) <- emptyenv()

  expect_snapshot(
    new_config(
      cycle = life_cycle(
        transition("a", "b", f1, "probability"),
        transition("b", "a", f2, "probability")
      ),
      preds = NULL,
      initial_population = c(a = 1L, b = 0L),
      steps = 10L,
      max_delay = 365L
    )
  )
})

test_that("output test with non-NULL predictors", {

  # Need to set the environment so it doesn't change in snapshots
  f1 <- function() 0.1
  f2 <- function() 10
  environment(f1) <- emptyenv()
  environment(f2) <- emptyenv()

  expect_snapshot(
    new_config(
      cycle = life_cycle(
        transition("a", "b", f1, "probability"),
        transition("b", "a", f2, "probability")
      ),
      preds = predictors(data.frame(
        pred = "temp",
        pred_subcategory = NA,
        j_day = NA,
        value = 1
      )),
      initial_population = c(a = 1L, b = 0L),
      steps = 10L,
      max_delay = 365L
    )
  )
})

test_that("catches decimal steps value", {})
test_that("catches negative steps value", {})
test_that("catches missing steps value", {})
test_that("catches steps of length > 1", {})

test_that("catches decimal max_delay value", {})
test_that("catches max_delay value < 1", {})
test_that("catches missing max_delay value", {})
test_that("catches max_delay of length > 1", {})

test_that("catches decimal initial_population values", {})
test_that("catches negative initial_population values", {})
test_that("catches initial_population of length zero", {})
test_that("catches duplicate life stage names in initial_population", {})

test_that("initial_population set to zero for any unspecified life stages", {})
