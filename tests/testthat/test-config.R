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

