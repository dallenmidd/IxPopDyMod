# predictors() ----------------------------------------------------------------
test_that("catches missing columns", {
  expect_error(
    predictors(data.frame(j_day = 1)),
    "names must be a set equal to {'pred','pred_subcategory','j_day','value'}",
    fixed = TRUE
  )
})

test_that("catches extra columns", {
  expect_error(
    predictors(data.frame(
      pred = "host_den",
      pred_subcategory = "mouse",
      j_day = 1,
      value = 1,
      extra_col = "extra"
    )),
    "has extra elements {'extra_col'}",
    fixed = TRUE
  )
})

test_that("catches missing values in `pred` column", {
  expect_error(
    predictors(data.frame(
      pred = NA,
      pred_subcategory = "mouse",
      j_day = 1,
      value = 1
    )),
    "Contains missing values (element 1)",
    fixed = TRUE
  )
})

test_that("catches empty string in `pred` column", {
  expect_error(
    predictors(data.frame(
      pred = "",
      pred_subcategory = "mouse",
      j_day = 1,
      value = 1
    )),
    "All elements must have at least 1 characters",
  )
})

test_that("catches empty string in `pred_subcategory` column", {
  expect_error(
    predictors(data.frame(
      pred = "host_den",
      pred_subcategory = "",
      j_day = 1,
      value = 1
    )),
    "All elements must have at least 1 characters",
  )
})

test_that("catches negative value in `j_day` column", {
  expect_error(
    predictors(data.frame(
      pred = "host_den",
      pred_subcategory = NA,
      j_day = -1,
      value = 1
    )),
    "is not >= 0"
  )
})

test_that("catches fractional value in `j_day` column", {
  expect_error(
    predictors(data.frame(
      pred = "host_den",
      pred_subcategory = NA,
      j_day = 1.5,
      value = 1
    )),
    "Must be of type 'integer', not 'double'.",
    fixed = TRUE
  )
})

test_that("coerces integer-like value in `j_day` column to integer", {
  preds <- predictors(data.frame(
    pred = "host_den",
    pred_subcategory = NA,
    j_day = 1.0,
    value = 1
  ))
  expect_type(preds$j_day, "integer")
  expect_identical(preds$j_day, 1L)
})

test_that("catches missing value in `value` column", {
  expect_error(
    predictors(data.frame(
      pred = "host_den",
      pred_subcategory = NA,
      j_day = 1,
      value = NA
    )),
    "Contains missing values",
    fixed = TRUE
  )
})
