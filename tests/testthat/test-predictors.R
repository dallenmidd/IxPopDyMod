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

test_that("works with NA jday", {
  expect_error(
    predictors(data.frame(
      pred = "host_den",
      pred_subcategory = NA,
      j_day = NA,
      value = 1
    )),
    regexp = NA
  )
})

test_that("catches a mix of NA and non-NA j_day for a single predictor", {
  expect_error(
    predictors(data.frame(
      pred = "host_den",
      pred_subcategory = NA,
      j_day = c(NA, 1),
      value = c(1, 2)
    )),
    regexp = paste0(
      "The `j_day` column for each `pred` must be entirely NA (indicating a ",
      "constant value) or entirely non-NA "
    ),
    fixed = TRUE
  )
})

test_that("catches missing j_days for a single predictor", {
  expect_error(
    predictors(data.frame(
      pred = "host_den",
      pred_subcategory = NA,
      j_day = c(1, 3),
      value = c(1, 2)
    )),
    regexp = "is missing day(s): 2",
    fixed = TRUE
  )
})

test_that("catches missing first day", {
  expect_error(
    predictors(data.frame(
      pred = "host_den",
      pred_subcategory = NA,
      j_day = 2,
      value = 1
    )),
    regexp = "is missing day(s): 1",
    fixed = TRUE
  )
})

test_that("catches when a predictor spans a shorter time range than another", {
  expect_error(
    predictors(data.frame(
      pred = "host_den",
      pred_subcategory = c("mouse", "deer", "deer"),
      j_day = c(1, 1, 2),
      value = 1
    )),
    regexp = "is missing day(s): 2",
    fixed = TRUE
  )
})




