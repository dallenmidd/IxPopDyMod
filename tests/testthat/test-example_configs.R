test_that("example configs are valid", {
  # Since there have been various changes to the config structure, adding this
  # simple test to ensure that the example configs that come with the package
  # remain valid.
  expect_error(validate_config(config_ex_1), NA)
  expect_error(validate_config(config_ex_2), NA)
  expect_error(validate_config(ogden2005), NA)
  expect_error(validate_config(temp_example_config), NA)
  expect_error(validate_config(host_example_config), NA)
  expect_error(validate_config(infect_example_config), NA)
  expect_error(validate_config(winter_tick), NA)
})
