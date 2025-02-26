test_that("frgd_ calculates forage demand correctly - imperial", {
  result <- frgd_(head = 136, wt = 1350, days = 20, area = 931, prct_int = 2.5, input_system = "imperial")
  expect_equal(round(as.numeric(result)), 99)  # Expected result in lb/ac
})

test_that("frgd_ calculates forage demand correctly - metric", {
  result <- frgd_(head = 136, wt = 1350, days = 20, area = units::set_units(931, "ha"), prct_int = 2.5, input_system = "metric")
  expect_equal(round(as.numeric(result)), 99)  # Expected result in lb/ac
})

test_that("frgd_ calculates forage demand correctly - metric", {
  frgd_(head = 136, wt = 1350, days = 20, area = units::set_units(931, "ha"), prct_int = 2.5, input_system = "metric", output_unit = "AUM/ac") %>%
  expect_no_error()
})

