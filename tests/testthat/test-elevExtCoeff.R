test_that("elevExtCoeff issues experimental warning", {
  expect_warning(elevExtCoeff(1000), "experimental")
})

test_that("elevExtCoeff returns a single numeric at sea level", {
  result <- suppressWarnings(elevExtCoeff(0))
  expect_type(result, "double")
  expect_length(result, 1)
})

test_that("elevExtCoeff sea-level value matches formula", {
  # 0.1451 * exp(0) + 0.136 = 0.2811
  expect_equal(suppressWarnings(elevExtCoeff(0)), 0.2811, tolerance = 1e-4)
})

test_that("elevExtCoeff is monotonically decreasing with elevation", {
  vals <- suppressWarnings(elevExtCoeff(c(0, 500, 1000, 2000)))
  expect_true(all(diff(vals) < 0))
})

test_that("elevExtCoeff is vectorised", {
  result <- suppressWarnings(elevExtCoeff(c(0, 500, 1000, 2000)))
  expect_length(result, 4)
})

test_that("error on negative elevation", {
  expect_error(elevExtCoeff(-1), "'elev' must be a non-negative numeric")
})

test_that("error on non-numeric elevation", {
  expect_error(elevExtCoeff("high"), "'elev' must be a non-negative numeric")
})
