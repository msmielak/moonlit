# Test location: Armidale, NSW, Australia — a typical ecology study site
LAT  <- -30.5
LON  <- 151.5
TZ   <- "Australia/Sydney"
E    <- 0.21

# A night-time timestamp during a bright moon period (full moon ~27 Nov 2023)
DATE_NIGHT <- as.POSIXct("2023-11-27 22:00:00", tz = TZ)

# A sequence spanning a full night for physics-constraint tests
NIGHT_SEQ <- seq(
  from = as.POSIXct("2023-11-27 19:00:00", tz = TZ),
  to   = as.POSIXct("2023-11-28 07:00:00", tz = TZ),
  by   = "30 mins"
)

# ---------------------------------------------------------------------------
# Output structure
# ---------------------------------------------------------------------------

test_that("returns a data frame with the correct columns", {
  result <- calculateMoonlightIntensity(LAT, LON, DATE_NIGHT, E)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("date", "lat", "lon", "night", "sunAltDegrees",
                         "moonAltDegrees", "moonlightModel", "twilightModel",
                         "illumination", "moonPhase"))
})

test_that("returns one row per input timestamp", {
  result <- calculateMoonlightIntensity(LAT, LON, NIGHT_SEQ, E)
  expect_equal(nrow(result), length(NIGHT_SEQ))
})

# ---------------------------------------------------------------------------
# Physics constraints
# ---------------------------------------------------------------------------

test_that("moonlightModel is 0 whenever moon is at or below the horizon", {
  result <- calculateMoonlightIntensity(LAT, LON, NIGHT_SEQ, E)
  below  <- result$moonAltDegrees <= 0
  expect_true(all(result$moonlightModel[below] == 0))
})

test_that("moonlightModel is positive whenever moon is above the horizon", {
  result <- calculateMoonlightIntensity(LAT, LON, NIGHT_SEQ, E)
  above  <- result$moonAltDegrees > 0
  expect_true(all(result$moonlightModel[above] > 0))
})

test_that("moonlightModel contains no NA or Inf values", {
  result <- calculateMoonlightIntensity(LAT, LON, NIGHT_SEQ, E)
  expect_false(any(is.na(result$moonlightModel)))
  expect_false(any(is.infinite(result$moonlightModel)))
})

test_that("twilightModel is 0 when sun is below -18 degrees", {
  result  <- calculateMoonlightIntensity(LAT, LON, NIGHT_SEQ, E)
  deep    <- result$sunAltDegrees < -18
  expect_true(all(result$twilightModel[deep] == 0))
})

test_that("moonPhase is between 0 and 1", {
  result <- calculateMoonlightIntensity(LAT, LON, NIGHT_SEQ, E)
  expect_true(all(result$moonPhase >= 0 & result$moonPhase <= 1))
})

test_that("illumination is non-negative", {
  result <- calculateMoonlightIntensity(LAT, LON, NIGHT_SEQ, E)
  expect_true(all(result$illumination >= 0))
})

test_that("night flag matches sun below geometric horizon", {
  result <- calculateMoonlightIntensity(LAT, LON, NIGHT_SEQ, E)
  expect_equal(result$night, result$sunAltDegrees < 0)
})

test_that("moonlightModel near 1 during bright full moon at high altitude", {
  # Full moon on 27 Nov 2023; moon near zenith around local midnight
  # moonlightModel should be well above 0 and at most ~1.3 (perigee boost possible)
  result <- calculateMoonlightIntensity(LAT, LON, DATE_NIGHT, E)
  if (result$moonAltDegrees > 30 && result$moonPhase > 0.95) {
    expect_gt(result$moonlightModel, 0.5)
    expect_lt(result$moonlightModel, 1.5)
  }
})

# ---------------------------------------------------------------------------
# Input validation — errors
# ---------------------------------------------------------------------------

test_that("error on non-POSIXct date", {
  expect_error(
    calculateMoonlightIntensity(LAT, LON, "2023-11-27 22:00:00", E),
    "'date' must be a POSIXct object"
  )
})

test_that("error on Date (not POSIXct) date", {
  expect_error(
    calculateMoonlightIntensity(LAT, LON, as.Date("2023-11-27"), E),
    "'date' must be a POSIXct object"
  )
})

test_that("error on lat out of range", {
  expect_error(calculateMoonlightIntensity(91,  LON, DATE_NIGHT, E), "'lat'")
  expect_error(calculateMoonlightIntensity(-91, LON, DATE_NIGHT, E), "'lat'")
})

test_that("error on lon out of range", {
  expect_error(calculateMoonlightIntensity(LAT,  181, DATE_NIGHT, E), "'lon'")
  expect_error(calculateMoonlightIntensity(LAT, -181, DATE_NIGHT, E), "'lon'")
})

test_that("error on non-positive extinction coefficient", {
  expect_error(calculateMoonlightIntensity(LAT, LON, DATE_NIGHT,  0), "'e'")
  expect_error(calculateMoonlightIntensity(LAT, LON, DATE_NIGHT, -1), "'e'")
})

test_that("error on e with length > 1", {
  expect_error(calculateMoonlightIntensity(LAT, LON, DATE_NIGHT, c(0.21, 0.24)), "'e'")
})

test_that("error when lat length mismatches date length", {
  dates <- seq(DATE_NIGHT, length.out = 5, by = "1 hour")
  expect_error(
    calculateMoonlightIntensity(c(LAT, -33), LON, dates, E),
    "'lat' must be either length 1"
  )
})

test_that("error when lon length mismatches date length", {
  dates <- seq(DATE_NIGHT, length.out = 5, by = "1 hour")
  expect_error(
    calculateMoonlightIntensity(LAT, c(LON, 152), dates, E),
    "'lon' must be either length 1"
  )
})

# ---------------------------------------------------------------------------
# Input validation — warnings
# ---------------------------------------------------------------------------

test_that("warning for latitude beyond Arctic/Antarctic circle", {
  expect_warning(
    calculateMoonlightIntensity(70, LON, DATE_NIGHT, E),
    "Arctic/Antarctic circle"
  )
  expect_warning(
    calculateMoonlightIntensity(-70, LON, DATE_NIGHT, E),
    "Arctic/Antarctic circle"
  )
})
