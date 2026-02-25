# Test location: Armidale, NSW, Australia
LAT <- -30.5
LON <- 151.5
TZ  <- "Australia/Sydney"
E   <- 0.21
T   <- "1 hour"   # coarse resolution to keep tests fast

# Four records spanning two nights:
#   Records 1-3 → night of 27 Nov (same night, different hours)
#   Record  4   → night of 28 Nov (different night)
DATES <- as.POSIXct(c(
  "2023-11-27 21:00:00",  # afternoon = TRUE  → night starts Nov 27
  "2023-11-27 23:00:00",  # afternoon = TRUE  → night starts Nov 27
  "2023-11-28 02:00:00",  # afternoon = FALSE → night started Nov 27
  "2023-11-28 22:00:00"   # afternoon = TRUE  → night starts Nov 28
), tz = TZ)

# ---------------------------------------------------------------------------
# Output structure
# ---------------------------------------------------------------------------

test_that("returns a data frame with the correct columns", {
  result <- calculateMoonlightStatistics(LAT, LON, DATES, E, T, TZ)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("sunset", "sunrise", "date",
                         "meanMoonlightIntensity", "maxMoonlightIntensity",
                         "minMoonlightIntensity", "meanMoonPhase",
                         "maxMoonPhase", "minMoonPhase"))
})

test_that("returns one row per input record", {
  result <- calculateMoonlightStatistics(LAT, LON, DATES, E, T, TZ)
  expect_equal(nrow(result), length(DATES))
})

# ---------------------------------------------------------------------------
# Correctness
# ---------------------------------------------------------------------------

test_that("sunrise is always after sunset", {
  result <- calculateMoonlightStatistics(LAT, LON, DATES, E, T, TZ)
  expect_true(all(result$sunrise > result$sunset))
})

test_that("records on the same night share identical statistics", {
  result <- calculateMoonlightStatistics(LAT, LON, DATES, E, T, TZ)
  # Records 1, 2, 3 all belong to the night of 27→28 Nov
  expect_equal(result$meanMoonlightIntensity[1], result$meanMoonlightIntensity[2])
  expect_equal(result$meanMoonlightIntensity[1], result$meanMoonlightIntensity[3])
  expect_equal(result$sunset[1], result$sunset[2])
  expect_equal(result$sunset[1], result$sunset[3])
})

test_that("records on different nights have different statistics", {
  result <- calculateMoonlightStatistics(LAT, LON, DATES, E, T, TZ)
  # Record 4 is on a different night from records 1-3
  expect_false(isTRUE(all.equal(result$sunset[1], result$sunset[4])))
})

test_that("input row order is preserved in output", {
  result <- calculateMoonlightStatistics(LAT, LON, DATES, E, T, TZ)
  expect_equal(as.numeric(result$date), as.numeric(DATES))
})

test_that("max >= mean >= min for moonlight intensity", {
  result <- calculateMoonlightStatistics(LAT, LON, DATES, E, T, TZ)
  expect_true(all(result$maxMoonlightIntensity >= result$meanMoonlightIntensity))
  expect_true(all(result$meanMoonlightIntensity >= result$minMoonlightIntensity))
})

test_that("max >= mean >= min for moon phase", {
  result <- calculateMoonlightStatistics(LAT, LON, DATES, E, T, TZ)
  expect_true(all(result$maxMoonPhase >= result$meanMoonPhase))
  expect_true(all(result$meanMoonPhase >= result$minMoonPhase))
})

# ---------------------------------------------------------------------------
# Input validation — errors
# ---------------------------------------------------------------------------

test_that("error on non-POSIXct date", {
  expect_error(
    calculateMoonlightStatistics(LAT, LON, "2023-11-27", E, T, TZ),
    "'date' must be a POSIXct object"
  )
})

test_that("error on invalid timezone", {
  expect_error(
    calculateMoonlightStatistics(LAT, LON, DATES[1], E, T, "Not/ATimezone"),
    "not a recognised timezone"
  )
})

test_that("error on non-character sampling interval", {
  expect_error(
    calculateMoonlightStatistics(LAT, LON, DATES[1], E, 30, TZ),
    "'t' must be a single character string"
  )
})

test_that("error on lat out of range", {
  expect_error(
    calculateMoonlightStatistics(91, LON, DATES[1], E, T, TZ),
    "'lat'"
  )
})

test_that("error on non-positive extinction coefficient", {
  expect_error(
    calculateMoonlightStatistics(LAT, LON, DATES[1], 0, T, TZ),
    "'e'"
  )
})

test_that("error when lat length mismatches date length", {
  expect_error(
    calculateMoonlightStatistics(c(LAT, -33), LON, DATES, E, T, TZ),
    "'lat' must be either length 1"
  )
})
