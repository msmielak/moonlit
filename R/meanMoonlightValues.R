

#' Calculate nightly moonlight statistics
#'
#' @param lat latitude, numerical decimal
#' @param lon longitude, numerical decimal
#' @param date date time as POSIXct with the local time zone. If needed use
#'   as.POSIXct(date, tz=timezone)
#' @param e extinction coefficient - a single numerical value depending on the
#'   altitude. Average extinction coefficients (magnitude per air mass) are as
#'   follows: (At sea level: 0.28; at 500m asl: 0.24; at 1000m asl: 0.21; at
#'   2000m asl: 0.16)
#' @param t sampling interval. It is used in seq() function so the same values
#'   are accepted: A character string, containing one of "sec", "min", "hour".
#'   This can optionally be preceded by a (positive or negative) integer and a
#'   space, or followed by "s". Example: "15 mins", "3 hour" etc.
#' @param timezone time zone of the data usually in the format "Continent/City",
#'   i.e. for Poland: "Europe/Warsaw"
#'
#' @importFrom suncalc getSunlightTimes
#' @importFrom lubridate hour
#'
#' @return A data frame with one row per input record and the following columns:
#'   \item{sunset}{Time of sunset for the night the record belongs to}
#'   \item{sunrise}{Time of sunrise for the night the record belongs to}
#'   \item{date}{Original date-time from the input}
#'   \item{meanMoonlightIntensity}{Mean nightly moonlight illumination}
#'   \item{maxMoonlightIntensity}{Maximum nightly moonlight illumination}
#'   \item{minMoonlightIntensity}{Minimum nightly moonlight illumination}
#'   \item{meanMoonPhase}{Mean nightly moon phase (fraction of disc illuminated)}
#'   \item{maxMoonPhase}{Maximum nightly moon phase}
#'   \item{minMoonPhase}{Minimum nightly moon phase}
#' @export
#'
#' @examples
#' \donttest{
#' lat  <- 52.2297
#' lon  <- 21.0122
#' date <- as.POSIXct("2023-06-15 22:00:00", tz = "Europe/Warsaw")
#' result <- calculateMoonlightStatistics(lat, lon, date, e = 0.21,
#'                                        t = "30 mins",
#'                                        timezone = "Europe/Warsaw")
#' }


### Function to calculate nightly moonlight statistics. Requires the same input
### as calculateMoonlightIntensity() plus t (temporal resolution) and timezone.
### For large datasets (thousands of nights) t = "30 mins" is a reasonable
### value; for smaller ones you can increase resolution to "15 mins" or finer.
###
### Performance note: sunset/sunrise and moonlight statistics are computed once
### per unique (night_date, lat, lon) combination, then joined back to all
### records. This avoids redundant computation when multiple records share the
### same night and location.

calculateMoonlightStatistics <- function(lat, lon, date, e, t, timezone) {

  # --- Input validation ---
  if (!inherits(date, "POSIXct")) {
    stop("'date' must be a POSIXct object. Use as.POSIXct(date, tz = timezone) to convert.")
  }
  if (!is.numeric(lat) || any(lat < -90 | lat > 90, na.rm = TRUE)) {
    stop("'lat' must be numeric and in the range [-90, 90].")
  }
  if (!is.numeric(lon) || any(lon < -180 | lon > 180, na.rm = TRUE)) {
    stop("'lon' must be numeric and in the range [-180, 180].")
  }
  if (!is.numeric(e) || length(e) != 1 || e <= 0) {
    stop("'e' must be a single positive numeric value. Typical values: 0.28 (sea level), 0.24 (500m), 0.21 (1000m), 0.16 (2000m).")
  }
  if (!is.character(t) || length(t) != 1) {
    stop("'t' must be a single character string, e.g. \"15 mins\" or \"1 hour\".")
  }
  if (!timezone %in% OlsonNames()) {
    stop(paste0("'", timezone, "' is not a recognised timezone. See OlsonNames() for valid values."))
  }
  n <- length(date)
  if (length(lat) != 1 && length(lat) != n) {
    stop("'lat' must be either length 1 or the same length as 'date'.")
  }
  if (length(lon) != 1 && length(lon) != n) {
    stop("'lon' must be either length 1 or the same length as 'date'.")
  }
  if (any(abs(lat) > 66.5, na.rm = TRUE)) {
    warning("Latitude beyond the Arctic/Antarctic circle (>66.5 degrees). Results may be unreliable due to polar day/night conditions.")
  }

  # Determine whether each record falls in the afternoon (>= 12:00 local time).
  # This is used to identify which calendar night the record belongs to.
  afternoon <- hour(date) >= 12

  # Convert POSIXct to calendar date in the local timezone, then relabel as UTC.
  # (workaround for timezone handling in getSunlightTimes)
  day <- as.Date(date, tz = timezone)
  attr(day, "tzone") <- "UTC"

  # The night a record belongs to started on 'day' (for afternoon records) or
  # on 'day - 1' (for morning records, i.e. the night that crossed midnight).
  night_date <- day - as.integer(!afternoon)

  # Build a per-record data frame. row_id preserves the original input order
  # after the merge step below.
  records <- data.frame(
    row_id     = seq_len(length(date)),
    date       = date,
    lat        = lat,
    lon        = lon,
    night_date = night_date
  )
  attr(records$date, "tzone") <- timezone

  # Extract unique (night_date, lat, lon) combinations. Sunset/sunrise and
  # moonlight statistics will be computed once for each of these, regardless
  # of how many input records share the same night and location.
  unique_nights <- unique(records[, c("night_date", "lat", "lon")])

  # --- Vectorised replacement for the original sunset/sunrise loop ---
  # Two calls total instead of 2 * nrow(records) individual calls.
  # getSunlightTimes() requires multiple lat/lon values via the 'data' argument.
  sunsets  <- getSunlightTimes(
    data = data.frame(date = unique_nights$night_date,
                      lat  = unique_nights$lat,
                      lon  = unique_nights$lon),
    keep = "sunset")
  sunrises <- getSunlightTimes(
    data = data.frame(date = unique_nights$night_date + 1,
                      lat  = unique_nights$lat,
                      lon  = unique_nights$lon),
    keep = "sunrise")

  unique_nights$sunset  <- as.POSIXct(sunsets$sunset,   origin = "1970-01-01", tz = "UTC")
  unique_nights$sunrise <- as.POSIXct(sunrises$sunrise, origin = "1970-01-01", tz = "UTC")
  attr(unique_nights$sunset,  "tzone") <- timezone
  attr(unique_nights$sunrise, "tzone") <- timezone

  # --- Replacement for the original moonlight statistics loop ---
  # Iterate over unique nights only, not over every input record.
  unique_nights$meanMoonlightIntensity <- NA_real_
  unique_nights$maxMoonlightIntensity  <- NA_real_
  unique_nights$minMoonlightIntensity  <- NA_real_
  unique_nights$meanMoonPhase          <- NA_real_
  unique_nights$maxMoonPhase           <- NA_real_
  unique_nights$minMoonPhase           <- NA_real_

  for (i in seq_len(nrow(unique_nights))) {

    nightSeq <- seq(from = unique_nights$sunset[i],
                    to   = unique_nights$sunrise[i],
                    by   = t)

    a <- calculateMoonlightIntensity(date = nightSeq,
                                     lat  = unique_nights$lat[i],
                                     lon  = unique_nights$lon[i],
                                     e    = e)

    unique_nights$meanMoonlightIntensity[i] <- mean(a$moonlightModel)
    unique_nights$maxMoonlightIntensity[i]  <- max(a$moonlightModel)
    unique_nights$minMoonlightIntensity[i]  <- min(a$moonlightModel)
    unique_nights$meanMoonPhase[i]          <- mean(a$moonPhase)
    unique_nights$maxMoonPhase[i]           <- max(a$moonPhase)
    unique_nights$minMoonPhase[i]           <- min(a$moonPhase)

  }

  # Join the nightly statistics back to all original records, then restore
  # the original input order.
  output <- merge(records, unique_nights, by = c("night_date", "lat", "lon"),
                  sort = FALSE)
  output <- output[order(output$row_id), ]

  output[, c("sunset", "sunrise", "date",
             "meanMoonlightIntensity", "maxMoonlightIntensity", "minMoonlightIntensity",
             "meanMoonPhase", "maxMoonPhase", "minMoonPhase")]

}
