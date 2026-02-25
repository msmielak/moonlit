#' Calculate moonlight intensity
#'
#' This function predicts moonlight intensity on the ground for any given place
#' and time, based on the location, position of the moon and number of correction
#' factors
#'
#' @param lat Latitude, numerical decimal
#' @param lon Longitude, numerical decimal
#' @param date Date time as POSIXct with the local time zone. If needed use
#'   as.POSIXct(date, tz=timezone)
#' @param e Extinction coefficient - a single numerical value depending on the
#'   altitude. Average extinction coefficients (magnitude per air mass) are as
#'   follows: At sea level: 0.28; at 500m asl: 0.24; at 1000m asl: 0.21; at
#'   2000m asl: 0.16
#'
#' @return A data frame with the following columns:
#'   \item{night}{Logical, TRUE when sun is below the horizon}
#'   \item{sunAltDegrees}{Solar altitude in degrees}
#'   \item{moonlightModel}{Predicted moonlight illumination, relative to an "average" full moon}
#'   \item{twilightModel}{Predicted twilight illumination in lux}
#'   \item{illumination}{Combined moon and twilight intensity, in lux}
#'   \item{moonPhase}{Lunar phase as a numerical value (\% of moon face illuminated)}
#'
#'
#' @export
#'
#' @examples
#' lat  <- 52.2297
#' lon  <- 21.0122
#' date <- as.POSIXct("2023-06-15 22:00:00", tz = "Europe/Warsaw")
#' result <- calculateMoonlightIntensity(lat, lon, date, e = 0.21)
#'


calculateMoonlightIntensity <- function(lat, lon, date, e)


{





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

  # --- Physical constants ---
  # Mean Earth-Moon distance (km); inverse-square law reference (Meeus 1991)
  MOON_MEAN_DISTANCE_KM <- 384400
  # Mean full moon illuminance at zenith (lux); scales normalised model to lux
  FULL_MOON_ILLUMINANCE_LUX <- 0.32
  # Solar altitude (degrees) below which twilight contribution is negligible
  TWILIGHT_CUTOFF_DEG <- -18
  # Kasten & Young (1989) air mass formula coefficients
  AIRMASS_A <- 0.025
  AIRMASS_B <- 11
  # Pogson's ratio: converts magnitude difference to flux ratio via 10^(POGSON * mag)
  POGSON <- -0.4

  #extiction coefficient based on the user-assigned value
  extCoef <- e




  #creating a dataframe to calculate sun and moon positions
  d1 <- data.frame(date, lat, lon)


  #calculating values using suncalc library
  moonPosition <- suncalc::getMoonPosition(data = d1)
  moonIllum <- suncalc::getMoonIllumination(date = date)
  sunPosition <- suncalc::getSunlightPosition(data = d1)



  #using one of the outputs to start building a table for the results
  night<- moonIllum


  #integrating into the dataset


  #calculated values
  night$moonAlt <- moonPosition$altitude
  night$sunAlt <- sunPosition$altitude
  night$moonAltDegrees <- (180/pi)*night$moonAlt
  night$sunAltDegrees <- (180/pi)*night$sunAlt
  night$moonIllum <- moonIllum$fraction
  night$moonIllumCorrected <- (night$moonAlt > 0) * night$moonIllum
  night$phase <- moonIllum$phase
  night$distance <- moonPosition$distance

# TRUE when sun is below the geometric horizon (sunAltDegrees < 0)
night$night <- night$sunAltDegrees < 0

################################################################
### Modelling moonlight intensity on the ground ################
################################################################

# Model provides an iluminance value for a given location and time. Outcome of
# the model is a scalar quantity and represents a proportion of reference
# illumination value. The reference value, in this case, is luminance of full
# moon at 384400 km (average distance) in zenith. Model includes 4 variables,
# each providing a correction factor of 0-1: 1) visual extinction 2) distance to
# the moon relative to mean distance 3) phase effect 4) Angle of incidence of
# moonlight Final value is obtained by multiplying all 4 values.

############################## 1) Visual extinction of the stars depending on
###the angle of the atmosphere equations provided in: Green, Daniel W. E. 1992.
###"Magnitude Corrections for Atmospheric Extinction." International Comet
###Quarterly 14: 55-59 Average extinction coefficients (magnitude per air mass)
###are as follows:
#At sea level: 0.28
#At 500m asl: 0.24
#at 1000m asl: 0.21
#at 2000m asl: 0.16
#Default value in the model is for 1000m asl and, if needed,
###should be changed below: ############################

#Calculating thikness of the atmosphere (airmass value) at given elevation. Note
#that elevation is above the horizon, so we need to use 90-elevation from
#suncalc model. Might show false results for moon altitude <0, but these always
#have value 0 in end model so it doesn't matter. moon altitude must be in
#radians!

zenith <- pi/2 - night$moonAlt
airmass_denom <- cos(zenith) + AIRMASS_A * exp(-AIRMASS_B * cos(zenith))
# The denominator passes through zero near moonAlt ~ -4 degrees, producing Inf.
# The formula is only physically valid above the horizon; clamp to a minimum of
# 1 (zenith air mass) so all downstream calculations remain finite. Rows with
# moonAlt <= 0 are zeroed in the final model regardless.
night$airMass <- ifelse(airmass_denom > 0, 1 / airmass_denom, 1)


#difference in magnitudes - in zenith the absortion reduces brightess by 0.24
#magnitude, or ~20%, a minimal value possible. We will calculate the difference
#between extinction at given angle and extinction at zenith and transform it to
#proportion

night$airMassMagnitude <- (night$airMass-1)*extCoef

#proportion
night$atmExtinction <- 10^(POGSON * night$airMassMagnitude)



##########################################
####### 2) Distance to the moon ##########
##########################################

### the illumination is proportional to 1/(distance^2) (inverse-square law,
### Meeus 1991), therefore relative illumination:
night$distanceIllum <- (MOON_MEAN_DISTANCE_KM / night$distance)^2


###########################################
###### 3) phase effect ####################
###########################################

###Relationship between phase angle (Sun-Moon-observer) and normalised, disc-integrated brightness of the moon
###Data and spline function are defined in R/spline_data.R and computed once at package load.
###Source: Buratti, Bonnie J., John K. Hillier, and Michael Wang. 1996. "The
###Lunar Opposition Surge: Observations by Clementine." Icarus 124 (December):
###490-99. https://doi.org/10.1006/icar.1996.0225.

#correction factor for model
night$phaseEffect <- .brightness_spline(night$moonIllum)

########################################
###### 4) angle of incidence ###########
########################################

### correction factor is sine of the angle of moon's altitude (Austin et al. 1976)
night$incidence <- sin(night$moonAlt)

######################################################
##### total correction factor #######################

# Multiply all four correction factors; set to 0 when moon is below the horizon.
night$illuminationModelComponents <- night$atmExtinction * night$distanceIllum * night$phaseEffect * night$incidence
night$moonlightModel <- ifelse(night$moonAlt > 0, night$illuminationModelComponents, 0)









#############################################################
##############################################################
#### Twilight illumination from empirical data https://www.jstor.org/stable/44612241

####  Sun Position and Twilight Times for Driver Visibility Assessment; Duane D.
####  MacInnis, Peter B. Williamson and Geoffrey P. Nielsen; SAE Transactions;
####  Vol. 104, Section 6: JOURNAL OF PASSENGER CARS: Part 1 (1995), pp. 759-783

####  Values calulated from the paper for angles -18 to 5 degrees, by 0.25 degree, imported to R and spline function applied
##############################################################


# Twilight data and spline function are defined in R/spline_data.R and computed once at package load.
# Source: MacInnis et al. (1995). Values for solar angles -18 to +5 degrees.

night$twilightModel <- .twilight_spline(night$sunAltDegrees)

#the model can only be interpolated to -18 degrees, so need to assign 0 to everything below -18 degrees
night$twilight <- night$sunAltDegrees > TWILIGHT_CUTOFF_DEG
night$twilightModel <- night$twilightModel*night$twilight


##############################################################
### Combining twilight illumination with lunar illumination ###

night$illumination <- night$moonlightModel * FULL_MOON_ILLUMINANCE_LUX + night$twilightModel

#generating output data frame
d1$night <- night$night
d1$sunAltDegrees <- night$sunAltDegrees
d1$moonAltDegrees <- night$moonAltDegrees
d1$moonlightModel <- night$moonlightModel
d1$twilightModel <- night$twilightModel
d1$illumination <- night$illumination
d1$moonPhase <- night$moonIllum



return(d1)

}


######################################################
###### Extinction coefficient - in development #######
######################################################

##### Calculating an extinction coefficient based on the observer's elevation above sea level
# extinction coefficient based on the
# Peak lunar irradiance is around 560 nm (Veilleux & Cummings, 2012  https://doi.org/10.1242/jeb.071415)
# Aerosol Optical Depth (AOD) is assumed to be 0.15, an average value

#' Calculate extinction coefficient based on elevation of the observer
#'
#' @param elev elevation in meters asl
#'
#' @return A single numeric value: the estimated extinction coefficient for the
#'   given elevation.
#' @export
#'
#' @examples
#' # Extinction coefficient at sea level
#' suppressWarnings(elevExtCoeff(0))
#'
#' # Extinction coefficient at 1000 m asl (e.g. approximate for many study sites)
#' suppressWarnings(elevExtCoeff(1000))

elevExtCoeff <- function(elev) {

  warning("elevExtCoeff() is experimental. Validate results before use in research.")

  if (!is.numeric(elev) || any(elev < 0, na.rm = TRUE)) {
    stop("'elev' must be a non-negative numeric value (elevation in metres above sea level).")
  }

  # Rayleigh scattering coefficient at peak lunar irradiance (~560 nm)
  # Source: Veilleux & Cummings (2012) https://doi.org/10.1242/jeb.071415
  RAYLEIGH_COEFF <- 0.1451
  # Atmospheric scale height (metres)
  SCALE_HEIGHT_M <- 7995
  # Aerosol optical depth baseline (AOD assumed = 0.15)
  AEROSOL_BASELINE <- 0.136

  e <- RAYLEIGH_COEFF * exp(-elev / SCALE_HEIGHT_M) + AEROSOL_BASELINE

  return(e)

}
