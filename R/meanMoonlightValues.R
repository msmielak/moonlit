


#' Title
#'
#' @param lat
#' @param lon
#' @param date
#' @param e
#' @param t
#' @param timezone
#'
#' @import suncalc
#' @import stats
#' @import graphics
#' @import lubridate
#' @import openair
#'
#' @return data
#' @export
#'



### function to calculate mean moonlight values for a night
### requirest the same input as calculateMoonlightIntensity() function, plus an extra value t representing temporal resolution (in minutes)
### for larger datastes (thousands of nights) t = 30 is a reasonable value, for smaller ones you can increase the resolutions to 15 or more.

calculateMoonlightStatistics <- function(lat, lon, date, e, t, timezone)





{



  #take date-time which is in local time zone and divine into before- and afternoon.
  #Assign a variable for that - it will be needed to decide which date to use for sunset and sunrise
  afternoon <- hour(date) >= 12




  # To calculate statistics for each night we need to use night (from noon to noon) and not a day (from midnight to midnight)
  # we use new variable afternoon the day when this night started
  # for records from noon to midnight nothing changes, for records from midnight to noon we assign the previous day etc

  # for getSunlightTimes we need date and coordinates:
  day <-  as.Date(date, tz=timezone)


  #for further calculations convert to UTC
  attr(date, "tzone") <- "UTC"


  #we arrange needed values into a data frame
  data <- data.frame(day, lat, lon, afternoon)


  ### and we assign sunrise and sunset to each record using a for() loop

  for(i in 1:nrow(data)) {
  #for(i in 1:100) {

   #progress(i, progress.bar = T)

    #if the record is in the afternoon (in original time zone)
    if (afternoon[i] == TRUE){

      #calculate sunset for the same day
      sunTimes <- getSunlightTimes(day[i], lat[i], lon[i], keep="sunset")

      #and parse it into dataset
      data$sunset[i] <- sunTimes$sunset

      #calculate sunrise for the next day
      sunTimes <- getSunlightTimes(day[i]+1, lat[i], lon[i], keep="sunrise")

      #and parse it
      data$sunrise[i] <- sunTimes$sunrise

    #if the record isin the morning
    }else{

      #calculate sunset for the previous day
      sunTimes <- getSunlightTimes(day[i]-1, lat[i], lon[i], keep="sunset")

      #and parse it into dataset
      data$sunset[i] <- sunTimes$sunset

      #calculate sunrise for the same day
      sunTimes <- getSunlightTimes(day[i], lat[i], lon[i], keep="sunrise")

      #and parse it
      data$sunrise[i] <- sunTimes$sunrise

    }

  }



  data$date <- date

  #then convert to POSIX at UTM
  data$sunset <- as.POSIXct(data$sunset, origin = "1970-01-01", tz="UTC")
  data$sunrise <- as.POSIXct(data$sunrise, origin = "1970-01-01", tz="UTC")

  #and assign local timezone for readibility
  attr(data$date, "tzone") <- timezone
  attr(data$sunrise, "tzone") <- timezone
  attr(data$sunset, "tzone") <- timezone


  ### Second loop, now calculating moonlight statistics for each record

    for(i in 1:nrow(data)) {
    #for(i in 1:1) {


    nightSeq <- seq(from=data$sunset[i], to=data$sunrise[i], by=t)

   a <- calculateMoonlightIntensity(date=nightSeq, lat=data$lat[i], lon=data$lon[i], e=e)


    data$meanMoonlightIntensity[i] <- mean(a$moonlightModel)
    data$maxMoonlightIntensity[i] <- max(a$moonlightModel)
    data$minMoonlightIntensity[i] <- min(a$moonlightModel)
    data$meanMoonPhase[i] <- mean(a$moonPhase)
    data$maxMoonPhase[i] <- max(a$moonPhase)
    data$minMoonPhase[i] <- min(a$moonPhase)


  }

  return(data)


}
