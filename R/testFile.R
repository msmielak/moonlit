#
# #Testing function
#
# t           # interval for sample values -  "15 mins"  is more than enough, can go down to "1 hour" probably for speed
# e           # extinction coefficient - the same as the main function, for instance, 0.26
# timezone    # time zone of the data - usually in the format "Continent/City", i.e. for Poland: "Europe/Warsaw"
# date        # date time as POSIXct with the local time zone. If needed use as.POSIXct(date, tz=timezone)
# lat         # latitude, numerical decimal
# lon         # longitude, numerical decimal
#
#
#
#
# library(dplyr)
# alldetections <- sample_n(alldetections, 500)
#
# #input values for testing
# t <- "15 mins"
# e <- 0.26
# timezone <- "Australia/Brisbane"
# date <- as.POSIXct(alldetections$date, tz=timezone)
# lat <- alldetections$lat
# lon <- alldetections$lon
#
#
# test <- calculateMoonlightStatistics(lat=lat, lon=lon, date=date, e=0.26, t="1 hour", timezone="Australia/Brisbane")
#
#
#


#
#
#
#
# #######################################################
# #######################################################
#
# #creating a dataframe of reference illumination values - mean, median, max
# reference <- data.frame(nightStart, lat, lon)
#
# #removing duplicates to save computing time
# reference <- unique(reference)
#
#
# #loop for calculating moonlight intensity for a given day
#
# #initialise columns first
# reference$max <- NA
# reference$median <- NA
# reference$mean <- NA
# reference$moonPhase <- NA
#
#
# # for each cell from row 1 to the end of the dataframe
# #set start and stop (24 hours from noon to noon)
# start <- reference$nightStart+hours(12)
# stop <- reference$nightStart+hours(36)
#
# #initialise first set of
# #refTime <- seq(from=start[1], to=stop[1], by="15 mins")
#
# #going night by night
# for(i in 1:nrow(reference)) {
#
#   #generate sequence
#   refTime[i] <- seq(from=start[i], to=stop[i], by="15 mins")
#
#   #calculate moonlight values
#   calculateMoonlightIntensity(lat = lat[i])
#
# }
#
#
#
#
#
# ### testing
#
# for(i in 1:3)) {
#
#   #generate sequence
#   refTime[i] <- seq(from=start[i], to=stop[i], by="15 mins")
#
#   #calculate moonlight values
#   d <- calculateMoonlightIntensity(lat = lat[i], lon=lon[i], date=refTime[i], e=0.2)
#
#
#
# }
#
#
#
#
#
#
# #alldetections$date <- alldetections$`Date/Time Original`
# #alldetections$date <- as.POSIXct(alldetections$date, tz="Australia/Brisbane")
# attr(alldetections$date, "tzone") <- "Australia/Brisbane"
#
# attr(alldetections$date, "tzone")
#
#
# date <- as.POSIXct(alldetections$date)
# date <- as.POSIXct(alldetections$date, tz="Australia/Brisbane")
# attr(date, "tzone") <- "UTC"
#
#
# date <-  as.Date(date)
# lat <- alldetections$lat
# lon <- alldetections$lon
#
# data <- data.frame(date, lat, lon)
#
# library(suncalc)
# sunTimes <- getSunlightTimes(data=data)
#
#
# date <- alldetections$date
#
#
#
# start <- nightStart[1]+hours(12)
# attr(start, "tzone")
# attr(start, "tzone") <- "Australia/Brisbane"
#
# stop <- nightStart[1]+hours(36)
# attr(stop, "tzone") <- "Australia/Brisbane"
#
# refTime <- seq(from=start, to=stop, by="15 mins")
#
# data.frame(refTime, lat[1], lon[1])
#
# d <- calculateMoonlightIntensity(lat = lat[1], lon=lon[1], date=refTime, e=0.26)
# d <-
#
#
#
#  library(dplyr)
#  alldetections <- sample_n(alldetections, 500)
