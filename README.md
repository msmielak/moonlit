

<p align="center">
  <img align="center" src="graphics/moonlit_logo.png" width="250px"/>
</p>

# moonlit v 0.9
(c) Michal Smielak 2020  
Avaiblabe under CC BY-NC-SA 4.0 licence

## Description

R package providing biologically meaningful moonlight measures. Allows to study ecological and behavioural effects of changing moonlight intensity.
Particularly usefull in determining preference towards high or low illumination levels in different temporal scales (night, lunar cycle, seasons, years).


### Disclaimer

__This is an early release and functionalities might change. This package is currently pending a peer-review and detailed desctription will be available after it is published. I take no responsibility for the proper functioning of this package. If you have any questions, concerns or you would simply like to apply it to your data, I encourage to contact me directly. Otherwise, basic explaination of currently available functions is be available below.__


### Currently working functionalities:

- [x] Predicting moonlight intensity on the ground for any given place and time
- [x] Predicting twilight illumination levels 
- [x] Calculating nightly mean illumianation levels for a given location



## Using the moonlit library
### Installing
To install from github you need devtool package

```R
#install and load devtools
install.packages("devtools")
library(devtools)

#install moonlit library from github repo
install_github("msmielak/moonlit")

#load the moonlit library
library(moonlit)
```

### Functions
---

#### calculateMoonlightIntensity()


```R
calculateMoonlightIntensity(lat, lon, date, e)
```
Function requires as an input a matrix of values for location and date and a value of extinction coefficient *e*.  
Accepted formats:

* latitude and longitude - decimal degrees
* date - POSIXct
* Extinction coefficient *e* - a single numerical value depending on the altitude. Average extinction coefficients (magnitude per air mass) are as follows:
  - At sea level: 0.28
  - At 500m asl: 0.24
  - at 1000m asl: 0.21
  - at 2000m asl: 0.16

Function returns a data frame with following columns:

* **night** - a logical value, true when sun below the horizon for given date and location
* **sunAltDegrees** - solar altitude in degrees; this can be used to subset "true" night observations
* **moonlightModel** - predicted moonlight illumination, relative to an "average" full moon
* **twilightModel** - predicted twilight illumination in lx; this is a crude approximation based solely on the position of the sun, so use with caution.
* **illumination** - combined moon and twilight intensity, in lx. Moonlight converted to lx based on average full moon value of 0.32 lx.
* **moonPhase** - lunar phase in numerical value - % of moon face illuminated

It will also conveniently plot predicted values as points and moon phase as line, allowing for quick visual comparison of the two measures.

**Caution -  currently it assigns 0 when sun is above the horizon, even if the moon is visible. Normally, this is not a problem, becasue when sun is visible, moonlight is negligible, but it is worth keeping that in mind. I might change that at some point as there is a "night" field so users can filter out night-only records on their own.**

---
#### calculateMoonlightStatistics()
```R
calculateMoonlightStatistics(lat, lon, date, e, t, timezone)
```

This function calculates **nightly** statistics for moonlight illumination and moon phase.

For each record it will assing min, max and mean values for the night. For diurnal records, the **nearest** night is assigned, so for records before noon it returns statistics for the night that starts on the previous day, and for records after noon it assigns statistics for the night that starts on this day.

Function requires as an input a matrix of values for location and date, local time zone and a value of extinction coefficient *e*.  
Accepted formats: 

* lat - latitude, numerical decimal
* lon - longitude, numerical decimal
* date - date time as POSIXct with the local time zone. If needed use as.POSIXct(date, tz=timezone)
* e - extinction coefficient - the same as the main function, for instance, 0.26
* t - sampling interval -  15 minutes is more than enough, can go down to 1 hour for large datasets to save time.
It is used in seq() function so the same values are accepted: A character string, containing one of "sec", "min", "hour". This can optionally be preceded by a (positive or negative) integer and a space, or followed by "s". Example: "15 mins", "3 hour" etc.
* timezone - time zone of the data - usually in the format "Continent/City", i.e. for Poland: "Europe/Warsaw"


Function returns a data frame with following columns:

*	**sunset** and **sunrise** – time of sunset and sunrise between which the mean value is calcuated 
* **meanMoonlightIntensity** - mean value of modelled illumination for the night
* **minMoonlightIntensity** - min value of modelled illumination for the night
* **maxMoonlightIntensity** - max value of modelled illumination for the night
* **meanMoonPhase** - mean value of moon phase (% of moon illuminated)
* **minMoonPhase** - min value of moon phase (% of moon illuminated)
* **maxMoonPhase** - max value of moon phase (% of moon illuminated)



## References

* Kyba, C., A. Mohar, and T. Posch. How bright is moonlight? Astronomy & Geophysics. 2017, 58:1.31–31.32. DOI: 10.1093/astrogeo/atx025  
* Prugh LR, Golden CD. Does moonlight increase predation risk? Meta-analysis reveals divergent responses of nocturnal mammals to lunar cycles. Journal of Animal Ecology. 2014 Mar;83(2):504-14. DOI: 10.1111/1365-2656.12148.  
* Austin, R. H., Phillips, B. F. and Webb, D. J. Method for calculating moonlight illuminance at the earth's surface. Journal of Applied Ecology 13 (3): 741–48. DOI:10.2307/2402251  
* Green, D. W. E. Magnitude corrections for atmospheric extinction. International Comet Quarterly. 1992 (14):55-59  
* Agafonkin, V., Benoit T. Suncalc: Compute Sun Position, Sunlight Phases, Moon Position and Lunar Phase (version 0.4). 2018. https://CRAN.R-project.org/package=suncalc.  
* Buratti, Bonnie J., John K. Hillier, and Michael Wang. The Lunar Opposition Surge: Observations by Clementine.” Icarus. 1996 Dec;124:490–99. DOI: 10.1006/icar.1996.0225

