#' day.duration {QC}
#'
#' Sunshine Duration
#'
#' @Description
#'
#' Function that calculates one aproximation of daily maximum sunshine. To work properly needs 
#' to be given tree arguments and other tree regarding localization (Latitude, Longitude, Altitude).
#'  
#' @Usage 
#'  
#' day.duration(iyear,imonth,iday) 
#' day.duration(2019,1,20) 
#'  
#' @Arguments  
#'  
#' iyear, imonth, iday   numeric value of year, month and day needed to run this function.
#'
#' @Details
#'
#' Based in one algoritm this function will calculates aproximately max sunshine for a year month and day
#' in determinate location.
#'
#' This function needs also Latitude, Longitude and Altitude of place/station. Is not in arguments for function
#' meaning this three arguments needs to be previously "assign" or define to global environment in order to work.  
#'
#' @Author: Pedro Gomes
#'
day.duration <- function(iyear,imonth,iday) {
  #  
  # define test values
  #iyear  <- 1977
  #imonth <- 1
  #iday   <- 15
  #lat      <- 35.0
  #long     <- -8.0
  #alt      <- 95.0
  #
  if(exists("lat") & exists("long") & exists("alt")) {
  } else {
    writeLines("Latitude or/and Longitude or/and Altitude not found.")
    writeLines("Variables: lat,long and alt must be assign to global environment")
    stop()
  }
  
  # Julian day number (of date user want)
  jdn1 <- (1461*(iyear+4800+(imonth-14)/12))/4
  jdn2 <- (367*(imonth-2-12*((imonth-14)/12)))/12
  jdn3 <- (3*((iyear+4900+(imonth-14)/12)/100))/4
  jdn4 <- iday-32075
  jd   <- c(jdn1,jdn2,-jdn3,jdn4)
  jdn  <- sum (jd)
  
  # n = number of days since 1/1/2000 12:00
  n <- jdn-2451545.0+0.0008   
  
  # mean solar noon
  jmean <- n-(long/360.0)
  
  # Solar mean anomaly m
  m1 <- 357.5291+(0.98569928*jmean)
  m <- m1 %% 360
  mrad <- (m*pi)/180.0
  mrad1 <- 2*mrad
  mrad2 <- 3*mrad
  
  # Equation of center
  c <- (1.9148 * sin(mrad))+(0.0200 * sin(mrad1))+(0.0003 * sin(mrad2))
  
  # Ecliptic longitude
  lambda1 <- m+c+180.0+102.9372
  lambda <- lambda1 %% 360
  lambdarad <- (lambda*pi)/180.0
  
  #Solar transit
  #jtransit <- 2451545.0 + jmean + (0.0053*sin(mrad)) - (0.0069*sin(2*lambdarad))
  
  # declination of the sun
  delta1 <- sin(lambdarad) * sin(0.409105)  #23.44 = 0.409105 rad
  delta  <- asin(delta1)
 
   # hour angle (w)
  if(alt > 200.0) {
    correc <- -0.83 - ((2.076*sqrt(alt))/60)
  } else {
    correc <- -0.83
  }
 
  #print(correc)
  correc_rad <- (correc*pi)/180.0
  lat_rad <- (lat*pi)/180.0
  w1 <- sin(correc_rad) - (sin(lat_rad)*delta1)
  w2 <- cos(lat_rad)*cos(delta)
  w3 <- w1 / w2
  w <- acos(w3)
  w <- (w*180.0)/pi
  
  #sunset (in julian)
  #sunset <- jtransit + (w/360.0)
  
  #sunrise (in julian)
  #jsunrise <- jtransit - (w/360.0)
  
  max_solar_day <- (w/15)*2
  if(max_solar_day > 24.0) { max_solar_day <- 24.0 } 
  if(max_solar_day < 0.0) { max_solar_day <- 0.0 }
  return(max_solar_day)
  #print(max_solar_day)
}
# End function day.duration