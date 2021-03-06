#******************************************************************
#* function for convertion mmhg -> mbar / hPa                     *
#******************************************************************
mb_hpa <- function(mmhg){
 1.333224*mmhg
}
#******************************************************************
#* function for convertion mbar / hPa -> mmhg                     *
#******************************************************************
mmhg <- function(mb_hpa){
 mb_hpa/1.333224
}
#******************************************************************
#* function for convertion atm -> mbar / hPa                      *
#******************************************************************
mbhpa <- function(atm){
 1013.25*atm 
}
#******************************************************************
#* function for convertion mbar / hPa -> atm                      *
#******************************************************************
atm <- function(mbhpa){
  mbhpa/1013.25 
}
#******************************************************************
#* function for convertion psi -> mbar / hPa                      *
#******************************************************************
hpa_mb <- function(psi){
 68.948*psi 
}
#******************************************************************
#* function for convertion mbar / hPa -> psi                      *
#******************************************************************
psi <- function(mb_hpa){
 mb_hpa/68.948 
}
#******************************************************************
#* function for convertion fahrenheit -> celsius                  *
#******************************************************************
fcelsius <- function(fahrenheit){
 (5*(fahrenheit-32))/9 
}
#******************************************************************
#* function for convertion kelvin -> celsius                      *
#******************************************************************
kcelsius <- function(kelvin){
 kelvin-273.15 
}
#******************************************************************
#* function for convertion hour.minutes in hours                  *
#******************************************************************
call_hours <- function(hour_minute){
  dec_val <- (hour_minute-floor(hour_minute))/0.6
  round(floor(hour_minute) + dec_val, digits = 1)
}