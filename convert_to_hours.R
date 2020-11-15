#******************************************************************
#* function for convertion hour.minutes in hours                  *
#******************************************************************
call_hours <- function(hour_minute){
  dec_val <- (hour_minute-floor(hour_minute))/0.6
  round(floor(hour_minute) + dec_val, digits = 1)
}
# End function call_hours