#' long.calc {QC}
#' 
#' Longitude Calculation
#'
#' @Description
#'
#' Function to calculate Longitude in degrees,
#' having coordenates in degrees, minutes, seconds and direction.
#'
#' @Usage
#'
#' long.calc()
#'
#' @Details
#'
#' This function will ask to introduce the coordenates in degrees, minutes, seconds and direction.
#'
#' @Author: Pedro Gomes
#'
long.calc <- function() {
  #  
  long_deg <- as.numeric(readline("Introduce longitude degrees:  "))
  long_min <- as.numeric(readline("Introduce longitude minuts:  "))
  long_sec <- as.numeric(readline("Introduce longitude seconds:  "))
  long_dir <- as.character(readline("Introduce longitude direction: ")) 
  
  # calculating long_degree:
  writeLines("Calculating Longitude in degrees...")
  if(long_deg > -180 & long_deg < 180) {
    if(long_min >= 0 & long_min <= 60) {
      minuts <- long_min/60.
    } else {
      minuts <- 0.0
    }
    if(long_sec >= 0 & long_sec <= 60) {
      seconds <- long_sec/3600.
    } else {
      seconds <- 0.0
    }
  } else if(long_deg > 180 & long_deg <= 360) {
    writeLines("Value btw 0 and 360 degrees. Recalculating longitude...")
    long_deg <- 360 - long_deg 
    long_dir <- "W"
    if(long_min >= 0 & long_min <= 60) {
      minuts <- long_min/60.
    } else {
      minuts <- 0.0
    }
    if(long_sec >= 0 & long_sec <= 60) {
      seconds <- long_sec/3600.
    } else {
      seconds <- 0.0
    }
  } else { 
    if(long_deg < -180 | long_deg > 360) {
      writeLines("Degrees not correct!!")
      stop()
    }
  } 
  long <- long_deg + minuts + seconds
  if(long_dir == "w" | long_dir == "W") {
    if(long_dir == "w" | long_dir == "W") {
      long <- -long
    }
  } else { writeLines("Direction not correct, Longitude will point to East coordenates") }
  return(long)
}
# End function long.calc