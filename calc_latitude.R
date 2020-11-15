#' lat.calc {QC}
#' 
#' Latitude Calculation
#'
#' @Description
#'
#' Function to calculate Latitude in degrees,
#' having coordenates in degrees, minutes, seconds and direction.
#'
#' @Usage
#'
#' lat.calc()
#'
#' @Details
#'
#' This function will ask to introduce the coordenates in degrees, minutes, seconds and direction.
#'
#' @Author: Pedro Gomes
#'
lat.calc <- function() {
  #
  # 
  lat_deg <- as.numeric(readline("Introduce latitude degrees:  "))
  lat_min <- as.numeric(readline("Introduce latitude minuts:  "))
  lat_sec <- as.numeric(readline("Introduce latitude seconds:  "))
  lat_dir <- as.character(readline("Introduce latitude direction:  "))
  
  # calculating lat_degree:
  writeLines("Calculating Latitude in degrees...")
  if(lat_deg < -90 | lat_deg > 90) {
    writeLines("Latitude degrees not correct!!")
    stop()
  } 
  if(lat_min >= 0 & lat_min <= 60) {
    minuts <- lat_min/60.
  } else {
    minuts <- 0.0
  }
  if(lat_sec >= 0 & lat_sec <= 60) {
    seconds <- lat_sec/3600.
  } else {
    seconds <- 0.0
  }
  lat <- lat_deg + minuts + seconds
  if(lat_dir == "s" | lat_dir == "S" | lat_dir == "n" | lat_dir == "N") {
    if(lat_dir == "s" | lat_dir == "S") {
      lat <- -lat
    }          
  } else { writeLines("Direction not correct, Lat will point to North Hemisphere") }
  return(lat)
}
# End function lat.calc