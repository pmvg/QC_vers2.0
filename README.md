# QC_vers2.0
Quality control for sunshine data version 2.0
#' Quality Control Test
#'
#' @Description
#'
#' This function test daily sunshine values to max daily value that could take and optionally
#' test with some statistic (mean or sum according to case applied).
#' 
#' Is also capable of apply statistic test to any variable, if all information is given. 
#' 
#' @Usage
#'
#' qc.test ()
#' qc.test (fileRead="file.csv", meanchk=FALSE, vname="ss")
#' qc.test ("file.csv",F,"ss")
#'
#' @Arguments:
#'
#' fileRead       Can write file name, this should be in R work directory if no path attach.
#'                Default: "ask for file". 
#'
#' meanchk        Logical, trigger chance to test mean or sum published with calculated within
#'                function. Default: FALSE.  
#'
#' vname          Character, can specify short name of variable.This argument is necessary, if not
#'                exists on specific field in SEF file (vbl). 
#'                Default: NA. (see C3S-DRS Guideline).
#'
#'
#' @Details
#' 
#' This function will work for any variable in terms of statistical tests for time been, 
#' calculating stats and comparate with published ones. Only sunshine hours will have specific tests. 
#'  
#' In some publications papers, after a certain date and in some places, sunshine hours start
#' to be designated by "insolation". Diference between both is in units, ie. sunshine hours its
#' in hour.minuts units and insolation in hours units and so can be consider that
#' sunshine hours is equal to insolation. Based on "C3S-DRS guideline" variable take name of
#' sunshine hours(ss) and units is hours(h), meaning that all values that are in hour.minuts will be
#' convert to only hours, making a metadata observation of original values.
#'
#' What this function provide or do?
#' 1. Collect data information and statistic data (means/sum if exist);
#' 2. Sunshine hours test with max hours that can take one place for determinate date;
#' 3. Calculate statistic, month and daily, for any variable.
#' 4. Statistic test, ie. publish stats (mean or sum) with calculated by function with data values 
#' collected to any variable.
#'
#' Formats of file that goes to read:
#'  
#'  "SEF" - Station Exchange Format - v0.2.0 (8 col): 
#'   [Year][Month][Day][Hour][Minute][Period][Value][Meta]
#'   
#'   https://datarescue.climate.copernicus.eu/st_formatting-land-stations
#'   
#'  To read this files a function "read.go()" will do job and will come inside "readFiles.R" file.
#'  This is a external function and can be replace by any other function to read files made by user,
#'  according with followed requirements:
#'    -> function must have name "read.go";
#'    -> for QualityControl function to run must be form from file reader function a data frame with
#'       name "Data" and with columns names as SEF format above;
#'    -> Since SEF files have metadata section on top of file (12 first lines), its need to be sure
#'       those variables are defined in data frame "metadata", according with html page above.
#'        (At least the "opts": Lat, Lon, Alt and Vbl(= variable))
#'   
#'  This file its a txt file were values are separated by "TAB" and the extension its
#'  .tsv (tab separated values) or .txt if "TAB" separated values used.
#'  Also contains metadata in first 12 lines necessary to run this function.
#'
#' Statistic files:
#' In case of statistic test (mean or sum), file with data published should be of type txt 
#' and at least with follow columns: [Year][Month][Hour][Minute][<stat name>] or
#' [Year][Month][<stat name>].
#' 
#' [<stat name>] column takes only "Mean" or "Sum" names. If name is "Value" it will be
#' ask to identify type of stat to compare, "Mean" or "Sum".
#' 
#' [Hour][Minute] columns should apear if month stats exist for more than 1 hour.
#' 
#' exemples: [Year][Month][Hour][Minute][Mean] or [Year][Month][Sum] 
#'
#'    
#' Variables need to run this and other functions:
#'
#' lat           latitude in degrees of station   
#' long          longitude in degrees of station  
#' alt           altitude of station              
#' 
#' In case of no lat and long coordenates in degrees exist inside data file, is possible to get it
#' if degree,minuts,seconds and direction coordenates values exist. For this first must run follow
#' function in order to obtain latitude and longitude.
#'
#' lat.calc()
#' long.calc()
#'
#'
#' @Dependency of foward function:
#'
#' read.go()
#'   Reads metadata and data from files.
#'
#' read.stat()
#'   Reads published stats files for statistic test (if exists and apply).
#'
#' day.duration(iyear,imonth,iday)                
#'   arguments: iyear, imonth, iday;
#'   variables: latitude, longitude, altitude;
#'   calculates max solar hours day can take for a certain day, month, year and place.
#'
#' stats.go(obj=NA,type="full",period="month",printStats="FALSE") 
#'   arguments: obj, type, period, printStats;
#'   Calculates some stats values (monthly and daily) for data series with name "Data".   
#'
#' @Output files
#' 
#' "qc" add to fileRead                with table and metadata from QC test function.
#' log.txt                             run log of function.
#' stats.txt or <vname>_stats.txt      With stats values (if choosed to).
#'
#' @Exemples 
#' 
#' 1) qc.test("test.csv",TRUE) or qc.test("test.csv",T)
#'      fileRead = "test.csv"
#'      meanchk  = TRUE
#'      vname    = NA (default)
#'
#' 2) qc.test(meanchk=T)
#'      fileRead = (will open a window to choose)
#'      meanchk  = TRUE
#'      vname    = NA  (default)
#'
#' 3) qc.test(,T,"ss")
#'      fileRead = (will open a window to choose)
#'      meanchk  = TRUE
#'      vname    = "ss"
#'
#' 4) qc.test()
#'      fileRead = (will open a window to choose)
#'      meanchk  = FALSE (default)
#'      vname    = NA  (default)
#'
#' If user only want to specify one argument, have to write name of argument like exemple 2) above.
#' If user don't write name of argument like exemple 1), then its extremely necessary to keep the
#' order of each one. 
#' 
#' @version
#'  2.0
#'
#' @Author
#'  Pedro Gomes
#'
