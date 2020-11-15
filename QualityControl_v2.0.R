#' qc.test {QualityControl}
#' 
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
qc.test<-function(fileRead = "NA", meanchk = FALSE, vname = NA) {

writeLines("-> Checking functions, files and arguments")
cat("-> Checking functions, files and arguments",file = "log.txt",fill = TRUE,append = TRUE)  
# Assign arguments and variables 
assign("meanchk",meanchk,envir = globalenv())
assign("vname",vname,envir = globalenv())
assign("fileRead",fileRead,envir = globalenv()) 
assign("data_error",-99.9,envir = globalenv())  

# Input files paths
if(fileRead == "NA") {
  writeLines("Adquiring data file...")
  fileRead <- file.choose()
} 
assign("fileRead",fileRead,envir = globalenv())
if(meanchk) {
  writeLines("Adquiring stats file...")
  fileStat <- file.choose()
  assign("fileStat",fileStat,envir = globalenv())
}

# Librarys required
library("stats")

# Files and functions
if(file.exists("log.txt")) {
  writeLines("Log file already exist.")
  choice <- askYesNo("Delete log file?", default = FALSE)
  if(choice) {
    file.remove("log.txt")
    cat(date(), file="log.txt", sep = " ", fill = TRUE)
  } else {
    cat("--------------->  <---------------", file="log.txt", sep = " ", fill = TRUE)
    cat(date(), file="log.txt", sep = " ", fill = TRUE, append = TRUE)
  }
} 

# write to log file path to files
cat("Table file path:",fileRead, file="log.txt", sep = " ", fill = TRUE, append = TRUE )
if(meanchk) {
  cat("Mean file path:",fileStat, file="log.txt", sep = " ", fill = TRUE, append = TRUE ) 
}

# External functions
if(file.exists("statistic.R")) {
  source("statistic.R")  
} else {
  writeLines("Couldn't find the file statistic.R, statistic test will not work.")
  cat("Couldn't find the file statistic.R, statistic test will not work.", file="log.txt", fill = TRUE, append = TRUE )
}

if(file.exists("sunshine.R")) {
  source("sunshine.R")  
} else {
  writeLines("Couldn't find the file sunshine.R, in case vname is 'ss' sunshine test will not work.")
  cat("Couldn't find the file sunshine.R, in case vname is 'ss' sunshine test will not work.", file="log.txt", fill = TRUE, append = TRUE )
}

if(file.exists("readFiles.R")){
  source("readFiles.R")  
} else {
  if(existsFunction("read.go")) {
    writeLines("Function to read files present....")
  } else { 
    writeLines("Couldn't find the function read.go, program will end.")
    cat("Couldn't find the function read.go, program will end.", file="log.txt", fill = TRUE, append = TRUE )
    stop()
  }
  if(meanchk){
    if(existsFunction("read.stat")) {
      writeLines("Function to read statistic files present....")
    } else {
      writeLines("Couldn't find the function read.stat, statistic test disabled.")
      cat("Couldn't find the function read.stat, statistic test disabled.", file="log.txt", fill = TRUE, append = TRUE )
      meanchk <- FALSE      
    }
  }
}



# Reading files & Metadata
read.go()

# Defining data frame "metadata"
if(!exists("metadata")){
  opts <- c("SEF","ID","Name","Source","Lat","Lon","Alt","Link","Vbl","Stat","Units","Meta")
  val  <- c(NA)
  metadata <- data.frame(opts,val,stringsAsFactors = FALSE)
  assign("metadata",metadata,envir = globalenv())  
}

if(meanchk){
read.stat() 
}

# Mean header
if(meanchk) {
  data_name_stat <- colnames(Data_stat)
  cat("Number of columns of stats file: ",ncol(Data_stat),fill = TRUE,sep = " ")
  if(ncol(Data_stat) < 3) {
    writeLines("Insuficient number of columns.")
    cat("Insuficient number of columns.",file = "log.txt",fill = TRUE,append = TRUE)
    meanchk <- FALSE
  } else {
    if("Value" %in% data_name_stat){
      choice <- menu(c("Mean","Sum"), graphics = TRUE, title = "Stat to compare?")
      if(choice == "1"){
        data_name_stat[data_name_stat == "Value"] <- "Mean"       
      } else {
        data_name_stat[data_name_stat == "Value"] <- "Sum"
      }
      assign("data_name_stat",data_name_stat,envir = globalenv())
      names(Data_stat) <- data_name_stat
      assign("Data_stat", Data_stat, envir = globalenv())
    }
  }
}

# Check data error value
writeLines("-> Check data error value...")
cat("-> Check data error value...", file = "log.txt",fill = TRUE,append = TRUE)
choice <- askYesNo("Data error or missing value is -99.9, choose another?", default = FALSE)
if(choice) {
  data_error <- readline("Introduce data error value:  ")
  if(nchar(data_error) == 0) {
    writeLines("No data error defined, default in place!!")
    cat("No data error defined, default in place!!", file="log.txt", fill=TRUE, append = TRUE)
    data_error <- -99.9
  } 
}
Data$Value[Data$Value == data_error] <- NA
if(meanchk) {
  if("Mean" %in% data_name_stat){
    Data_stat$Mean[Data_stat$Mean == data_error] <- NA   
  } 
  if("Sum" %in% data_name_stat){
    Data_stat$Sum[Data_stat$Sum == data_error] <- NA    
  }
}
assign("Data", Data, envir = globalenv())
# Latitude, Longitude and Altitude definitions.
writeLines("-> Latitude, Longitude and Altitude definitions")
cat("-> Latitude, Longitude and Altitude definitions", file = "log.txt",fill = TRUE,append = TRUE)
lat  <- as.numeric(metadata$val[metadata$opts == "Lat"])
long <- as.numeric(metadata$val[metadata$opts == "Lon"])
alt  <- as.numeric(metadata$val[metadata$opts == "Alt"])
assign("lat",lat,envir = globalenv())
assign("long",long,envir = globalenv())
assign("alt",alt,envir = globalenv())
# Check values of lat, long and alt
if(lat < -90 | lat > 90) {
  writeLines("Latitude may not be correct!!")
  cat("Latitude may not be correct!!",file="log.txt",append = TRUE)
}
if(long < -180 | long > 180) {
  writeLines("Longitude may not be correct!!")
  cat("Longitude may not be correct!!",file="log.txt",append = TRUE)
}
if(alt < 0.0) {
  writeLines("Incorrect altitude, default in place!!")
  cat("Incorrect altitude, default in place!!",file="log.txt",append = TRUE)
  alt <- 0.0
}

# Check if vname exists and is not NA
writeLines("-> Check if vname exists and is not NA")
cat("-> Check if vname exists and is not NA", file = "log.txt",fill = TRUE,append = TRUE)
if(is.na(vname)) {
  writeLines("vname is on default...checking SEF file....")
  cat("vname is on default...checking SEF file....", file = "log.txt",fill = TRUE,append = TRUE)
  vname = metadata$val[metadata$opts == "Vbl"]
  if(is.na(vname)) {
    writeLines("Couldnt define vname, specific tests will not be apply. Please use argument vname.")
    cat("Couldnt define vname, specific tests will not be apply. Please use argument vname.", file = "log.txt",fill = TRUE,append = TRUE)
    vname <- ""
  }
} else {
  if(vname == metadata$val[metadata$opts == "Vbl"]) {
    writeLines("Variable name correspond to exist in SEF file.")
  } else {
    writeLines("Variable name in SEF file doesnt match vname. SEF file variable name adopted.")
    cat("Variable name in SEF file doesnt match vname. SEF file variable name adopted.", file = "log.txt",fill = TRUE,append = TRUE)
    vname <- metadata$val[metadata$opts == "Vbl"]
  }
}
cat("Variable Name: ",vname,sep = " ",fill = TRUE)
cat("Variable Name: ",vname,file = "log.txt",sep = " ",fill = TRUE,append = TRUE)
assign("vname",vname,envir = globalenv())

# Check data length / number of hours and years / metadata variable
writeLines("-> Check data length / number of hours and years / metadata variable")
cat("-> Check data length / number of hours and years / metadata variable", file = "log.txt",fill = TRUE,append = TRUE)

# data length
data_len <- length(Data$Value)
assign("data_len",data_len,envir = globalenv())

# number of hours 
if("Hour" %in% colnames(Data)) {
  Hours <- unique(Data$Hour)
  assign("Hours",Hours,envir = globalenv())
  cat("Number of Hours:", length(Hours),"->",Hours, sep = " ", fill = TRUE)
  cat("Number of Hours:", length(Hours),"->",Hours, file = "log.txt", sep = " ", fill = TRUE, append = TRUE)
}
if(length(Hours) > 1 & meanchk == TRUE) {
  if("Hour" %in% colnames(Data_stat)) {
  } else {
    writeLines("Warning: Stats comparation test will be disabled.
               Check if stats file have [Hour] column present.")
    cat("Warning: Stats comparation test will be disabled.
               Check if stats file have [Hour] column present.",
        file = "log.txt",fill = TRUE,append = TRUE)
    meanchk <- FALSE
    assign("meanchk",meanchk,envir = globalenv())
  }
}

# number of years 
numYears <- unique(Data$Year)
assign("numYears",numYears,envir = globalenv())
cat("Number of Years:", length(numYears),"->",numYears, sep = " ", fill = TRUE)
cat("Number of Years:", length(numYears),"->",numYears, file = "log.txt", sep = " ", fill = TRUE, append = TRUE)

# metadata variable
Meta <- c()
for (i in 1:data_len){Meta[i] <- NA}

# Calculation and Tests
writeLines("-> Calculations and Tests")
cat("-> Calculation and Tests", file = "log.txt",fill = TRUE,append = TRUE)

# Statistic Calculation
writeLines("Statistic Calculation")
cat("Statistic Calculation",file = "log.txt", fill = TRUE, append = TRUE)
if(length(Hours)>1) {
  DataOri <- Data
  if(exists("Data_calc")) {
    rm(Data_calc)    
  }
  for(i in 1:length(Hours)) {
    Data <- DataOri[DataOri$Hour == Hours[i], ]
    assign("Data",Data,envir = globalenv())
    cat("running stats for Hour:",Hours[i], sep = " ",fill = TRUE)
    cat("running stats for Hour:",Hours[i], file = "log.txt",sep = " ",fill = TRUE,append = TRUE)
    if(exists("Data_calc")) {
      data_old <- Data_calc
      stats.go(type="full")
      Data_calc <- rbind(data_old,Data_calc,stringsAsFactors = FALSE)
      assign("Data_calc",Data_calc,envir = globalenv())
    } else {
      stats.go(type="full")
    }
  }
  Data <- DataOri
  assign("Data",Data,envir = globalenv())
} else {
  cat("running stats subroutine",sep = " ",fill = TRUE)
  cat("running stats subroutine",file = "log.txt",sep = " ",fill = TRUE,append = TRUE)
  stats.go(type="full") 
}

# Max Sunshine test
if(vname == "ss") {
  writeLines("Testing max sunshine")
  cat("Testing max sunshine",file = "log.txt",fill = TRUE,append = TRUE)
  max_day <- c()
  for(i in 1:data_len) {
    max_day[i] <- day.duration(Data$Year[i],Data$Month[i],Data$Day[i])
    max_day[i] <- round(max_day[i],digits = 1)
    if(is.na(Data$Value[i])) {
      cat("Error value found in ",Data$Year[i],"/",Data$Month[i],"/",Data$Day[i], fill = TRUE)      
    } else {
      if(Data$Value[i] > max_day[i]+0.1) {
        cat(Data$Year[i],"/",Data$Month[i],"/",Data$Day[i]," <- Flag raised: max_sunshine", sep = " ", fill = TRUE)
        cat(Data$Value[i],"= Obs > Max =",max_day[i], sep = " ", fill = TRUE)
        cat(Data$Year[i],"/",Data$Month[i],"/",Data$Day[i]," <- Flag raised: max_sunshine",file = "log.txt",sep = " ", fill = TRUE,append = TRUE)
        cat(Data$Value[i],"= Obs > Max =",max_day[i],file = "log.txt",sep = " ", fill = TRUE,append = TRUE)
        if(is.na(Meta[i])) {
          Meta[i] <- "max"
        } else {
          Meta[i] <- paste(Meta[i],"max", sep = ",")
        }
      }
    }
  }
  assign("max_day", max_day, envir = globalenv())
}

# Comparation with mean or sum values
if(meanchk) {
  writeLines("Statistic Testing")
  cat("Statistic Testing",file = "log.txt",fill = TRUE,append = TRUE)
  for(i in 1:length(numYears)) {
    for(j in 1:12) {
      if(length(Hours)<=1) {
        if("Sum" %in% colnames(Data_stat)) {
          data_val_1 <- Data_calc$Sum[Data_calc$Year == numYears[i] & Data_calc$Month == j] 
          data_val_2 <- Data_stat$Sum[Data_stat$Year == numYears[i] & Data_stat$Month == j]            
        }
        if("Mean" %in% colnames(Data_stat)) {
          data_val_1 <- Data_calc$Mean[Data_calc$Year == numYears[i] & Data_calc$Month == j]
          data_val_2 <- Data_stat$Mean[Data_stat$Year == numYears[i] & Data_stat$Month == j]            
        }
        dif_val <- round(abs(data_val_2 - data_val_1),digits = 2)
        if(is.na(dif_val)) {
          cat("Year:",numYears[i],"Month:",j,"does not exist.", sep = " ", fill = TRUE)
          cat("Year:",numYears[i],"Month:",j,"does not exist.", file = "log.txt", sep = " ", fill = TRUE, append = TRUE)
        } else {
          if (dif_val >= 0.15){
            cat("Year:",numYears[i],"Month:",j,"<- Flag raised: mean_sunshine", sep = " ", fill = TRUE)
            cat("publish    :",data_val_2, sep = " ", fill = TRUE)
            cat("calculated :",data_val_1, sep = " ", fill = TRUE)
            cat("Diference  :",dif_val, sep = " ", fill = TRUE)
            cat("Year:",numYears[i],"Month:",j,"<- Flag raised: mean_sunshine", file = "log.txt", sep = " ", fill = TRUE, append = TRUE)
            cat("publish    :",data_val_2,file = "log.txt", sep = " ", fill = TRUE, append = TRUE)
            cat("calculated :",data_val_1,file = "log.txt", sep = " ", fill = TRUE, append = TRUE)
            cat("Diference  :",dif_val,file = "log.txt", sep = " ", fill = TRUE, append = TRUE)
            for (k in 1:data_len) {
              if (Data$Year[k] == numYears[i] & Data$Month[k] == j) {
                if (is.na(Meta[k])) {
                  Meta[k] <- "Mean"
                } else {
                  Meta[k] <- paste(Meta[k],"Mean", sep = "|")
                }
              }
            }         
          }       
        }
      } else {
        for(k in 1:length(Hours)) {
          if("Sum" %in% colnames(Data_stat)) {
            data_val_1 <- Data_calc$Sum[Data_calc$Year == numYears[i] & Data_calc$Month == j & Data_calc$Hour == Hours[k]]
            data_val_2 <- Data_stat$Sum[Data_stat$Year == numYears[i] & Data_stat$Month == j & Data_stat$Hour == Hours[k]]            
          }           
          if("Mean" %in% colnames(Data_stat)) {
            data_val_1 <- Data_calc$Mean[Data_calc$Year == numYears[i] & Data_calc$Month == j & Data_calc$Hour == Hours[k]]
            data_val_2 <- Data_stat$Mean[Data_stat$Year == numYears[i] & Data_stat$Month == j & Data_stat$Hour == Hours[k]]            
          }
          dif_val <- round(abs(data_val_1 - data_val_2),digits = 2)
          if(is.na(dif_val)) {
            cat("Year:",numYears[i],"Month:",j,"Hour:",Hours[k],"does not exist.", sep = " ", fill = TRUE)
            cat("Year:",numYears[i],"Month:",j,"Hour:",Hours[k],"does not exist.", file = "log.txt", sep = " ", fill = TRUE, append = TRUE)
          } else {
            if (dif_val >= 0.15){
              cat("Year:",numYears[i],"Month:",j,"Hour:",Hours[k],"<- Flag raised: mean_sunshine", sep = " ", fill = TRUE)
              cat("publish    :",data_val_1, sep = " ", fill = TRUE)
              cat("calculated :",Data_val_2, sep = " ", fill = TRUE)
              cat("Diference  :",dif_val, sep = " ", fill = TRUE)
              cat("Year:",numYears[i],"Month:",j,"Hour:",Hours[k],"<- Flag raised: mean_sunshine", file = "log.txt", sep = " ", fill = TRUE, append = TRUE)
              cat("publish    :",data_val_1,file = "log.txt", sep = " ", fill = TRUE, append = TRUE)
              cat("calculated :",Data_val_2,file = "log.txt", sep = " ", fill = TRUE, append = TRUE)
              cat("Diference  :",dif_val,file = "log.txt", sep = " ", fill = TRUE, append = TRUE)
              for (kk in 1:data_len) {
                if (Data$Year[kk] == numYears[i] & Data$Month[kk] == j) {
                  if (is.na(Meta[kk])) {
                    Meta[kk] <- "mean"
                  } else {
                    Meta[kk] <- paste(Meta[kk],"mean", sep = "|")
                  }
                }
              }         
            }
          }
        }
      }      
    }
  }
  if(vname == "ss" | vname == "rrh") {
    cat("Warning: Units for this variable is Hour, confirm if all is correct with values.
        Errors in comparation tests can be because of wrong units.")
  }
}  
  
# Write to files  
writeLines("-> Write to files")
writeLines("Data table")
cat("-> Write to files",file = "log.txt",fill = TRUE,append = TRUE)
cat("Data table",file = "log.txt",fill = TRUE,append = TRUE)
if("Meta" %in% colnames(Data)) {
  writeLines("Meta column present, update new information...")
  for(i in 1:data_len) {
    if(is.na(Data$Meta[i])) {
      Data$Meta[i] <- Meta[i]
    } else {
      Data$Meta[i] <- paste(Data$Meta[i],Meta[i], sep = "|")
    }
  }
} else {
  writeLines("Meta column not present, add information...")
  Data$Meta <- Meta 
  assign("Data", Data, envir = globalenv()) 
}
for(i in 1:data_len) {
  if(is.na(Data$Value[i])) { 
    data_print$Value[i] <- data_error
  } 
  if(is.na(Data$Meta[i])) {
    Data$Meta[i] <- ""
  }
}

fileI    <- substr(fileRead,start = 1,stop = nchar(fileRead)-4)
fileE    <- substr(fileRead,start = nchar(fileRead)-3, stop = nchar(fileRead))
file_out <- paste(fileI,"_qc",fileE,sep = "")

write.table (metadata, file = file_out, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)  
write.table (t(names(Data)), file = file_out, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
write.table (Data, file = file_out, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

# Write to file data stats
choice <- askYesNo("Print stats to file?", default = FALSE)
if(choice) {
  writeLines("Data stats")
  cat("Data stats",file = "log.txt",fill = TRUE,append = TRUE)
  if(vname == "") {
    wfile <- "stats.txt"
  } else {
    wfile <- paste(vname,"_stats",".txt",sep = "")
  }
  write.table (Data_calc, file = wfile, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
}
}
# End function