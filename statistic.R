#' stats.go {QC}
#'
#' Statistic Calculation
#'
#' @Description
#'
#' Function that calculates basic statistic of a dataset.
#'  
#' @Usage  
#'  
#' stats.go(), stats.go("mean") or stats.go(type="min") or stats.go(type="min",period="month")
#'  
#' @Arguments
#' 
#' obj           object (matrix or data frame) to calculate stats. 
#'  
#' type          character value, specify type of statistic user want calculate, see details for list.
#'               Default = "full".
#'               
#' period        character value, say time period to calculate stats, "month" or "daily" if data
#'               its sub-daily (have several hours). Default = "month".
#'
#' printStats    logical, to print stats to a file. Default = FALSE.               
#'
#' @Details
#'
#' This function will calculate some basic statistic and give it in form of data.frame.
#' Names for data frame are: "Data" for test values, "Data_c" for monthly or daily stats.
#' There chance of designate object target of this function, with argument "obj". If not, it will
#' search if exist in global environment with name "Data". Remember is name that this function use
#' to store test values. 
#' 
#' List of stats that calculates:
#' 
#' sum
#' nobs              
#' mean      or med      
#' max       or maximum   
#' min       or minimum
#' median
#' sd        or standart deviation
#' var       or variance
#' amp       or amplitude
#'
#' In argument "type" can be writed all name or short according with list above.
#' 
#' This function uses the name 'Data' for dataset table and MUST HAVE HEADER with at least
#' four columns with names [Year][Month][Day][Value]. Optional can have [Hour][Minute] if sub-daily
#' dataset, but this function doesnt distinguish between daily hours.
#'
#' In case of sub-daily datasets, make sure "Data" contain only one hour information each time for
#' month stats. Exemple: Data_s <- Data    (to save all info)
#'                       Data <- Data_s[Data_s$Hour == 12, ]
#'
#' @examples
#'  
#' stats.go(obj="Data","maximum")      gives monthly maximum values of dataset with name "Data"
#' stats.go("max")                     gives monthly maximum values of dataset
#' stats.go("variance")                gives monthly variance of dataset
#' stats.go("sum")                     gives monthly sum of dataset
#' stats.go("max","daily")             gives daily maximum values of dataset
#' stats.go(type="amp",period="daily") gives daily amplitude values of dataset
#' stats.go("full","month")            gives all monthly stats of dataset "Data".
#'
#' @Author: Pedro Gomes
#'
stats.go <- function(obj,type="full",period="month",printStats=FALSE) {

if(length(obj) <= 1 ){
  writeLines("Data table with values doesnt exist...")
} else {
  Data <- obj
}

numYears <- unique(Data$Year)
assign("numYears",numYears,envir = globalenv())
cat("Number of Years:", length(numYears),"->",numYears, sep = " ", fill = TRUE)
cat("Number of Years:", length(numYears),"->",numYears, file = "log.txt", sep = " ", fill = TRUE, append = TRUE)

s_year   <- c()
s_month  <- c()
s_day    <- c()
s_obs    <- c()
s_sum    <- c()
s_mean   <- c()
s_max    <- c()
s_min    <- c()
s_median <- c()
s_sd     <- c()
s_var    <- c()
s_amp    <- c()
if("Hour" %in% colnames(Data)) {
  s_hour <- Data$Hour[1]
} else {
  s_hour <- 99
}
if("Minute" %in% colnames(Data)) {
  s_minu <- Data$Minute[1]
} else {
  s_minu <- 99
}

k <- 1
for(ii in 1:length(numYears)) {  
  for(jj in 1:12) {
    if(period == "daily") {
      nb <- c(31,28,31,30,31,30,31,31,30,31,30,31)
      if(numYears[ii]%%4 == 0) {
        nb[2] <- 29
      } else {
        nb[2] <- 28
      }
      for(ll in 1:nb[ii]) {
        Data_sub <- Data[Data$Year == numYears[ii] & Data$Month == jj & Data$Day == ll, ]
        if(length(Data_sub$Value) == 0) {
          cat(numYears[ii],".",jj,".",ll," dont exist...",sep = "", fill = TRUE)
          cat(numYears[ii],".",jj,".",ll," dont exist...",file = "log.txt",sep = "", fill = TRUE, append = TRUE)
        } else {
          # Number Observations
          s_obs[k] <- length(Data_sub$Value)
          # Sum
          s_sum[k] <- round(sum(Data_sub$Value, na.rm = TRUE),digits = 1) 
          # Mean      
          s_mean[k] <- round(mean(Data_sub$Value, na.rm = TRUE),digits = 2) 
          # Max 
          s_max[k] <- round(max(Data_sub$Value, na.rm = TRUE),digits = 1) 
          # Min 
          s_min[k] <- round(min(Data_sub$Value, na.rm = TRUE),digits = 1) 
          # Median 
          s_median[k] <- round(median(Data_sub$Value, na.rm = TRUE),digits = 2) 
          # Standart Deviation 
          s_sd[k] <- round(sd(Data_sub$Value, na.rm = TRUE),digits = 1) 
          # variance 
          s_var[k] <- round(var(Data_sub$Value, na.rm = TRUE),digits = 1) 
          # Amplitude 
          s_amp[k] <- round(s_max[k]-s_min[k],digits = 1)
          s_year[k] <- numYears[ii]
          s_month[k] <- jj
          s_day[k] <- ll
          k <- k+1
        }
      }
    } else {
      Data_sub <- Data[Data$Year == numYears[ii] & Data$Month == jj, ]
      if(length(Data_sub$Value) == 0) {
        cat(numYears[ii],".",jj," dont exist...",sep ="",fill = TRUE)
        cat(numYears[ii],".",jj," dont exist...",file = "log.txt",sep = "", fill = TRUE, append = TRUE)
      } else {
        # Number Observations
        s_obs[k] <- length(Data_sub$Value)
        # Sum
        s_sum[k] <- round(sum(Data_sub$Value, na.rm = TRUE),digits = 1) 
        # Mean      
        s_mean[k] <- round(mean(Data_sub$Value, na.rm = TRUE),digits = 2) 
        # Max 
        s_max[k] <- round(max(Data_sub$Value, na.rm = TRUE),digits = 1) 
        # Min 
        s_min[k] <- round(min(Data_sub$Value, na.rm = TRUE),digits = 1) 
        # Median 
        s_median[k] <- round(median(Data_sub$Value, na.rm = TRUE),digits = 2) 
        # Standart Deviation 
        s_sd[k] <- round(sd(Data_sub$Value, na.rm = TRUE),digits = 1) 
        # variance 
        s_var[k] <- round(var(Data_sub$Value, na.rm = TRUE),digits = 1) 
        # Amplitude 
        s_amp[k] <- round(s_max[k]-s_min[k],digits = 1)
        s_year[k] <- numYears[ii]
        s_month[k] <- jj
        k <- k+1
      }    
    }  
  }
}
if(type == "full") {
  if(period == "daily"){
    stat_name <- c("Year","Month","Day","Obs","Sum","Mean","Max","Min","Median","Sd","Var","Amp")
    Data_c <- data.frame(s_year,s_month,s_day,s_obs,s_sum,s_mean,s_max,s_min,s_median,s_sd,s_var,s_amp, stringsAsFactors=FALSE)  
  } else {
    stat_name <- c("Year","Month","Hour","Minute","Obs","Sum","Mean","Max","Min","Median","Sd","Var","Amp")  
    Data_c <- data.frame(s_year,s_month,s_hour,s_minu,s_obs,s_sum,s_mean,s_max,s_min,s_median,s_sd,s_var,s_amp, stringsAsFactors=FALSE)  
    
  }
} else {
  if(period == "daily"){
    if(type == "sum") {
      Data_c <- data.frame(s_year,s_month,s_day,s_obs,s_sum, stringsAsFactors=FALSE)   
      stat_name <- c("Year","Month","Day","Obs","Sum")
    } else if(type == "mean" | type == "med") {
      Data_c <- data.frame(s_year,s_month,s_day,s_obs,s_mean, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Day","Obs","Mean")
    } else if(type == "maximum" | type == "max") {
      Data_c <- data.frame(s_year,s_month,s_day,s_obs,s_max, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Day","Obs","Max")
    } else if(type == "minimum" | type == "min") {
      Data_c <- data.frame(s_year,s_month,s_day,s_obs,s_min, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Day","Obs","Min")
    } else if(type == "median") {
      Data_c <- data.frame(s_year,s_month,s_day,s_obs,s_median, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Day","Obs","Median")
    } else if(type == "sd" | type == "standart deviation") {
      Data_c <- data.frame(s_year,s_month,s_day,s_obs,s_sd, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Day","Obs","Sd")
    } else if(type == "variance" | type == "var") {
      Data_c <- data.frame(s_year,s_month,s_day,s_obs,s_var, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Day","Obs","Var")
    } else if(type == "amplitude" | type == "amp") {
      Data_c <- data.frame(s_year,s_month,s_day,s_obs,s_amp, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Day","Obs","Amp")
    }    
  } else {
    if(type == "sum") {
      Data_c <- data.frame(s_year,s_month,s_hour,s_minu,s_obs,s_sum, stringsAsFactors=FALSE)   
      stat_name <- c("Year","Month","Hour","Minute","Obs","Sum")
    } else if(type == "mean" | type == "med") {
      Data_c <- data.frame(s_year,s_month,s_hour,s_minu,s_obs,s_mean, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Hour","Minute","Obs","Mean")
    } else if(type == "maximum" | type == "max") {
      Data_c <- data.frame(s_year,s_month,s_hour,s_minu,s_obs,s_max, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Hour","Minute","Obs","Max")
    } else if(type == "minimum" | type == "min") {
      Data_c <- data.frame(s_year,s_month,s_hour,s_minu,s_obs,s_min, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Hour","Minute","Obs","Min")
    } else if(type == "median") {
      Data_calc <- data.frame(s_year,s_month,s_hour,s_minu,s_obs,s_median, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Hour","Minute","Obs","Median")
    } else if(type == "sd" | type == "standart deviation") {
      Data_c <- data.frame(s_year,s_month,s_hour,s_minu,s_obs,s_sd, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Hour","Minute","Obs","Sd")
    } else if(type == "variance" | type == "var") {
      Data_c <- data.frame(s_year,s_month,s_hour,s_minu,s_obs,s_var, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Hour","Minute","Obs","Var")
    } else if(type == "amplitude" | type == "amp") {
      Data_c <- data.frame(s_year,s_month,s_hour,s_minu,s_obs,s_amp, stringsAsFactors=FALSE)    
      stat_name <- c("Year","Month","Hour","Minute","Obs","Amp")
    }    
  }
}
names(Data_c) <- stat_name
assign("Data_c",Data_c,envir = globalenv())
if(printStats) {
  if(period == "month"){
    write.table (Data_c, file = "stats_month.txt", sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
  } else {
    write.table (Data_c, file = "stats_daily.txt", sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
  }
}
return(Data_c)
}
#End function