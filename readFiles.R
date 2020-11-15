#' read.go(), read.stat()
#' 
#' Read from SEF 0.2.0 files and published statistic files.
#' 
#' @description 
#' 
#' This functions will read data tables from
#'  SEF 0.2.0 files  -> read.go()
#'  Statistic files  -> read.stat()
#' 
#' @Usage
#'
#' read.go()
#' read.stat()
#'
#' @Details
#'
#' "read.go"
#' This function will read data and metadata existed in SEF 0.2.0 file format. 
#' The file its tab formatted values (.tsv) and will form 2 data frames variables named 
#' "Data" and "metadata". 
#' 
#' "read.stat"
#' This function will read stats files, if they exist or statistic test is enabled. This data
#' is delivered in data frame variable "Data_stat".
#' 
#' @Author: Pedro Gomes
#' 
read.go <- function(){
  
  if(!exists("fileRead")){
    fileRead <- file.choose()
  }
  
  # SEF file -> first 12 lines metadata
  writeLines("-> Reading metadata")
  cat("-> Reading metadata", file = "log.txt", fill = TRUE, append = TRUE)
  metadata <- read.table(fileRead, header= FALSE, stringsAsFactors = FALSE, sep = "\t", quote = "", nrows = 12)
  names(metadata) <- c("opts","val")
  assign("metadata",metadata,envir = globalenv())
  # SEF file -> data values
  writeLines("-> Reading table files")
  cat("-> Reading table files", file = "log.txt", fill = TRUE, append = TRUE)
  Data <- read.table(fileRead, header = TRUE, stringsAsFactors = FALSE, sep = "\t", quote = "", skip = 12)
  assign("Data", Data, envir = globalenv())  
}

read.stat <- function(){
  
  if(!exists("fileStat")){
    fileStat <- file.choose()
  }
  
  writeLines("-> Reading published stats")
  cat("-> Reading published stats", file = "log.txt", fill = TRUE, append = TRUE)
  Data_stat <- read.table(fileStat, header = TRUE, stringsAsFactors = FALSE, sep = "\t", quote = "")
  assign("Data_stat", Data_stat, envir = globalenv()) 
}