require("testthat")
require("assertthat")
setwd("data")
file <- "101.csv"

getYear <- function(file) read.csv(file,nrows=1,header=TRUE)$YEAR[1]
getMonth <- function(file) read.csv(file,nrows=1,header=TRUE)$MONTH[1]
leadingZero <- function(x) if(is.na(x)) x else if (x<10) paste("0",x,sep="") else x
getYearMonth <- function(file) paste(getYear(file),leadingZero(getMonth(file)),sep="_")

# getYearMonth calls getYear and getMonth
# getYear and getMonth read a single row from the CSV and require the presence of
# (1) YEAR and (2) MONTH columns
# This sets a constraint on the file that was read (101.csv)
identifier <- getYearMonth(file)

print(identifier)
