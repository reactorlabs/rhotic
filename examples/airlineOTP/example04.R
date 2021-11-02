require("testthat")
require("assertthat")
setwd("data")
file <- "101.csv"

indicesForNames <- function(df, nm) sort( which( names(df) %in% nm ) )

# Simplified version from example03: just select the columns we want
# (1) data must have these columns
columnTrim <- function(df) {
  fnames <- c("YEAR", "QUARTER", "MONTH", "DAY_OF_MONTH", "DAY_OF_WEEK", "CARRIER",
              "TAIL_NUM", "FL_NUM", "ORIGIN", "DEST", "CRS_DEP_TIME", "DEP_TIME",
              "DEP_DELAY", "DEP_DEL15", "DEP_DELAY_GROUP", "TAXI_OUT", "WHEELS_OFF",
              "WHEELS_ON", "TAXI_IN", "CRS_ARR_TIME", "ARR_TIME", "ARR_DELAY", "ARR_DEL15",
              "ARR_DELAY_GROUP", "CANCELLED", "CANCELLATION_CODE", "CRS_ELAPSED_TIME",
              "ACTUAL_ELAPSED_TIME", "AIR_TIME", "DISTANCE", "DISTANCE_GROUP",
              "CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "SECURITY_DELAY",
              "LATE_AIRCRAFT_DELAY", "AVG_TICKET_PRICE")
  df <- subset(df, select = fnames)

  # This is a postcondition, after we have mutated the data, so it shouldn't be a constraint
  assert_that(length(df) == 37)
  df
}

# Tracing through this function will be tricky
retype <- function (values, desired) {
  actual <- class(values)
  if (desired == actual) {
    result <- values
  } else if (desired == "integer") {
    if (actual == "numeric" || actual == "factor" || actual == "character")
      result <- as.integer(values)
  } else if (desired == "logical") {
    if (actual == "factor" || actual == "integer")
      result <- as.logical(values)
  } else if (desired == "factor") {
    if (actual == "integer") result <- as.factor(values)
  }
  if (!exists("result")) stop("Could not make ", actual," into ",desired)
  result
}

# Tracing through this will be tricky
# This function basically mutates the data and doesn't impose constraints
# It asserts that a column is a factor, but that was because of a mutation
# One possible "constraint" is that spaces should be interpreted as NA
spaceToNA <- function(df, name) {
  assert_that(is.atomic(name))
  idx <- indicesForNames(df,name)
  assert_that(is.factor(df[,idx]))
  df[df[,idx]=="",idx]<-NA
  df[,idx]<-droplevels(df[,idx])
  df
}

# data has the columns (1) CANCELLATION_CODE and (2) TAIL_NUM
# (3) both of those columns should be interpreted as factors
# (4) and spaces in those columns should be interpreted as NA
# data also has the columns (5) CANCELLED and (6) FL_NUM
# (7) CANCELLED is a factor/int that is interpreted as logical
# (8) FL_NUM is (an integer, according to the retype function, that is) interpreted as a factor
retypeMisc <- function(f) {
  f$CANCELLATION_CODE <- as.factor(f$CANCELLATION_CODE)
  f$TAIL_NUM <- as.factor(f$TAIL_NUM)
  f <- spaceToNA(f,"CANCELLATION_CODE")
  f <- spaceToNA(f,"TAIL_NUM")
  f$CANCELLED <- retype(f$CANCELLED, "logical")
  f$FL_NUM <- retype(f$FL_NUM,"factor")
  f
}

# (9) more columns that are required in the data
# (10) furthermore, they should be ints
retypeInt <- function(f) {
  # the following should be integer columns
  integer_cols <- c("YEAR","QUARTER","MONTH","DAY_OF_MONTH","DAY_OF_WEEK","CRS_DEP_TIME","DEP_TIME","DEP_DELAY","DEP_DEL15","DEP_DELAY_GROUP",
                    "TAXI_OUT","WHEELS_OFF", "WHEELS_ON","TAXI_IN","CRS_ARR_TIME","ARR_TIME","ARR_DELAY","ARR_DEL15","ARR_DELAY_GROUP",
                    "CRS_ELAPSED_TIME","ACTUAL_ELAPSED_TIME","AIR_TIME","DISTANCE","DISTANCE_GROUP","CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY",
                    "SECURITY_DELAY","LATE_AIRCRAFT_DELAY")
  idx <- indicesForNames(f, integer_cols)
  assert_that(length(integer_cols)==length(idx))
  f[,idx] <- apply(f[,idx],2, function(x) retype(x,"integer"))
  f
}

clean <- function(f) {
  f <- columnTrim(f)
  f <- retypeMisc(f)
  f <- retypeInt(f)
  f
}

# For this example, assume the data is valid; read in the CSV and its header,
# and only take the first 100 rows and 110 columns
df <- read.csv(file, nrows=100, header=TRUE, check.names=FALSE)[, 1:110]
df <- clean(df)

head(df)
