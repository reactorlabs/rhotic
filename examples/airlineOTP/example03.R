require("testthat")
require("assertthat")
setwd("data")
file <- "101.csv"

indicesForNames <- function(df, nm) sort( which( names(df) %in% nm ) )

columnTrim <- function(df) {
  # This assertion occurs AFTER we deleted columns, so it should not be a constraint
  assert_that(length(df) >= 110)

  # (1) data frame contains column names that contain "DIV", otherwise grep
  # returns an empty vector, which doesn't make sense with subset
  # Not an error, but likely unintended behaviour
  diverted <- grep("DIV",names(df))
  # Syntactic sugar for: df[, -diverted]
  df <- subset(df, select= -diverted)

  # (2) data contains these column names
  # Again, violating this contraint isn't an error, but is likely unintended
  # Difficulty is that it's not obvious what indicesForNames is doing
  fnames <- c("FL_DATE","UNIQUE_CARRIER","AIRLINE_ID","ORIGIN_AIRPORT_ID","ORIGIN_AIRPORT_SEQ_ID","ORIGIN_CITY_MARKET_ID",
              "ORIGIN_CITY_NAME","ORIGIN_STATE_FIPS","ORIGIN_STATE_NM","ORIGIN_WAC","DEST_AIRPORT_ID","DEST_AIRPORT_SEQ_ID",
              "DEST_CITY_MARKET_ID","DEST_CITY_NAME","DEST_STATE_ABR","DEST_STATE_FIPS","DEST_STATE_NM","DEST_WAC", "DEP_TIME_BLK",
              "ARR_TIME_BLK","FLIGHTS","ORIGIN_STATE_ABR","DEP_DELAY_NEW","ARR_DELAY_NEW","FIRST_DEP_TIME","LONGEST_ADD_GTIME","TOTAL_ADD_GTIME")
  df <- subset(df, select = -indicesForNames(df,fnames))

  # This is a postcondition, after we have mutated the data, so it shouldn't be a constraint
  assert_that(length(df) == 37)
  df
}

rowTrim <- function(df){
  # data contains the columns (3) ORIGIN and (4) DEST
  # Here the code explicitly casts to factor, so (5) those columns should be factors,
  # but the original type could be anything
  origins <- levels(as.factor(df$ORIGIN))
  dests <- levels(as.factor(df$DEST))

  # (6) This is another "should" constraint; we're comparing the lengths
  # of the levels of factors, and if they're equal, we return. Otherwise, we
  # enforce this contraint by mutating the data
  if (length(dests) == length(origins)) return (df)

  df <- df[df$DEST %in% origins,]
  df$DEST <- droplevels(as.factor(df$DEST))
  df <- df[!is.na(df[,1]),]
  df
}

clean <- function(f) {
  f <- columnTrim(f)
  f <- rowTrim(f)
  f
}

# For this example, assume the data is valid; read in the CSV and its header,
# and only take the first 110 columns
df <- read.csv(file, header=TRUE, check.names=FALSE)[, 1:110]
df <- clean(df)

head(df)
