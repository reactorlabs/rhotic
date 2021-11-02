require("testthat")
require("assertthat")
setwd("data")
file <- "101.csv"

# There is a conditional constraint here: if the number of columns > 110, then
# we assert that the remaining columns are all NAs, and we remove those columns.
# If the number of columns <= 110 then we don't have to do anything
readAll <- function(file) {
  res <- read.csv(file, skip=1, header=FALSE, check.names=FALSE)
  # Maybe simplify this as extracting columns 1:110 and ignoring the rest
  if (length(res) > 110) {
    assert_that( all( is.na(res[, 111:length(res)])))
    res <- subset(res, select=-(111:length(res)))
  }
  names(res) <- columnNames
  res
}

# We read in a single row, treating it as the header
# Constraints: (1) the input has a header, (2) there are at least 110 columns
columnNames <- names(read.csv(file, nrow=1, header=TRUE))[1:110]
df <- readAll(file)

head(df)
