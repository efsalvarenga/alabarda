guessDateFormat <- function(x, returnDates = FALSE, tzone = "") {
  x1 <- x
  # replace blanks with NA and remove
  # x1[x1 == ""] <- NA # [efsa]
  x1 <- x1[!is.na(x1)]
  if (length(x1) == 0)
    return(NA)
  # if it's already a time variable, set it to character
  if ("POSIXt" %in% class(x1[1])) {
    x1 <- as.character(x1)
  }
  dateTimes <- do.call(rbind, strsplit(x1, " "))
  for (i in ncol(dateTimes)) {
    dateTimes[dateTimes[, i] == "NA"] <- NA
  }
  # assume the time part can be found with a colon
  timePart <- which(apply(dateTimes, MARGIN = 2, FUN = function(i) {
    any(grepl(":", i))
  }))
  # everything not in the timePart should be in the datePart
  datePart <- setdiff(seq(ncol(dateTimes)), timePart)
  # should have 0 or 1 timeParts and exactly one dateParts
  if (length(timePart) > 1 || length(datePart) != 1)
    stop("cannot parse your time variable")
  timeFormat <- NA
  if (length(timePart)) {
    # find maximum number of colons in the timePart column
    ncolons <- max(nchar(gsub("[^:]", "", na.omit(dateTimes[, timePart]))))
    if (ncolons == 1) {
      timeFormat <- "%H:%M"
    } else if (ncolons == 2) {
      timeFormat <- "%H:%M:%S"
    } else stop("timePart should have 1 or 2 colons")
  }
  # remove all non-numeric values
  dates <- gsub("[^0-9]", "", na.omit(dateTimes[, datePart]))
  # sep is any non-numeric value found, hopefully / or -
  sep <- unique(na.omit(substr(gsub("[0-9]", "", dateTimes[, datePart]), 1, 1)))
  if (length(sep) > 1)
    stop("too many seperators in datePart")
  # maximum number of characters found in the date part
  dlen <- max(nchar(dates))
  dateFormat <- NA
  # when six, expect the century to be omitted
  if (dlen == 6) {
    if (sum(is.na(as.Date(dates, format = "%y%m%d"))) == 0) {
      dateFormat <- paste("%y", "%m", "%d", sep = sep)
    } else if (sum(is.na(as.Date(dates, format = "%m%d%y"))) == 0) {
      dateFormat <- paste("%m", "%d", "%y", sep = sep)
    } else stop("datePart format [six characters] is inconsistent")
  } else if (dlen == 8) {
    if (sum(is.na(as.Date(dates, format = "%Y%m%d"))) == 0) {
      dateFormat <- paste("%Y", "%m", "%d", sep = sep)
    } else if (sum(is.na(as.Date(dates, format = "%m%d%Y"))) == 0) {
      dateFormat <- paste("%m", "%d", "%Y", sep = sep)
    } else stop("datePart format [eight characters] is inconsistent")
  } else {
    stop(sprintf("datePart has unusual length: %s", dlen))
  }
  if (is.na(timeFormat)) {
    format <- dateFormat
  } else if (timePart == 1) {
    format <- paste(timeFormat, dateFormat)
  } else if (timePart == 2) {
    format <- paste(dateFormat, timeFormat)
  } else stop("cannot parse your time variable")
  if (returnDates)
    return(as.POSIXlt(x, format = format, tz = tzone))
  format
}

# I copied from here: http://biostat.mc.vanderbilt.edu/wiki/pub/Main/ColeBeck/datestimes.pdf


