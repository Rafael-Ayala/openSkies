checkAirport <- function(airport) {
  if(!is.character(airport) | length(airport) == 0) {
    stop("Please enter a valid airport name.")
  }
  if(length(airport) > 1) {
    stop("Only one airport per query is supported")
  }
}

checkCallSign <- function(callSign) {
  if(!is.character(callSign) | length(callSign) == 0) {
    stop("Please enter a valid call sign")
  }
  if(length(callSign) > 1) {
    stop("Only one call sign per query is supported")
  }
}

checkTime <- function(timeString) {
  tryCatch({
    invisible(as.Date(timeString))
    }, error=function(e) {
    stop("Time must be provided as an unambiguous date(-time) string.")
    })
}

checkICAO24 <- function(aircraftString) {
  if(!(grepl("^[a-fA-F0-9]+$", aircraftString)
       & is.character(aircraftString)
       & nchar(aircraftString) == 6)) {
    stop("One or more of the provided ICAO24 addresses are not valid.")
  }
}

checkCoordinate <- function(coordinateValue, coordinateName) {
  if(!(is.numeric(coordinateValue) 
       & coordinateValue >= -180
       & coordinateValue <= 180)) {
    stop(paste("The provided ", coordinateName, " is not valid.", sep=""))
  }
}

checkOpenSkiesStateVectorSet <- function(x, checkTimeSeries=FALSE) {
  if(class(x)[1] != "openSkiesStateVectorSet") {
    stop(strwrap(paste(deparse(substitute(x)), " is not an openSkiesStateVectorSet object", sep="")),
         initial="", prefix="\n")
  }
  if(checkTimeSeries & !(x$time_series)) {
    stop(strwrap("The provided openSkiesStateVectorSet object does not represent
                 a time series for a single aircraft", initial="", prefix="\n"))
  }
}

checkSquawk <- function(squawkCode) {
  if(!(is.character(squawkCode)
       & grepl("^[0-7][0-7][0-7][0-7]$", squawkCode))
     ) {
    stop(strwrap("Invalid squawk code provided. Squawk codes should be provided
                 as 4-digit strings, with each digit ranging between 0 and 7", 
                 initial="", prefix="\n"))
  }
}
