checkAirport <- function(airport) {
  if(!is.character(airport) | length(airport) == 0) {
    stop("Please enter a valid airport name.")
  }
  if(length(airport) > 1) {
    stop("Only one airport per query is supported")
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
