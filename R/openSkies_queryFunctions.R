getAirportArrivals <- function(airport, startTime, endTime, timeZone=Sys.timezone(),
                               username=NULL, password=NULL) {
  checkAirport(airport)
  checkTime(startTime)
  checkTime(endTime)
  response <- GET(paste(openskyApiRootURL, "flights/arrival", sep="" ),
                  query=list(airport=airport,
                             begin=stringToEpochs(startTime, timeZone),
                             end=stringToEpochs(endTime, timeZone)),
                  if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
  arrivalsList <- formatFlightsListResponse(content(response))
  return(arrivalsList)
}


getAirportDepartures <- function(airport, startTime, endTime, timeZone=Sys.timezone(),
                                 username=NULL, password=NULL) {
  checkAirport(airport)
  checkTime(startTime)
  checkTime(endTime)
  response <- GET(paste(openskyApiRootURL, "flights/departure", sep="" ),
                  query=list(airport=airport,
                             begin=stringToEpochs(startTime, timeZone),
                             end=stringToEpochs(endTime, timeZone)),
                  if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
  departuresList <- formatFlightsListResponse(content(response))
  return(departuresList)
}

getAircraftFlights <- function(aircraft, startTime, endTime, timeZone=Sys.timezone(),
                               username=NULL, password=NULL) {
  checkICAO24(aircraft)
  checkTime(startTime)
  checkTime(endTime)
  response <- GET(paste(openskyApiRootURL, "flights/aircraft", sep="" ),
                  query=list(icao24=aircraft,
                             begin=stringToEpochs(startTime, timeZone),
                             end=stringToEpochs(endTime, timeZone)),
                  if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
  flightsList <- formatFlightsListResponse(content(response))
  return(flightsList)
}

getIntervalFlights <- function(startTime, endTime, timeZone=Sys.timezone(),
                               username=NULL, password=NULL) {
  checkTime(startTime)
  checkTime(endTime)
  response <- GET(paste(openskyApiRootURL, "flights/all", sep="" ),
                  query=list(begin=stringToEpochs(startTime, timeZone),
                             end=stringToEpochs(endTime, timeZone)),
                  if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
  flightsList <- formatFlightsListResponse(content(response))
  return(flightsList)
}

getSingleTimeStateVectors <- function(aircraft=NULL, time=NULL, timeZone=Sys.timezone(),
                                      minLatitude=NULL, maxLatitude=NULL, minLongitude=NULL,
                                      maxLongitude=NULL, username=NULL, password=NULL) {
  if(!is.null(aircraft)) {
    checkICAO24(aircraft)
  }
  if(!is.null(time)){
    checkTime(time)
  }
  if(!is.null(minLatitude)) {
    checkCoordinate(minLatitude, "minimum latitude")
  }
  if(!is.null(maxLatitude)) {
    checkCoordinate(maxLatitude, "maximum latitude")
  }
  if(!is.null(minLongitude)) {
    checkCoordinate(minLongitude, "minimum longitude")
  }
  if(!is.null(maxLongitude)) {
    checkCoordinate(maxLongitude, "maximum longitude")
  }
  if(is.null(username) | is.null(password)) {
    if(!is.null(time)) {
      if(is.null(aircraft)) {
        time <- NULL
        warning(strwrap("Timepoint specified but no login provided and no aircraft
                        specified. Specified time will be ignored and data for current
                        time will be retrieved.", initial="", prefix="\n"))
      } else if(length(aircraft) > 1) {
        time <- NULL
        warning(strwrap("Timepoint specified but no login provided and several aircrafts
                        specified. Specified time will be ignored and data for current
                        time will be retrieved.", initial="", prefix="\n"))
      }
    }
  } else if(!is.null(time)) {
    if(secondsFromCurrentTime(time, timeZone) >= 3600) {
      if(is.null(aircraft) | length(aircraft) > 1) {
        stop(strwrap("Historical data for multiple aircrafts older than 1 hour ago
                     cannot be retrieved. Please specify a single aircraft or
                     request a timepoint not older than 1 hour ago.", initial="",
                     prefix="\n"))
      }
    }
  }
  queryParameters <- c(makeAircraftsQueryList(aircraft),
                       time=if(!is.null(time)) stringToEpochs(time, timeZone) else NULL,
                       lamin=minLatitude, lomin=minLongitude,
                       lamax=maxLatitude, lomax=maxLongitude)
  queryParameters <- as.list(queryParameters[lengths(queryParameters) != 0])
  response <- GET(paste(openskyApiRootURL, "states/all", sep="" ),
                  query=queryParameters,
                  if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
  formattedStateVectors <- formatStateVectorsResponse(content(response))
  if(length(formattedStateVectors)==1) formattedStateVectors <- unlist(formattedStateVectors, recursive=FALSE)
  return(formattedStateVectors)
}

getAircraftStateVectorsSeries <- function(aircraft, startTime, endTime, timeZone=Sys.timezone(),
                                          timeResolution, username=NULL, password=NULL) {
  if(timeResolution < 10) {
    if(is.null(username) | is.null(password)) {
      timeResolution <- 10
      warning(strwrap("Anonymous users cannot retrieve data with a time resolution
                      higher than 10 seconds. A time resolution of 10 seconds will be used.",
                      initial="", prefix="\n"))
    } else if(timeResolution < 5) {
      timeResolution <- 5
      warning(strwrap("Data with a time resolution higher than 5 seconds cannot
                      be retrieved. A time resolution of 5 seconds will be used.",
                      initial="", prefix="\n"))
    }
  }
  timePoints <- generateTimePoints(startTime, endTime, timeZone, timeResolution)
  stateVectorsSeries <- vector(mode="list", length=length(timePoints))
  for(i in seq_len(length(timePoints))) {
    response <- GET(paste(openskyApiRootURL, "states/all", sep="" ),
                    query=list(icao24=aircraft,
                               time=timePoints[i]),
                    if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
    stateVectorsSeries[[i]] <- unlist(formatStateVectorsResponse(content(response)), recursive=FALSE)
  }
  return(stateVectorsSeries)
}

getAircraftMetadata <- function(aircraft) {
  if(is.null(aircraft) | length(aircraft) > 1) {
    stop("Please provide one ICAO24 identifier.")
  }
  checkICAO24(aircraft)
  response <- GET(paste(openskyApiRootURL, "metadata/aircraft/icao/", aircraft, sep=""))
  if(status_code(response) != 200) {
    stop(strwrap("No metadata for the aircraft with the provided ICAO24 address 
                 is available.", initial="", prefix="\n"))
  }
  formattedMetadata <- formatAircraftMetadataResponse(content(response))
  return(formattedMetadata)
}