getAirportArrivals <- function(airport, startTime, endTime, timeZone=Sys.timezone(),
                               username=NULL, password=NULL, includeStateVectors=FALSE, 
                               timeResolution=NULL, useTrino=FALSE, 
                               includeAirportsMetadata=FALSE,
                               timeOut=60, maxQueryAttempts=1) {
  checkAirport(airport)
  checkTime(startTime)
  checkTime(endTime)
  if(includeStateVectors && is.null(timeResolution)){
    stop("Time resolution must be provided when requesting state vectors.")
  }
  fixedIntervals <- makeIntervals(startTime, endTime, timeZone, 604800)
  startTimes <- fixedIntervals[[1]]
  endTimes <- fixedIntervals[[2]]
  allArrivalsOpenSkiesFlights <- NULL
  for(i in 1:length(startTimes)) {
      currentStartTime <- startTimes[i]
      currentEndTime <- endTimes[i]
      arrivalsCurrentInterval <- getAirportArrivalsSingleInterval(airport, currentStartTime, currentEndTime,
                                                       timeZone, username, password, includeStateVectors, 
                                                       timeResolution, useTrino, 
                                                       includeAirportsMetadata, timeOut,
                                                       maxQueryAttempts)
      allArrivalsOpenSkiesFlights <- c(allArrivalsOpenSkiesFlights, arrivalsCurrentInterval)
  }
  if(is.null(allArrivalsOpenSkiesFlights)) {
      message(strwrap("No arrivals found for the specified interval and 
                    airport.", initial="", prefix="\n"))
  }
  return(allArrivalsOpenSkiesFlights)
}


getAirportDepartures <- function(airport, startTime, endTime, timeZone=Sys.timezone(),
                                 username=NULL, password=NULL, includeStateVectors=FALSE, 
                                 timeResolution=NULL, useTrino=FALSE, 
                                 includeAirportsMetadata=FALSE,
                                 timeOut=60, maxQueryAttempts=1) {
  checkAirport(airport)
  checkTime(startTime)
  checkTime(endTime)
  if(includeStateVectors && is.null(timeResolution)){
    stop("Time resolution must be provided when requesting state vectors.")
  }
  fixedIntervals <- makeIntervals(startTime, endTime, timeZone, 604800)
  startTimes <- fixedIntervals[[1]]
  endTimes <- fixedIntervals[[2]]
  allDeparturesOpenSkiesFlights <- NULL
  for(i in 1:length(startTimes)) {
      currentStartTime <- startTimes[i]
      currentEndTime <- endTimes[i]
      departuresCurrentInterval <- getAirportDeparturesSingleInterval(airport, currentStartTime, currentEndTime,
                                                                    timeZone, username, password, includeStateVectors, 
                                                                    timeResolution, useTrino, 
                                                                    includeAirportsMetadata, timeOut,
                                                                    maxQueryAttempts)
      allDeparturesOpenSkiesFlights <- c(allDeparturesOpenSkiesFlights, departuresCurrentInterval)
  }
  if(is.null(allDeparturesOpenSkiesFlights)) {
      message(strwrap("No departures found for the specified interval and 
                    airport.", initial="", prefix="\n"))
  }
  return(allDeparturesOpenSkiesFlights)
}

getAircraftFlights <- function(aircraft, startTime, endTime, timeZone=Sys.timezone(),
                               username=NULL, password=NULL, includeStateVectors=FALSE, 
                               timeResolution=NULL, useTrino=FALSE, 
                               includeAirportsMetadata=FALSE,
                               timeOut=60, maxQueryAttempts=1) {
  checkICAO24(aircraft)
  checkTime(startTime)
  checkTime(endTime)
  if(includeStateVectors && is.null(timeResolution)){
    stop("Time resolution must be provided when requesting state vectors.")
  }
  fixedIntervals <- makeIntervals(startTime, endTime, timeZone, 604800)
  startTimes <- fixedIntervals[[1]]
  endTimes <- fixedIntervals[[2]]
  allAircraftOpenSkiesFlights <- NULL
  for(i in 1:length(startTimes)) {
      currentStartTime <- startTimes[i]
      currentEndTime <- endTimes[i]
      aircraftFlightsCurrentInterval <- getAircraftFlightsSingleInterval(aircraft, currentStartTime, currentEndTime, timeZone,
                                                                         username, password, includeStateVectors, 
                                                                         timeResolution, useTrino, 
                                                                         includeAirportsMetadata,
                                                                         timeOut, maxQueryAttempts)
      allAircraftOpenSkiesFlights <- c(allAircraftOpenSkiesFlights, aircraftFlightsCurrentInterval)
  }
  if(is.null(allAircraftOpenSkiesFlights)) {
      message(strwrap("No flights found for the specified interval and 
                    aircraft", initial="", prefix="\n"))
  }
  return(allAircraftOpenSkiesFlights)
}

getIntervalFlights <- function(startTime, endTime, timeZone=Sys.timezone(),
                               username=NULL, password=NULL, includeStateVectors=FALSE, 
                               timeResolution=NULL, useTrino=FALSE, 
                               includeAirportsMetadata=FALSE,
                               timeOut=60, maxQueryAttempts=1) {
  checkTime(startTime)
  checkTime(endTime)
  if(includeStateVectors && is.null(timeResolution)){
    stop("Time resolution must be provided when requesting state vectors.")
  }
  fixedIntervals <- makeIntervals(startTime, endTime, timeZone, 7200)
  startTimes <- fixedIntervals[[1]]
  endTimes <- fixedIntervals[[2]]
  allIntervalOpenSkiesFlights <- NULL
  for(i in 1:length(startTimes)) {
      currentStartTime <- startTimes[i]
      currentEndTime <- endTimes[i]
      intervalFlightsCurrentInterval <- getIntervalFlightsSingleInterval(currentStartTime, currentEndTime, timeZone,
                                                                         username, password, includeStateVectors, 
                                                                         timeResolution, useTrino, 
                                                                         includeAirportsMetadata,
                                                                         timeOut, maxQueryAttempts)
      allIntervalOpenSkiesFlights <- c(allIntervalOpenSkiesFlights, intervalFlightsCurrentInterval)
  }
  if(is.null(allIntervalOpenSkiesFlights)){
      message("No flights found for the specified interval")
  }
  return(allIntervalOpenSkiesFlights)
}