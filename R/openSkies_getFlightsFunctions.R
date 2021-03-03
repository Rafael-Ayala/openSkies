getAirportArrivals <- function(airport, startTime, endTime, timeZone=Sys.timezone(),
                               username=NULL, password=NULL, includeStateVectors=FALSE, 
                               timeResolution=NULL, useImpalaShell=FALSE) {
  checkAirport(airport)
  checkTime(startTime)
  checkTime(endTime)
  if(includeStateVectors && is.null(timeResolution)){
    stop("Time resolution must be provided when requesting state vectors.")
  }
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- tryCatch({
      GET(paste(openskyApiRootURL, "flights/arrival", sep="" ),
          query=list(airport=airport,
                     begin=stringToEpochs(startTime, timeZone),
                     end=stringToEpochs(endTime, timeZone)),
          timeout(300),
          if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
    },
    error = function(e) e
    )
    if(inherits(response, "error")) {
      message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
      return(NULL)
    }
    jsonResponse <- grepl("json", headers(response)$`content-type`)
    if(attemptCount > 100) {
      message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
      return(NULL)
    }
  }
  if(status_code(response) != 200) {
    message(strwrap("No arrivals found for the specified interval and 
                    airport.", initial="", prefix="\n"))
    return(NULL)
  }
  arrivalsList <- formatFlightsListResponse(content(response))
  arrivalsOpenSkiesFlights <- lapply(arrivalsList, listToOpenSkiesFlight)
  if(includeStateVectors){
    for(i in 1:length(arrivalsOpenSkiesFlights)){
      departureTime <- arrivalsOpenSkiesFlights[[i]]$departure_time
      arrivalTime <- arrivalsOpenSkiesFlights[[i]]$arrival_time
      stateVectors <- getAircraftStateVectorsSeries(arrivalsOpenSkiesFlights[[i]]$ICAO24, departureTime, arrivalTime, timeZone, timeResolution, username, password, useImpalaShell)
      arrivalsOpenSkiesFlights[[i]]$state_vectors <- stateVectors
    }
  }
  return(arrivalsOpenSkiesFlights)
}


getAirportDepartures <- function(airport, startTime, endTime, timeZone=Sys.timezone(),
                                 username=NULL, password=NULL, includeStateVectors=FALSE, 
                                 timeResolution=NULL, useImpalaShell=FALSE) {
  checkAirport(airport)
  checkTime(startTime)
  checkTime(endTime)
  if(includeStateVectors && is.null(timeResolution)){
    stop("Time resolution must be provided when requesting state vectors.")
  }
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- tryCatch({
      GET(paste(openskyApiRootURL, "flights/departure", sep="" ),
          query=list(airport=airport,
                     begin=stringToEpochs(startTime, timeZone),
                     end=stringToEpochs(endTime, timeZone)),
          timeout(300),
          if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
    },
    error = function(e) e
    )
    if(inherits(response, "error")) {
      message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
      return(NULL)
    }
    jsonResponse <- grepl("json", headers(response)$`content-type`)
    if(attemptCount > 100) {
      message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
      return(NULL)
    }
  }
  if(status_code(response) != 200) {
    message(strwrap("No departures found for the specified interval and 
                    airport.", initial="", prefix="\n"))
    return(NULL)
  }
  departuresList <- formatFlightsListResponse(content(response))
  departuresOpenSkiesFlights <- lapply(departuresList, listToOpenSkiesFlight)
  if(includeStateVectors){
    for(i in 1:length(departuresOpenSkiesFlights)){
      departureTime <- departuresOpenSkiesFlights[[i]]$departure_time
      arrivalTime <- departuresOpenSkiesFlights[[i]]$arrival_time
      stateVectors <- getAircraftStateVectorsSeries(departuresOpenSkiesFlights[[i]]$ICAO24, departureTime, arrivalTime, timeZone, timeResolution, username, password, useImpalaShell)
      departuresOpenSkiesFlights[[i]]$state_vectors <- stateVectors
    }
  }
  return(departuresOpenSkiesFlights)
}

getAircraftFlights <- function(aircraft, startTime, endTime, timeZone=Sys.timezone(),
                               username=NULL, password=NULL, includeStateVectors=FALSE, 
                               timeResolution=NULL, useImpalaShell=FALSE) {
  checkICAO24(aircraft)
  checkTime(startTime)
  checkTime(endTime)
  if(includeStateVectors && is.null(timeResolution)){
    stop("Time resolution must be provided when requesting state vectors.")
  }
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- tryCatch({
      GET(paste(openskyApiRootURL, "flights/aircraft", sep="" ),
          query=list(icao24=aircraft,
                     begin=stringToEpochs(startTime, timeZone),
                     end=stringToEpochs(endTime, timeZone)),
          timeout(300),
          if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
    },
    error = function(e) e
    )
    if(inherits(response, "error")) {
      message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
      return(NULL)
    }
    jsonResponse <- grepl("json", headers(response)$`content-type`)
    if(attemptCount > 100) {
      message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
      return(NULL)
    }
  }
  if(status_code(response) != 200) {
    message(strwrap("No flights found for the specified interval and 
                    aircraft", initial="", prefix="\n"))
    return(NULL)
  } 
  aircraftFlightsList <- formatFlightsListResponse(content(response))
  aircraftOpenSkiesFlights <- lapply(aircraftFlightsList, listToOpenSkiesFlight)
  if(includeStateVectors)
  for(i in 1:length(aircraftOpenSkiesFlights)){
    departureTime <- aircraftOpenSkiesFlights[[i]]$departure_time
    arrivalTime <- aircraftOpenSkiesFlights[[i]]$arrival_time
    stateVectors <- getAircraftStateVectorsSeries(aircraft, departureTime, arrivalTime, timeZone, timeResolution, username, password, useImpalaShell)
    aircraftOpenSkiesFlights[[i]]$state_vectors <- stateVectors
  }
  return(aircraftOpenSkiesFlights)
}

getIntervalFlights <- function(startTime, endTime, timeZone=Sys.timezone(),
                               username=NULL, password=NULL, includeStateVectors=FALSE, 
                               timeResolution=NULL, useImpalaShell=FALSE) {
  checkTime(startTime)
  checkTime(endTime)
  if(includeStateVectors && is.null(timeResolution)){
    stop("Time resolution must be provided when requesting state vectors.")
  }
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- tryCatch({
      GET(paste(openskyApiRootURL, "flights/all", sep="" ),
          query=list(begin=stringToEpochs(startTime, timeZone),
                     end=stringToEpochs(endTime, timeZone)),
          timeout(300),
          if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
    },
    error = function(e) e
    )
    if(inherits(response, "error")) {
      message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
      return(NULL)
    }
    jsonResponse <- grepl("json", headers(response)$`content-type`)
    if(attemptCount > 100) {
      message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
      return(NULL)
    }
  }
  if(status_code(response) != 200) {
    message("No flights found for the specified interval")
    return(NULL)
  } 
  intervalFlightsList <- formatFlightsListResponse(content(response))
  intervalOpenSkiesFlights <- lapply(intervalFlightsList, listToOpenSkiesFlight)
  if(includeStateVectors){
    for(i in 1:length(intervalOpenSkiesFlights)){
      departureTime <- intervalOpenSkiesFlights[[i]]$departure_time
      arrivalTime <- intervalOpenSkiesFlights[[i]]$arrival_time
      stateVectors <- getAircraftStateVectorsSeries(intervalOpenSkiesFlights[[i]]$ICAO24, departureTime, arrivalTime, timeZone, timeResolution, username, password, useImpalaShell)
      intervalOpenSkiesFlights[[i]]$state_vectors <- stateVectors
    }
  }
  return(intervalOpenSkiesFlights)
}