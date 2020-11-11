getAirportArrivals <- function(airport, startTime, endTime, timeZone=Sys.timezone(),
                               username=NULL, password=NULL) {
  checkAirport(airport)
  checkTime(startTime)
  checkTime(endTime)
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- GET(paste(openskyApiRootURL, "flights/arrival", sep="" ),
                    query=list(airport=airport,
                               begin=stringToEpochs(startTime, timeZone),
                               end=stringToEpochs(endTime, timeZone)),
                    if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
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
  return(arrivalsList)
}


getAirportDepartures <- function(airport, startTime, endTime, timeZone=Sys.timezone(),
                                 username=NULL, password=NULL) {
  checkAirport(airport)
  checkTime(startTime)
  checkTime(endTime)
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- GET(paste(openskyApiRootURL, "flights/departure", sep="" ),
                    query=list(airport=airport,
                               begin=stringToEpochs(startTime, timeZone),
                               end=stringToEpochs(endTime, timeZone)),
                    if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
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
  return(departuresList)
}

getAircraftFlights <- function(aircraft, startTime, endTime, timeZone=Sys.timezone(),
                               username=NULL, password=NULL) {
  checkICAO24(aircraft)
  checkTime(startTime)
  checkTime(endTime)
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- GET(paste(openskyApiRootURL, "flights/aircraft", sep="" ),
                    query=list(icao24=aircraft,
                               begin=stringToEpochs(startTime, timeZone),
                               end=stringToEpochs(endTime, timeZone)),
                    if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
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
  flightsList <- formatFlightsListResponse(content(response))
  return(flightsList)
}

getIntervalFlights <- function(startTime, endTime, timeZone=Sys.timezone(),
                               username=NULL, password=NULL) {
  checkTime(startTime)
  checkTime(endTime)
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- GET(paste(openskyApiRootURL, "flights/all", sep="" ),
                    query=list(begin=stringToEpochs(startTime, timeZone),
                               end=stringToEpochs(endTime, timeZone)),
                    if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
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
  flightsList <- formatFlightsListResponse(content(response))
  return(flightsList)
}
