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
