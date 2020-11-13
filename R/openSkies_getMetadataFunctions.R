getAircraftMetadata <- function(aircraft) {
  if(is.null(aircraft) | length(aircraft) > 1) {
    stop("Please provide one ICAO24 identifier.")
  }
  checkICAO24(aircraft)
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- GET(paste(openskyApiRootURL, "metadata/aircraft/icao/", aircraft, sep=""))
    jsonResponse <- grepl("json", headers(response)$`content-type`)
    if(attemptCount > 100) {
      message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
      return(NULL)
    }
  }
  if(status_code(response) != 200) {
    message(strwrap("No metadata for the aircraft with the provided ICAO24 address 
                     is available.", initial="", prefix="\n"))
    return(NULL)
  }
  formattedMetadata <- formatAircraftMetadataResponse(content(response))
  return(formattedMetadata)
}


getAirportMetadata <- function(airport) {
  checkAirport(airport)
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- GET(paste(openskyApiRootURL, "airports", sep=""),
                    query=list(icao=airport))
    jsonResponse <- grepl("json", headers(response)$`content-type`)
    if(attemptCount > 100) {
      message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
      return(NULL)
    }
  }
  if(status_code(response) != 200) {
    message(strwrap("No metadata for the airport with the provided ICAO code 
                     is available.", initial="", prefix="\n"))
    return(NULL)
  }
  formattedMetadata <- formatAirportMetadataResponse(content(response))
  return(formattedMetadata)
}

getRouteMetadata <- function(route) {
  checkCallSign(route)
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- GET(paste(openskyApiRootURL, "routes", sep=""),
                    query=list(callsign=route))
    jsonResponse <- grepl("json", headers(response)$`content-type`)
    if(attemptCount > 100) {
      message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
      return(NULL)
    }
  }
  if(status_code(response) != 200) {
    message(strwrap("No metadata for the flight with the provided call sign 
                  is available.", initial="", prefix="\n"))
    return(NULL)
  }
  formattedMetadata <- formatRouteMetadataResponse(content(response))
  openSkiesRouteResult <- openSkiesRoute$new(
    call_sign = formattedMetadata$callSign,
    origin_airport = formattedMetadata$originAirportICAO,
    destination_airport = formattedMetadata$destinationAirportICAO,
    operator_IATA = formattedMetadata$operatorIATA,
    flight_number = formattedMetadata$flightNumber
  )
  return(openSkiesRouteResult)
}
