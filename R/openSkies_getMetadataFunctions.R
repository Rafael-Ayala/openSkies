getAircraftMetadata <- function(aircraft) {
  if(is.null(aircraft) | length(aircraft) > 1) {
    stop("Please provide one ICAO24 identifier.")
  }
  checkICAO24(aircraft)
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- tryCatch({
      GET(paste(openskyApiRootURL, "metadata/aircraft/icao/", aircraft, sep=""),
          timeout(300))
    },
    error = function(e) e
    )
    if(inherits(response, "error")) {
      message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
      return(NULL)
    }
    jsonResponse <- grepl("json", headers(response)$`content-type`)
    if(length(jsonResponse) == 0) {
      message(strwrap("No metadata for the aircraft with the provided ICAO24 address 
                     is available.", initial="", prefix="\n"))
      return(NULL)
    }
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
  openSkiesAircraftResult <- openSkiesAircraft$new(
    ICAO24 = formattedMetadata$ICAO24,
    registration = formattedMetadata$registration,
    origin_country = formattedMetadata$country,
    last_state_vector = NULL, 
    state_vector_history = NULL,
    manufacturer_name = formattedMetadata$manufacturerName,
    manufacturer_ICAO = formattedMetadata$manufacturerICAO,
    model = formattedMetadata$model,
    serial_number = formattedMetadata$serialNumber,
    line_number = formattedMetadata$lineNumber,
    ICAO_type_code = formattedMetadata$ICAOtypeCode,
    ICAO_aircraft_class = formattedMetadata$ICAOaircraftClass,
    owner = formattedMetadata$owner,
    operator = formattedMetadata$operator,
    operator_call_sign = formattedMetadata$operatorCallsign,
    operator_ICAO = formattedMetadata$operatorICAO,
    operator_IATA = formattedMetadata$operatorIATA,
    first_flight_date = formattedMetadata$firstFlightDate,
    category_description = formattedMetadata$categoryDescription
  )
  return(openSkiesAircraftResult)
}


getAirportMetadata <- function(airport) {
  checkAirport(airport)
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- tryCatch({
      GET(paste(openskyApiRootURL, "airports", sep=""),
          query=list(icao=airport),
          timeout(300))
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
    message(strwrap("No metadata for the airport with the provided ICAO code 
                     is available.", initial="", prefix="\n"))
    return(NULL)
  }
  formattedMetadata <- formatAirportMetadataResponse(content(response))
  openSkiesAirportResult <- openSkiesAirport$new(
    name = formattedMetadata$name,
    city = formattedMetadata$city,
    country = formattedMetadata$country,
    longitude = formattedMetadata$longitude,
    latitude = formattedMetadata$latitude,
    ICAO = formattedMetadata$ICAO,
    IATA = formattedMetadata$IATA,
    altitude = formattedMetadata$altitude,
    municipality = formattedMetadata$municipality,
    region = formattedMetadata$region,
    continent = formattedMetadata$continent,
    type = formattedMetadata$type,
    website = formattedMetadata$website,
    wikipedia_entry = formattedMetadata$wikipediaEntry,
    reliable_position = formattedMetadata$reliablePosition,
    GPS_code = formattedMetadata$GPSCode
  )
  return(openSkiesAirportResult)
}

getRouteMetadata <- function(route) {
  checkCallSign(route)
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- tryCatch({
      GET(paste(openskyApiRootURL, "routes", sep=""),
          query=list(callsign=route),
          timeout(300))
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
