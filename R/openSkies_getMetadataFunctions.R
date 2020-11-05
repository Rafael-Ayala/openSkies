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


getAirportMetadata <- function(airport) {
  checkAirport(airport)
  response <- GET(paste(openskyApiRootURL, "airports", sep=""),
                  query=list(icao=airport))
  if(status_code(response) != 200) {
    stop(strwrap("No metadata for the airport with the provided ICAO code 
                  is available.", initial="", prefix="\n"))
  }
  formattedMetadata <- formatAirportMetadataResponse(content(response))
  return(formattedMetadata)
}

getRouteMetadata <- function(route) {
  checkCallSign(route)
  response <- GET(paste(openskyApiRootURL, "routes", sep=""),
                  query=list(callsign=route))
  if(status_code(response) != 200) {
    stop(strwrap("No metadata for the route/flight with the provided call sign 
                  is available.", initial="", prefix="\n"))
  }
  formattedMetadata <- formatRouteMetadataResponse(content(response))
  return(formattedMetadata)
}
