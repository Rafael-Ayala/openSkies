formatFlightsListResponse <- function(responseList) {
  formattedList <- lapply(responseList, function(flight) list("ICAO24"=flight$icao24,
                                                              "callSign"=trimws(flight$callsign),
                                                              "departureAirport"=flight$estDepartureAirport,
                                                              "arrivalAirport"=flight$estArrivalAirport,
                                                              "departureTime"=as.POSIXct(flight$firstSeen, origin="1970-01-01", tz=Sys.timezone()),
                                                              "arrivalTime"=as.POSIXct(flight$lastSeen, origin="1970-01-01", tz=Sys.timezone())))
}

formatStateVectorsResponseImpala <- function(responseMatrix) {
  formattedList <- list()
  for(i in 1:nrow(responseMatrix)){
    row <- responseMatrix[i,]
    parsedRow <- list(
                      "ICAO24"=if(row["icao24"]!="NULL") row["icao24"] else NULL,
                      "callSign"=if(row["callsign"]!="NULL") row["callsign"] else NULL,
                      "lastPositionUpdateTime"=if(row["lastposupdate"]!="NULL") as.POSIXct(as.numeric(row["lastposupdate"]), origin="1970-01-01", tz=Sys.timezone()) else NULL,
                      "lastAnyUpdateTime"=if(row["lastcontact"]!="NULL") as.POSIXct(as.numeric(row["lastcontact"]), origin="1970-01-01", tz=Sys.timezone()) else NULL,
                      "longitude"=if(row["lon"]!="NULL") as.numeric(row["lon"]) else NULL,
                      "latitude"=if(row["lat"]!="NULL") as.numeric(row["lat"]) else NULL,
                      "baroAltitude"=if(row["baroaltitude"]!="NULL") as.numeric(row["baroaltitude"]) else NULL,
                      "geoAltitude"=if(row["geoaltitude"]!="NULL") as.numeric(row["geoaltitude"]) else NULL,
                      "onGround"=if(row["onground"]!="NULL") as.logical(row["onground"]) else NULL,
                      "velocity"=if(row["velocity"]!="NULL") as.numeric(row["velocity"]) else NULL,
                      "verticalRate"=if(row["vertrate"]!="NULL") as.numeric(row["vertrate"]) else NULL,
                      "squawk"=if(row["squawk"]!="NULL") row["squawk"] else NULL,
                      "specialPurposeIndicator"=if(row["spi"]!="NULL") as.logical(row["spi"]) else NULL,
                      "originCountry"=NULL,
                      "requestedTime"=if(row["time"]!="NULL") as.POSIXct(as.numeric(row["time"]), origin="1970-01-01", tz=Sys.timezone()) else NULL,
                      "trueTrack"=if(row["heading"]!="NULL") as.numeric(row["heading"]) else NULL,
                      "positionSource"=NULL
                      )
    formattedList[[i]] <- parsedRow
    
  }
  return(formattedList)
}

formatStateVectorsResponse <- function(responseList) {
  formattedList <- lapply(responseList$states, function(stateVector) list("ICAO24"=stateVector[[1]],
                                                                          "callSign"=trimws(stateVector[[2]]),
                                                                          "originCountry"=stateVector[[3]],
                                                                          "requestedTime"=as.POSIXct(responseList$time, origin="1970-01-01", tz=Sys.timezone()),
                                                                          "lastPositionUpdateTime"=if(!is.null(stateVector[[4]])) as.POSIXct(stateVector[[4]], origin="1970-01-01", tz=Sys.timezone()) else NULL,
                                                                          "lastAnyUpdateTime"=if(!is.null(stateVector[[5]])) as.POSIXct(stateVector[[5]], origin="1970-01-01", tz=Sys.timezone()) else NULL,
                                                                          "longitude"=stateVector[[6]],
                                                                          "latitude"=stateVector[[7]],
                                                                          "baroAltitude"=stateVector[[8]],
                                                                          "geoAltitude"=stateVector[[14]],
                                                                          "onGround"=stateVector[[9]],
                                                                          "velocity"=stateVector[[10]],
                                                                          "trueTrack"=stateVector[[11]],
                                                                          "verticalRate"=stateVector[[12]],
                                                                          "squawk"=stateVector[[15]],
                                                                          "specialPurposeIndicator"=stateVector[[16]],
                                                                          "positionSource"=switch(as.integer(stateVector[[17]]) + 1,
                                                                                                  "ADS-B",
                                                                                                  "ASTERIX",
                                                                                                  "MLAT"
                                                                          )))
  return(formattedList)
}

formatAircraftMetadataResponse <- function(responseList) {
  formattedList <- list("ICAO24"=responseList$icao24,
                        "registration"=responseList$registration,
                        "country"=responseList$country,
                        "manufacturerName"=responseList$manufacturerName,
                        "manufacturerICAO"=responseList$manufacturerIcao,
                        "model"=responseList$model,
                        "serialNumber"=responseList$serialNumber,
                        "lineNumber"=responseList$lineNumber,
                        "ICAOtypeCode"=responseList$typecode,
                        "ICAOaircraftClass"=responseList$icaoAircraftClass,
                        "owner"=responseList$owner,
                        "operator"=responseList$operator,
                        "operatorCallsign"=responseList$operatorCallsign,
                        "operatorICAO"=responseList$operatorIcao,
                        "operatorIATA"=responseList$operatorIata,
                        "firstFlightDate"=responseList$firstFlightDate,
                        "categoryDescription"=responseList$categoryDescription
  )
  return(formattedList)
}

formatAirportMetadataResponse <- function(responseList) {
  formattedList <- list("ICAO"=responseList$icao,
                        "IATA"=responseList$iata,
                        "name"=responseList$name,
                        "city"=responseList$city,
                        "municipality"=responseList$municipality,
                        "region"=responseList$region,
                        "country"=responseList$country,
                        "continent"=responseList$continent,
                        "longitude"=responseList$position$longitude,
                        "latitude"=responseList$position$latitude,
                        "altitude"=responseList$position$altitude,
                        "reliablePosition"=responseList$position$reasonable,
                        "GPSCode"=responseList$gpsCode,
                        "type"=responseList$type,
                        "website"=responseList$homepage,
                        "wikipediaEntry"=responseList$wikipedia
  )
  return(formattedList)
}

formatRouteMetadataResponse <- function(responseList) {
  formattedList <- list("callSign"=responseList$callsign,
                        "originAirportICAO"=responseList$route[[1]],
                        "destinationAirportICAO"=responseList$route[[2]],
                        "operatorIATA"=responseList$operatorIata,
                        "flightNumber"=responseList$flightNumber
  )
  return(formattedList)
}
