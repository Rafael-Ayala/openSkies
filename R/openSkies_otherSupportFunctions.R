formatFlightsListResponse <- function(responseList) {
  formattedList <- lapply(responseList, function(flight) list("ICAO24"=flight$icao24,
                                                              "callSign"=trimws(flight$callsign),
                                                              "departureAirport"=flight$estDepartureAirport,
                                                              "arrivalAirport"=flight$estArrivalAirport,
                                                              "departureTime"=as.POSIXct(flight$firstSeen, origin="1970-01-01"),
                                                              "arrivalTime"=as.POSIXct(flight$lastSeen, origin="1970-01-01")))
}

formatStateVectorsResponse <- function(responseList) {
  formattedList <- lapply(responseList$states, function(stateVector) list("ICAO24"=stateVector[[1]],
                                                                          "callSign"=trimws(stateVector[[2]]),
                                                                          "originCountry"=stateVector[[3]],
                                                                          "requestedTime"=as.POSIXct(responseList$time, origin="1970-01-01"),
                                                                          "lastPositionUpdateTime"=if(!is.null(stateVector[[4]])) as.POSIXct(stateVector[[4]], origin="1970-01-01") else NULL,
                                                                          "lastAnyUpdateTime"=if(!is.null(stateVector[[5]])) as.POSIXct(stateVector[[5]], origin="1970-01-01") else NULL,
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
                        "cattegoryDescription"=responseList$cattegoryDescription
  )
  return(formattedList)
}

stringToEpochs <- function(dateTimeString, timeZone) {
  return(as.integer(as.POSIXct(dateTimeString, tz=timeZone, origin="1970-01-01")))
}

secondsFromCurrentTime <- function(dateTimeString, timeZone="UTC") {
  return(stringToEpochs(Sys.time(), Sys.timezone())-stringToEpochs(dateTimeString, timeZone))
}

makeAircraftsQueryList <- function(aircraft) {
  if(is.null(aircraft)) {
    aircraftsQueryList <- NULL
  } else {
    aircraftsQueryList <- as.list(aircraft)
    names(aircraftsQueryList) <- rep("icao24", times=length(aircraft))
  }
  return(aircraftsQueryList)
}

generateTimePoints <- function(startTime, endTime, timeZone, timeResolution) {
  startTimeEpochs <- stringToEpochs(startTime, timeZone)
  endTimeEpochs <- stringToEpochs(endTime, timeZone)
  return(seq(startTimeEpochs, endTimeEpochs, by=timeResolution))
}
