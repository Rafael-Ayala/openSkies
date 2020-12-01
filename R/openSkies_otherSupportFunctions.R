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

getMapLimits <- function(longitudes, latitudes, paddingFactor) {
  minLon <- min(na.omit(unlist(longitudes)))
  maxLon <- max(na.omit(unlist(longitudes)))
  minLat <- min(na.omit(unlist(latitudes)))
  maxLat <- max(na.omit(unlist(latitudes)))
  width <- maxLon - minLon
  height <- maxLat - minLat
  mapLimits <- c(
    left = minLon - width * paddingFactor,
    right = maxLon + width * paddingFactor,
    top = maxLat + height * paddingFactor,
    bottom = minLat - height * paddingFactor
  )
  return(mapLimits)
}

listToOpenSkiesFlight <- function(flightDataList) {
  openSkiesFlightObject <- openSkiesFlight$new(
    ICAO24 = flightDataList$ICAO24,
    call_sign = flightDataList$callSign,
    origin_airport = flightDataList$departureAirport,
    destination_airport = flightDataList$arrivalAirport,
    departure_time = flightDataList$departureTime,
    arrival_time = flightDataList$arrivalTime
  )
}

listToOpenSkiesStateVector <- function(stateVectorList) {
  openSkiesStateVectorObject <- openSkiesStateVector$new(
    ICAO24 = stateVectorList$ICAO24, 
    call_sign= stateVectorList$callSign, 
    origin_country = stateVectorList$originCountry, 
    requested_time = stateVectorList$requestedTime,
    last_position_update_time = stateVectorList$lastPositionUpdateTime, 
    last_any_update_time = stateVectorList$lastAnyUpdateTime,
    longitude = stateVectorList$longitude, 
    latitude = stateVectorList$latitude, 
    baro_altitude = stateVectorList$baroAltitude,
    geo_altitude = stateVectorList$geoAltitude,
    on_ground = stateVectorList$onGround, 
    velocity = stateVectorList$velocity, 
    true_track = stateVectorList$trueTrack, 
    vertical_rate = stateVectorList$verticalRate,
    squawk = stateVectorList$squawk, 
    special_purpose_indicator = stateVectorList$specialPurposeIndicator, 
    position_source = stateVectorList$positionSource
  )
}

unwrapAngles <- function(angles, usingRadians=FALSE) {
  newAngles = c(angles[0])
  loops = 0
  if(usingRadians){
    threshold = pi
    halfValue = pi
  } else {
    threshold = 180
    halfValue = 180
  }
  angles = angles %% (2 * halfValue)
  for (i in 2:length(angles)){
    angle1 = angles[i-1]
    angle2 = angles[i]
    diff = angle2 - angle1
    # Counter-clockwise loop
    if(angle1>halfValue && angle2<halfValue && diff <= -threshold){
      loops = loops + 1
      # Clockwise loop
    } else if(angle2>halfValue && angle1<halfValue && diff >= threshold){
      loops = loops - 1
    }
    newAngle = angle2 + loops * (2*halfValue)
    newAngles = c(newAngles, newAngle)
  }
  return(newAngles)
}
