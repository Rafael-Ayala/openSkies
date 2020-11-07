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

getMapLimits <- function(longitudes, latitudes, paddingFactor){
  minLon <- min(unlist(longitudes))
  maxLon <- max(unlist(longitudes))
  minLat <- min(unlist(latitudes))
  maxLat <- max(unlist(latitudes))
  width <- maxLon - minLon
  height <- maxLat - minLat
  mapLimits <- c(
    left <- minLon - width * paddingFactor,
    right <- maxLon + width * paddingFactor,
    top <- maxLat + height * paddingFactor,
    bottom <- minLat - height * paddingFactor
  )
  return(mapLimits)
}
