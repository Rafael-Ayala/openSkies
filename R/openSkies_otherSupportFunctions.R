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

groupByFunction <- function(elements, groupingFunction, includeNull=FALSE, nullKeyName="unclassified"){
  groups <- list()
  for(element in elements){
    key <- groupingFunction(element)
    if(is.null(key)){
      if(includeNull) {
        key <- nullKeyName  
      } else {
        next
      }
    }
    key <- as.character(key)
    if(is.null(groups[[key]])){
      groups[[key]] <- list()
    }
    groups[[key]][[length(groups[[key]])+1]] <- element
  }
  return(groups)
}

secondsToHour <- function(seconds){
  hour <- seconds - (seconds %% 3600)
  return(hour)
}

makeImpalaQueryStateVectorsSingleTime <- function(aircraft=NULL, time=NULL, timeZone=Sys.timezone(),
                            minLatitude=NULL, maxLatitude=NULL, minLongitude=NULL,
                            maxLongitude=NULL){
  timeSeconds <- stringToEpochs(time, timeZone)
  hour <- secondsToHour(timeSeconds)
  query <- paste0("SELECT * FROM state_vectors_data4 WHERE hour=", hour, " AND time=", timeSeconds)
  if(!is.null(aircraft)){
    query <- paste0(query, " AND icao24='", aircraft, "'")
  }
  if(!is.null(minLatitude)){
    query <- paste0(query, " AND lat>=", minLatitude)
  }
  if(!is.null(maxLatitude)){
    query <- paste0(query, " AND lat<=", maxLatitude)
  }
  if(!is.null(minLongitude)){
    query <- paste0(query, " AND lon>=", minLongitude)
  }
  if(!is.null(maxLongitude)){
    query <- paste0(query, " AND lon<=", maxLongitude)
  }
  
  return(query)
}

makeImpalaQueryStateVectorsTimeSeries <- function(aircraft, timePoints){
  hours <- paste0('(',paste(unique(unlist(lapply(timePoints, secondsToHour))), collapse=','),')')
  times <- paste0('(',paste(timePoints, collapse=','),')')
  query <- paste0("SELECT * FROM state_vectors_data4 WHERE hour IN ", hours, " AND time IN ", times)
  if(!is.null(aircraft)){
    query <- paste0(query, " AND icao24='", aircraft, "'")
  }
  query <- paste0(query, " ORDER BY time")
  return(query)
}

runImpalaQuery <- function(query, username, password){
  session <- ssh_connect(paste(username,"@data.opensky-network.org:2230"), passwd=password)
  lines <- rawToChar(ssh_exec_internal(session,paste("-q ", query))$stdout)
  lines <- unlist(strsplit(lines, '\n'))
  colnames_line <- lines[2]
  colnames_line <- gsub("^.|.$", "", colnames_line)
  columnNames <- unlist(strsplit(colnames_line, '|', fixed=TRUE))
  columnNames <- trimws(columnNames)
  data_lines <- lines[4:(length(lines)-1)]
  data_lines <- gsub("^.|.$", "", data_lines)
  split_data_lines <- strsplit(data_lines, '|', fixed=TRUE)
  trimmed_data_lines <- lapply(split_data_lines, trimws)
  data_matrix <- matrix(unlist(trimmed_data_lines), ncol=length(columnNames), byrow=TRUE)
  colnames(data_matrix) <- columnNames
  ssh_disconnect(session)
  return(data_matrix)
}

generateEnclosingAirspace <- function(elements, groupingFunction){
  
  
}