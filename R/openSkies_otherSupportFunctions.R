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
  newAngles = c(angles[1])
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
    if(angle1>halfValue & angle2<halfValue & diff <= -threshold){
      loops = loops + 1
      # Clockwise loop
    } else if(angle2>halfValue & angle1<halfValue & diff >= threshold){
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

makeImpalaQueryStateVectorsInterval <- function(aircraft=NULL, startTime, endTime,
                                                timeZone=Sys.timezone(), minLatitude=NULL, 
                                                maxLatitude=NULL, minLongitude=NULL, maxLongitude=NULL,
                                                minBaroAltitude=NULL, maxBaroAltitude=NULL,
                                                minGeoAltitude=NULL, maxGeoAltitude=NULL,
                                                minVelocity=NULL, maxVelocity=NULL,
                                                minVerticalRate=NULL, maxVerticalRate=NULL,
                                                callSignFilter=NULL, onGroundStatus=NULL,
                                                squawkFilter=NULL, spiStatus=NULL, alertStatus=NULL) {
  startTimeSeconds <- stringToEpochs(startTime, timeZone)
  endTimeSeconds <- stringToEpochs(endTime, timeZone)
  startHour <- secondsToHour(startTimeSeconds)
  endHour <- secondsToHour(endTimeSeconds)
  query <- paste0("SELECT * FROM state_vectors_data4 WHERE hour>=", startHour,
                  " AND hour<=", endHour, " AND time>=", startTimeSeconds,
                  " AND time<=", endTimeSeconds)
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
  if(!is.null(minBaroAltitude)){
    query <- paste0(query, " AND baroaltitude>=", minBaroAltitude)
  }
  if(!is.null(maxBaroAltitude)){
    query <- paste0(query, " AND baroaltitude<=", maxBaroAltitude)
  }
  if(!is.null(minGeoAltitude)){
    query <- paste0(query, " AND geoaltitude>=", minGeoAltitude)
  }
  if(!is.null(maxGeoAltitude)){
    query <- paste0(query, " AND geoaltitude<=", maxGeoAltitude)
  }
  if(!is.null(minVelocity)){
    query <- paste0(query, " AND velocity>=", minVelocity)
  }
  if(!is.null(maxVelocity)){
    query <- paste0(query, " AND velocity<=", maxVelocity)
  }
  if(!is.null(minVerticalRate)){
    query <- paste0(query, " AND vertrate>=", minVerticalRate)
  }
  if(!is.null(maxVerticalRate)){
    query <- paste0(query, " AND vertrate<=", maxVerticalRate)
  }
  if(!is.null(callSignFilter)){
    query <- paste0(query, ' AND callsign REGEXP "', paste(callSignFilter, collapse = "|"), '"')
  }
  if(!is.null(onGroundStatus)){
    query <- paste0(query, ' AND (', if(!onGroundStatus) "NOT ", onGroundStatus, ")")
  }
  if(!is.null(squawkFilter)){
    query <- paste0(query, ' AND squawk REGEXP "', paste(squawkFilter, collapse = "|"), '"')
  }
  if(!is.null(spiStatus)){
    query <- paste0(query, ' AND (', if(!spiStatus) "NOT ", spiStatus, ")")
  }
  if(!is.null(alertStatus)){
    query <- paste0(query, ' AND (', if(!alertStatus) "NOT ", alertStatus, ")")
  }
  return(query)
}

runTrinoQuery <- function(query, connection) {
    result <- dbGetQuery(connection, query)
    return(as.data.frame(result))
}

runImpalaQuery <- function(query, username, password){
  session <- ssh_connect(paste(username,"@data.opensky-network.org:2230"), passwd=password)
  lines <- rawToChar(ssh_exec_internal(session,paste("-q ", query))$stdout)
  if(lines == "") {
    return(NULL)
  }
  lines <- unlist(strsplit(lines, '\n'))
  colnames_line <- lines[2]
  colnames_line <- gsub("^.|.$", "", colnames_line)
  columnNames <- unlist(strsplit(colnames_line, '|', fixed=TRUE))
  columnNames <- trimws(columnNames)
  data_lines <- lines[4:(length(lines)-1)]
  data_lines <- gsub("^.|.$", "", data_lines)
  split_data_lines <- strsplit(data_lines, '|', fixed=TRUE)
  trimmed_data_lines <- lapply(split_data_lines, trimws)
  if(length(trimmed_data_lines) > 1024) {
    extra_pages_number <- length(trimmed_data_lines) %/% 1024
    repeated_header_lines <- sequence(nvec=rep(4, extra_pages_number), 
                                      from=(1:extra_pages_number)*1024 + 1
                                            + seq(from=0, by=4, length=extra_pages_number))
    trimmed_data_lines <- trimmed_data_lines[-repeated_header_lines]
  }
  data_matrix <- matrix(unlist(trimmed_data_lines), ncol=length(columnNames), byrow=TRUE)
  colnames(data_matrix) <- columnNames
  ssh_disconnect(session)
  return(data_matrix)
}

G_membership <- function(x, mu, sigma) {
    membership <- exp((-(x - mu)^2) / (2*sigma^2))
    return(membership)
}

Z_membership <- function(x, a, b) {
    membership <- numeric(length(x))
    set1 <- x <= a
    set2 <- (x >= a) & (x <= ((a + b)/2))
    set3 <- (x >= ((a + b)/2)) & (x <= b)
    set4 <- x >= b
    membership[set1] <- 1
    membership[set2] <- 1 - 2*((x[set2] - a)/(b - a))^2
    membership[set3] <- 2*((x[set3] - b)/(b - a))^2
    membership[set4] <- 0
    return(membership)
}

S_membership <- function(x, a, b) {
    membership <- numeric(length(x))
    set1 <- x <= a
    set2 <- (x >= a) & (x <= ((a + b)/2))
    set3 <- (x >= ((a + b)/2)) & (x <= b)
    set4 <- x >= b
    membership[set1] <- 0
    membership[set2] <- 2*((x[set2] - a)/(b - a))^2
    membership[set3] <- 1 - 2*((x[set3] - b)/(b - a))^2
    membership[set4] <- 1
    return(membership)
}

# generateEnclosingAirspace <- function(elements, groupingFunction){ TODO
#   
#   
# }

makeIntervals <- function(startTime, endTime, timeZone, intervalLength) {
    intervalSecs <- abs(difftime(endTime, startTime, units = "secs"))
    if(intervalSecs > intervalLength) {
        numberOfIntervals <- as.numeric(intervalSecs/intervalLength)
        IntNumberOfIntervals <- trunc(numberOfIntervals)
        fracIntervals <- numberOfIntervals %% 1
        startTimes <- as.character(
            as.POSIXct(startTime, tz=timeZone) +
                seq(from=0, by=intervalLength+1, length.out=IntNumberOfIntervals + 1)
        )
        endTimes <- c(
            as.character(
                as.POSIXct(startTime, tz=timeZone) +
                    seq(from=intervalLength, by=intervalLength, length.out=IntNumberOfIntervals)
            )
            , endTime)
        if(fracIntervals == 0) {
            startTimes <- head(startTimes, -1)
            endTimes <- head(endTimes, -1)
        }
    } else {
        startTimes <- startTime
        endTimes <- endTime
    }
    return(list(startTimes, endTimes))
}

getAirportArrivalsSingleInterval <- function(airport, startTime, endTime, timeZone, 
                                             username, password, includeStateVectors, 
                                             timeResolution, useTrino, 
                                             includeAirportsMetadata, timeOut,
                                             maxQueryAttempts, credentials = NULL) {
  query<-list(airport=airport, begin=stringToEpochs(startTime,timeZone),end=stringToEpochs(endTime, timeZone))
  response <-makeAuthenticatedRequest("flights/arrival", query, credentials, timeOut, maxQueryAttempts)
  # FIXME catch NULL response if 404 no flights in interval
    arrivalsList <- formatFlightsListResponse(content(response))
    arrivalsOpenSkiesFlights <- lapply(arrivalsList, listToOpenSkiesFlight)
    if(includeStateVectors){
        for(i in 1:length(arrivalsOpenSkiesFlights)){
            departureTime <- arrivalsOpenSkiesFlights[[i]]$departure_time
            arrivalTime <- arrivalsOpenSkiesFlights[[i]]$arrival_time
            stateVectors <- getAircraftStateVectorsSeries(arrivalsOpenSkiesFlights[[i]]$ICAO24, departureTime, arrivalTime, timeZone, timeResolution, username, password, useTrino)
            arrivalsOpenSkiesFlights[[i]]$state_vectors <- stateVectors
        }
    }
    if(includeAirportsMetadata){
        for(i in 1:length(arrivalsOpenSkiesFlights)){
            originAirportICAO <- arrivalsOpenSkiesFlights[[i]]$origin_airport
            destinationAirportICAO <- arrivalsOpenSkiesFlights[[i]]$destination_airport
            if(!is.null(originAirportICAO)){
                originAirport <- getAirportMetadata(originAirportICAO)
                arrivalsOpenSkiesFlights[[i]]$origin_airport <- originAirport
            }
            if(!is.null(destinationAirportICAO)){
                destinationAirport <- getAirportMetadata(destinationAirportICAO)
                arrivalsOpenSkiesFlights[[i]]$destination_airport <- destinationAirport
            }
        }
    }
    return(arrivalsOpenSkiesFlights)
}

getAirportDeparturesSingleInterval <- function(airport, startTime, endTime, timeZone,
                                               username, password, includeStateVectors, 
                                               timeResolution, useTrino, includeAirportsMetadata,
                                               timeOut, maxQueryAttempts, credentials=NULL) {
  query<-list(airport=airport, begin=stringToEpochs(startTime,timeZone),end=stringToEpochs(endTime, timeZone))
  response <-makeAuthenticatedRequest("flights/departure", query, credentials, timeOut, maxQueryAttempts)
  # FIXME catch NULL response if 404 no flights in interval
    departuresList <- formatFlightsListResponse(content(response))
    departuresOpenSkiesFlights <- lapply(departuresList, listToOpenSkiesFlight)
    if(includeStateVectors){
        for(i in 1:length(departuresOpenSkiesFlights)){
            departureTime <- departuresOpenSkiesFlights[[i]]$departure_time
            arrivalTime <- departuresOpenSkiesFlights[[i]]$arrival_time
            stateVectors <- getAircraftStateVectorsSeries(departuresOpenSkiesFlights[[i]]$ICAO24, departureTime, arrivalTime, timeZone, timeResolution, username, password, useTrino)
            departuresOpenSkiesFlights[[i]]$state_vectors <- stateVectors
        }
    }
    if(includeAirportsMetadata){
        for(i in 1:length(departuresOpenSkiesFlights)){
            originAirportICAO <- departuresOpenSkiesFlights[[i]]$origin_airport
            destinationAirportICAO <- departuresOpenSkiesFlights[[i]]$destination_airport
            if(!is.null(originAirportICAO)){
                originAirport <- getAirportMetadata(originAirportICAO)
                departuresOpenSkiesFlights[[i]]$origin_airport <- originAirport
            }
            if(!is.null(destinationAirportICAO)){
                destinationAirport <- getAirportMetadata(destinationAirportICAO)
                departuresOpenSkiesFlights[[i]]$destination_airport <- destinationAirport
            }
        }
    }
    return(departuresOpenSkiesFlights)
}

getAircraftFlightsSingleInterval <- function(aircraft, startTime, endTime, timeZone,
                                             username, password, includeStateVectors, 
                                             timeResolution, useTrino, 
                                             includeAirportsMetadata,
                                             timeOut, maxQueryAttempts, credentials=NULL) {
  query<-list(icao24=aircraft, begin=stringToEpochs(startTime, timeZone), end=stringToEpochs(endTime, timeZone))
  response <-makeAuthenticatedRequest("flights/aircraft", query, credentials, timeOut, maxQueryAttempts)
  # FIXME catch NULL response if 404 no flights in interval
    aircraftFlightsList <- formatFlightsListResponse(content(response))
    aircraftOpenSkiesFlights <- lapply(aircraftFlightsList, listToOpenSkiesFlight)
    if(includeStateVectors)
        for(i in 1:length(aircraftOpenSkiesFlights)){
            departureTime <- aircraftOpenSkiesFlights[[i]]$departure_time
            arrivalTime <- aircraftOpenSkiesFlights[[i]]$arrival_time
            stateVectors <- getAircraftStateVectorsSeries(aircraft, departureTime, arrivalTime, timeZone, timeResolution, username, password, useTrino)
            aircraftOpenSkiesFlights[[i]]$state_vectors <- stateVectors
        }
    if(includeAirportsMetadata){
        for(i in 1:length(aircraftOpenSkiesFlights)){
            originAirportICAO <- aircraftOpenSkiesFlights[[i]]$origin_airport
            destinationAirportICAO <- aircraftOpenSkiesFlights[[i]]$destination_airport
            if(!is.null(originAirportICAO)){
                originAirport <- getAirportMetadata(originAirportICAO)
                aircraftOpenSkiesFlights[[i]]$origin_airport <- originAirport
            }
            if(!is.null(destinationAirportICAO)){
                destinationAirport <- getAirportMetadata(destinationAirportICAO)
                aircraftOpenSkiesFlights[[i]]$destination_airport <- destinationAirport
            }
        }
    }
    return(aircraftOpenSkiesFlights)
}

getIntervalFlightsSingleInterval <- function(startTime, endTime, timeZone,
                                             username, password, includeStateVectors, 
                                             timeResolution, useTrino, 
                                             includeAirportsMetadata,
                                             timeOut, maxQueryAttempts, credentials = NULL) {
  query=list(begin=stringToEpochs(startTime, timeZone), end=stringToEpochs(endTime, timeZone))
  response <-makeAuthenticatedRequest("flights/all", query, credentials, timeOut, maxQueryAttempts)
  # FIXME catch NULL response if 404 no flights in interval
    intervalFlightsList <- formatFlightsListResponse(content(response))
    intervalOpenSkiesFlights <- lapply(intervalFlightsList, listToOpenSkiesFlight)
    if(includeStateVectors){
        for(i in 1:length(intervalOpenSkiesFlights)){
            departureTime <- intervalOpenSkiesFlights[[i]]$departure_time
            arrivalTime <- intervalOpenSkiesFlights[[i]]$arrival_time
            stateVectors <- getAircraftStateVectorsSeries(intervalOpenSkiesFlights[[i]]$ICAO24, departureTime, arrivalTime, timeZone, timeResolution, username, password, useTrino)
            intervalOpenSkiesFlights[[i]]$state_vectors <- stateVectors
        }
    }
    if(includeAirportsMetadata){
        for(i in 1:length(intervalOpenSkiesFlights)){
            originAirportICAO <- intervalOpenSkiesFlights[[i]]$origin_airport
            destinationAirportICAO <- intervalOpenSkiesFlights[[i]]$destination_airport
            if(!is.null(originAirportICAO)){
                originAirport <- getAirportMetadata(originAirportICAO)
                intervalOpenSkiesFlights[[i]]$origin_airport <- originAirport
            }
            if(!is.null(destinationAirportICAO)){
                destinationAirport <- getAirportMetadata(destinationAirportICAO)
                intervalOpenSkiesFlights[[i]]$destination_airport <- destinationAirport
            }
        }
    }
    return(intervalOpenSkiesFlights)
}
