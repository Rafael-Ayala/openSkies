# TODO implement authentication using client_credentials, see openSkies_auth.R, openSkies_otherSupportFunctions.R
getSingleTimeStateVectors <- function(aircraft=NULL, time=NULL, timeZone=Sys.timezone(),
                                      minLatitude=NULL, maxLatitude=NULL, minLongitude=NULL,
                                      maxLongitude=NULL, username=NULL, password=NULL, 
                                      useTrino=FALSE, timeOut=60, maxQueryAttempts=1) {
  if(!is.null(aircraft)) {
    checkICAO24(aircraft)
  }
  if(!is.null(time)){
    checkTime(time)
  } 
  if(!is.null(minLatitude)) {
    checkCoordinate(minLatitude, "minimum latitude")
  }
  if(!is.null(maxLatitude)) {
    checkCoordinate(maxLatitude, "maximum latitude")
  }
  if(!is.null(minLongitude)) {
    checkCoordinate(minLongitude, "minimum longitude")
  }
  if(!is.null(maxLongitude)) {
    checkCoordinate(maxLongitude, "maximum longitude")
  }
  if(is.null(username) | is.null(password)) {
    if(!is.null(time)) {
      if(is.null(aircraft)) {
        time <- NULL
        warning(strwrap("Timepoint specified but no login provided and no aircraft
                        specified. Specified time will be ignored and data for current
                        time will be retrieved.", initial="", prefix="\n"))
      } else if(length(aircraft) > 1) {
        time <- NULL
        warning(strwrap("Timepoint specified but no login provided and several aircrafts
                        specified. Specified time will be ignored and data for current
                        time will be retrieved.", initial="", prefix="\n"))
      }
    }
  } else if((!is.null(time)) & !useTrino) {
    if(secondsFromCurrentTime(time, timeZone) >= 3600) {
      if(is.null(aircraft) | length(aircraft) > 1) {
        stop(strwrap("Historical data for multiple aircrafts older than 1 hour ago
                     cannot be retrieved. Please specify a single aircraft or
                     request a timepoint not older than 1 hour ago.", initial="",
                     prefix="\n"))
      }
    }
  }
  if(useTrino){
    if(is.null(time)) {
      time <- Sys.time()
    }
    token <- getTrinoToken(username, password)
    connection <- getTrinoConnection(username, token)
    query <- makeImpalaQueryStateVectorsSingleTime(aircraft, time, timeZone, minLatitude, maxLatitude, minLongitude, maxLongitude)
    results <- runTrinoQuery(query, connection)
    dbDisconnect(connection)
    if(is.null(results)) {
      message(strwrap("Query to Trino database did not yield any results.", 
                      initial="", prefix="\n"))
      return(NULL)
    }
    parsedResults <- formatStateVectorsResponseImpala(results)
    openSkiesStateVectorsList <- lapply(parsedResults, listToOpenSkiesStateVector)
    if(length(openSkiesStateVectorsList)>1){
      openSkiesStateVectorsResult <- openSkiesStateVectorsResult <- openSkiesStateVectorSet$new(
        state_vectors = openSkiesStateVectorsList)
    } else {
      openSkiesStateVectorsResult <- openSkiesStateVectorsList[[1]]
    }
    return(openSkiesStateVectorsResult)
    
  }else{
    queryParameters <- c(makeAircraftsQueryList(aircraft),
                         time=if(!is.null(time)) stringToEpochs(time, timeZone) else NULL,
                         lamin=minLatitude, lomin=minLongitude,
                         lamax=maxLatitude, lomax=maxLongitude)
    queryParameters <- as.list(queryParameters[lengths(queryParameters) != 0])
    jsonResponse <- FALSE
    attemptCount <- 0
    while(!jsonResponse) {
      attemptCount <- attemptCount + 1
      response <- tryCatch({
        GET(paste(openskyApiRootURL, "states/all", sep="" ),
            query=queryParameters,
            timeout(timeOut),
            if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
      },
      error = function(e) e
      )
      if(inherits(response, "error")) {
        message(strwrap("Resource not currently available. Please try again 
                         later.", initial="", prefix="\n"))
        return(NULL)
      }
      jsonResponse <- grepl("json", headers(response)$`content-type`)
      if(attemptCount > maxQueryAttempts) {
        message(strwrap("Resource not currently available. Please try again 
                         later.", initial="", prefix="\n"))
        return(NULL)
      }
    }
    if(status_code(response) != 200 || is.null(content(response)$states)) {
        if(status_code(response) == 500) {
            message(strwrap("Resource not currently available. Please try again 
                         later.", initial="", prefix="\n"))
            return(NULL)
        } else {
            message(strwrap("No state vectors found for the specified aircrafts, 
                       location and interval."), initial="", prefix="\n")
            return(NULL)
        }
    } 
    formattedStateVectorsList <- formatStateVectorsResponse(content(response))
    openSkiesStateVectorsList <- lapply(formattedStateVectorsList, listToOpenSkiesStateVector)
    if(length(openSkiesStateVectorsList) > 1){
      openSkiesStateVectorsResult <- openSkiesStateVectorSet$new(
        state_vectors = openSkiesStateVectorsList)
    } else if(length(openSkiesStateVectorsList) == 1) {
      openSkiesStateVectorsResult <- openSkiesStateVectorsList[[1]]
    }
    return(openSkiesStateVectorsResult)
  }
}

getIntervalStateVectors <- function(aircraft=NULL, startTime, endTime,
                                    timeZone=Sys.timezone(), minLatitude=NULL, 
                                    maxLatitude=NULL, minLongitude=NULL, maxLongitude=NULL,
                                    minBaroAltitude=NULL, maxBaroAltitude=NULL,
                                    minGeoAltitude=NULL, maxGeoAltitude=NULL,
                                    minVelocity=NULL, maxVelocity=NULL,
                                    minVerticalRate=NULL, maxVerticalRate=NULL,
                                    callSignFilter=NULL, onGroundStatus=NULL,
                                    squawkFilter=NULL, spiStatus=NULL, alertStatus=NULL,
                                    username, password) {
  if(!is.null(aircraft)) {
    checkICAO24(aircraft)
  }
  checkTime(startTime)
  checkTime(endTime)
  if(!is.null(minLatitude)) {
    checkCoordinate(minLatitude, "minimum latitude")
  }
  if(!is.null(maxLatitude)) {
    checkCoordinate(maxLatitude, "maximum latitude")
  }
  if(!is.null(minLongitude)) {
    checkCoordinate(minLongitude, "minimum longitude")
  }
  if(!is.null(maxLongitude)) {
    checkCoordinate(maxLongitude, "maximum longitude")
  }
  if(!is.null(minBaroAltitude) & !is.numeric(minBaroAltitude)) {
    stop("Invalid minimum barometric altitude value provided")
  }
  if(!is.null(maxBaroAltitude) & !is.numeric(maxBaroAltitude)) {
    stop("Invalid maximum barometric altitude value provided")
  }
  if(!is.null(minGeoAltitude) & !is.numeric(minGeoAltitude)) {
    stop("Invalid minimum geometric altitude value provided")
  }
  if(!is.null(maxGeoAltitude) & !is.numeric(maxGeoAltitude)) {
    stop("Invalid maximum geometric altitude value provided")
  }
  if(!is.null(minVelocity) & !is.numeric(minVelocity)) {
    stop("Invalid minimum velocity value provided")
  }
  if(!is.null(maxVelocity) & !is.numeric(maxVelocity)) {
    stop("Invalid maximum velocity value provided")
  }
  if(!is.null(minVerticalRate) & !is.numeric(minVerticalRate)) {
    stop("Invalid minimum vertical rate value provided")
  }
  if(!is.null(maxVerticalRate) & !is.numeric(maxVerticalRate)) {
    stop("Invalid maximum vertical rate value provided")
  }
  if(!is.null(callSignFilter) & !is.character(maxVerticalRate)) {
    stop("Invalid callsign provided. Callsign should be provided as a string")
  }
  if(!is.null(onGroundStatus) & !is.logical(onGroundStatus)) {
    stop("Invalid on ground status provided. Please provide a NULL, TRUE or FALSE value")
  }
  if(!is.null(squawkFilter)) {
    checkSquawk(squawkFilter)
  }
  if(!is.null(spiStatus) & !is.logical(spiStatus)) {
    stop("Invalid SPI status provided. Please provide a NULL, TRUE or FALSE value")
  }
  if(!is.null(alertStatus) & !is.logical(alertStatus)) {
    stop("Invalid alert status provided. Please provide a NULL, TRUE or FALSE value")
  }
  token <- getTrinoToken(username, password)
  connection <- getTrinoConnection(username, token)
  query <- makeImpalaQueryStateVectorsInterval(aircraft, startTime, endTime,
                                               timeZone, minLatitude, maxLatitude, 
                                               minLongitude, maxLongitude,
                                               minBaroAltitude, maxBaroAltitude,
                                               minGeoAltitude, maxGeoAltitude,
                                               minVelocity, maxVelocity,
                                               minVerticalRate, maxVerticalRate,
                                               callSignFilter, onGroundStatus,
                                               squawkFilter, spiStatus, alertStatus)
  results <- runTrinoQuery(query, connection)
  dbDisconnect(connection)
  if(is.null(results)) {
    message(strwrap("Query to Trino database did not yield any results.", 
                    initial="", prefix="\n"))
    return(NULL)
  }
  parsedResults <- formatStateVectorsResponseImpala(results)
  openSkiesStateVectorsList <- lapply(parsedResults, listToOpenSkiesStateVector)
  if(length(openSkiesStateVectorsList)>1){
    openSkiesStateVectorsResult <- openSkiesStateVectorsResult <- openSkiesStateVectorSet$new(
      state_vectors = openSkiesStateVectorsList)
  } else {
    openSkiesStateVectorsResult <- openSkiesStateVectorsList[[1]]
  }
  return(openSkiesStateVectorsResult)
}

getAircraftStateVectorsSeries <- function(aircraft, startTime, endTime, timeZone=Sys.timezone(),
                                          timeResolution, username=NULL, password=NULL, 
                                          useTrino=FALSE, timeOut=60, maxQueryAttempts=1) {
  if(timeResolution < 10) {
    if(is.null(username) | is.null(password)) {
      timeResolution <- 10
      warning(strwrap("Anonymous users cannot retrieve data with a time resolution
                      higher than 10 seconds. A time resolution of 10 seconds will be used.",
                      initial="", prefix="\n"))
    } else if(timeResolution < 5) {
      timeResolution <- 5
      warning(strwrap("Data with a time resolution higher than 5 seconds cannot
                      be retrieved. A time resolution of 5 seconds will be used.",
                      initial="", prefix="\n"))
    }
  }
  timePoints <- generateTimePoints(startTime, endTime, timeZone, timeResolution)
  if(useTrino){
    token <- getTrinoToken(username, password)
    connection <- getTrinoConnection(username, token)
    query <- makeImpalaQueryStateVectorsTimeSeries(aircraft, timePoints)
    results <- runTrinoQuery(query, connection)
    dbDisconnect(connection)
    if(is.null(results)) {
      message(strwrap("Query to Trino database did not yield any results.", 
                      initial="", prefix="\n"))
      return(NULL)
    }
    parsedResults <- formatStateVectorsResponseImpala(results)
    openSkiesStateVectorsList <- lapply(parsedResults, listToOpenSkiesStateVector)
    if(length(openSkiesStateVectorsList) < length(timePoints)){
      message(strwrap("No state vectors found for part of the specified 
                         interval."), initial="", prefix="\n")
    }
    openSkiesStateVectorsSeries <- openSkiesStateVectorSet$new(
      state_vectors = openSkiesStateVectorsList,
      time_series = TRUE)
    return(openSkiesStateVectorsSeries)
  } else {
    stateVectorsSeries <- vector(mode="list", length=length(timePoints))
    for(i in seq_len(length(timePoints))) {
      jsonResponse <- FALSE
      attemptCount <- 0
      while(!jsonResponse) {
        attemptCount <- attemptCount + 1
        response <- tryCatch({
          GET(paste(openskyApiRootURL, "states/all", sep="" ),
              query=list(icao24=aircraft,
                         time=timePoints[i]),
              timeout(timeOut),
              if (!(is.null(username) | is.null(password))) {authenticate(username, password)})
        },
        error = function(e) e
        )
        if(inherits(response, "error")) {
          message(strwrap("Resource not currently available. Please try again 
                         later.", initial="", prefix="\n"))
          return(NULL)
        }
        jsonResponse <- grepl("json", headers(response)$`content-type`)
        if(attemptCount > maxQueryAttempts) {
          message(strwrap("Resource not currently available. Please try again 
                         later.", initial="", prefix="\n"))
          return(NULL)
        }
      }
      if(status_code(response) != 200 || is.null(content(response)$states)) {
          if(status_code(response) == 500) {
              message(strwrap("Resource not currently available. Please try again 
                         later.", initial="", prefix="\n"))
          } else {
              message(strwrap("No state vectors found for part of the specified 
                         interval."), initial="", prefix="\n")
          }
          stateVectorsSeries[[i]] <- NULL
          next
      }
      stateVectorsSeries[[i]] <- listToOpenSkiesStateVector(unlist(formatStateVectorsResponse(content(response)), recursive=FALSE))
    }
    openSkiesStateVectorsSeries <- openSkiesStateVectorSet$new(
      state_vectors = stateVectorsSeries,
      time_series = TRUE)
    return(openSkiesStateVectorsSeries)
  }
}
