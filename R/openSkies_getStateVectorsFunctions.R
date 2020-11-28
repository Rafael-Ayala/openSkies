getSingleTimeStateVectors <- function(aircraft=NULL, time=NULL, timeZone=Sys.timezone(),
                                      minLatitude=NULL, maxLatitude=NULL, minLongitude=NULL,
                                      maxLongitude=NULL, username=NULL, password=NULL) {
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
  } else if(!is.null(time)) {
    if(secondsFromCurrentTime(time, timeZone) >= 3600) {
      if(is.null(aircraft) | length(aircraft) > 1) {
        stop(strwrap("Historical data for multiple aircrafts older than 1 hour ago
                     cannot be retrieved. Please specify a single aircraft or
                     request a timepoint not older than 1 hour ago.", initial="",
                     prefix="\n"))
      }
    }
  }
  queryParameters <- c(makeAircraftsQueryList(aircraft),
                       time=if(!is.null(time)) stringToEpochs(time, timeZone) else NULL,
                       lamin=minLatitude, lomin=minLongitude,
                       lamax=maxLatitude, lomax=maxLongitude)
  queryParameters <- as.list(queryParameters[lengths(queryParameters) != 0])
  jsonResponse <- FALSE
  attemptCount <- 0
  while(!jsonResponse) {
    response <- tryCatch({
      GET(paste(openskyApiRootURL, "states/all", sep="" ),
          query=queryParameters,
          timeout(300),
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
    if(attemptCount > 100) {
      message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
      return(NULL)
    }
  }
  if(status_code(response) != 200) {
    message(strwrap("No state vectors found for the specified aircrafts, 
                     location and interval."), initial="", prefix="\n")
    return(NULL)
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

getAircraftStateVectorsSeries <- function(aircraft, startTime, endTime, timeZone=Sys.timezone(),
                                          timeResolution, username=NULL, password=NULL) {
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
  stateVectorsSeries <- vector(mode="list", length=length(timePoints))
  for(i in seq_len(length(timePoints))) {
    jsonResponse <- FALSE
    attemptCount <- 0
    while(!jsonResponse) {
      response <- tryCatch({
        GET(paste(openskyApiRootURL, "states/all", sep="" ),
            query=list(icao24=aircraft,
                       time=timePoints[i]),
            timeout(300),
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
      if(attemptCount > 100) {
        message(strwrap("Resource not currently available. Please try again 
                       later.", initial="", prefix="\n"))
        return(NULL)
      }
    }
    if(status_code(response) != 200) {
      message(strwrap("No state vectors found for part of the specified 
                       interval."), initial="", prefix="\n")
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
