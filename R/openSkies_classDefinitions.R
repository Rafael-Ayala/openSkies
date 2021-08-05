openSkiesAirport <- R6Class(
  "openSkiesAirport",
  public = list(
    name = character(),
    ICAO = character(),
    IATA = character(),
    longitude = double(),
    latitude = double(),
    altitude = double(),
    city = character(),
    municipality = character(),
    region = character(),
    country = character(),
    continent = character(),
    type = character(),
    website = character(),
    wikipedia_entry = character(),
    reliable_position = logical(),
    GPS_code = character(),
    initialize = function(name,
                          city,
                          country,
                          longitude,
                          latitude,
                          ICAO = NULL,
                          IATA = NULL,
                          altitude = NA,
                          municipality = NULL,
                          region = NULL,
                          continent = NULL,
                          type = NULL,
                          website = NULL,
                          wikipedia_entry = NULL,
                          reliable_position = TRUE,
                          GPS_code = NULL) {
      self$name <- name
      self$ICAO <- ICAO
      self$IATA <- IATA
      self$longitude <- longitude
      self$latitude <- latitude
      self$altitude <- altitude
      self$city <- city
      self$municipality <- municipality
      self$region <- region
      self$country <- country
      self$continent <- continent
      self$type <- type
      self$website <- website
      self$wikipedia_entry <- wikipedia_entry
      self$reliable_position <- reliable_position
      self$GPS_code <- GPS_code
    },
    print = function(...) {
      cat("Airport name: ", self$name, "\n", sep = "")
      if (!is.null(self$ICAO)) cat("ICAO code: ", self$ICAO, "\n", sep = "")
      # cat("Location: ", self$city, ", ", self$country, "\n", sep ="")
      cat("Country code: ", self$country, "\n", sep ="")
      cat("Geographic coordinates: latitude ", self$latitude, 
          " degrees, longitude ", self$longitude, " degrees", sep = "")
      invisible(self)
    }
  )
)

openSkiesStateVector <- R6Class(
  "openSkiesStateVector",
  public = list(
    ICAO24 = character(),
    call_sign = character(),
    origin_country = character(),
    requested_time = character(),
    last_position_update_time = character(),
    last_any_update_time = character(),
    longitude = double(),
    latitude = double(),
    baro_altitude = double(),
    geo_altitude = double(),
    on_ground = logical(),
    velocity = double(),
    true_track = double(),
    vertical_rate = double(),
    squawk = character(),
    special_purpose_indicator = logical(),
    position_source = character(),
    initialize = function(ICAO24, call_sign=NULL, origin_country=NULL, requested_time=NULL,
                          last_position_update_time=NULL, last_any_update_time=NULL,
                          longitude, latitude, baro_altitude=NULL, geo_altitude=NULL,
                          on_ground=NULL, velocity=NULL, true_track=NULL, vertical_rate=NULL,
                          squawk=NULL, special_purpose_indicator=FALSE, position_source=NULL) {
      self$ICAO24 = ICAO24
      self$call_sign = call_sign
      self$origin_country = origin_country
      self$requested_time = requested_time
      self$last_position_update_time = last_position_update_time
      self$last_any_update_time = last_any_update_time
      self$longitude = longitude
      self$latitude = latitude
      self$baro_altitude = baro_altitude
      self$geo_altitude = geo_altitude
      self$on_ground = on_ground
      self$velocity = velocity
      self$true_track = true_track
      self$vertical_rate = vertical_rate
      self$squawk = squawk
      self$special_purpose_indicator = special_purpose_indicator
      self$position_source = position_source
    }, 
    print = function(...) {
        cat("State vector for aircraft with ICAO24 ", self$ICAO24, "\n", sep = "")
        cat("Requested time: ", as.character(self$requested_time), " ", 
            Sys.timezone(), "\n", sep ="")
        cat("Last status update at: ", as.character(self$last_any_update_time), 
            " ", Sys.timezone(), "\n", sep ="")
        invisible(self)
    }
  )
)

openSkiesStateVectorSet <- R6Class(
  "openSkiesStateVectorSet",
  public = list(
    state_vectors = list(),
    time_series = logical(),
    segments_categories = NULL,
    initialize = function(state_vectors_list, time_series=FALSE, segments_categories=NULL) {
      stopifnot(all(sapply(state_vectors_list, class)[1,] == "openSkiesStateVector"),
                length(state_vectors_list) > 0)
      self$state_vectors <- state_vectors_list
      self$time_series <- time_series
      self$segments_categories <- segments_categories
    },
    add_state_vector = function(state_vector) {
      stopifnot(class(state_vector)[1] == "openSkiesStateVector")
      self$state_vectors <- append(self$state_vectors, state_vector)
      invisible(self)
    },
    # categorize_segments = function() {
    #   if(!self$time_series) {
    #     stop("This openSkiesStateVectorSet is not a time series", sep="")
    #   }
    #   categories <- vector("character", length=length(self$state_vectors))
    #   vertical_rates <- self$get_values("vertical_rate")
    #   geo_altitudes <- self$get_values("geo_altitude")
    #   vertical_rates[is.na(vertical_rates)] <- 0
    #   for (i in 1:length(self$state_vectors)) {
    #     if(abs(vertical_rates[i]) < 1) {
    #       categories[i] <- "Cruise"
    #     } else if(vertical_rates[i] > 0) {
    #       categories[i] <- "Take-off"
    #     } else if(vertical_rates[i] < 0) {
    #       categories[i] <- "Landing"
    #     }
    #   }
    #   self$segments_categories <- categories
    #   invisible(self)
    # },
    get_values = function(field, removeNAs=FALSE, unwrapAngles=FALSE) {
      if(!(field %in% c("ICAO24", "call_sign", "origin_country", "requested_time",
                        "last_position_update_time", "last_any_update_time",
                        "longitude", "latitude", "baro_altitude", "geo_altitude",
                        "on_ground", "velocity", "true_track", "vertical_rate",
                        "squawk", "special_purpose_indicator", "position_source"))){
        stop(paste(field, " is not a valid openSkiesStateVector field name", sep=""))
      }
      values <- lapply(as.list(self$state_vectors), "[[", field)
      if(removeNAs){
        values <- values[sapply(values, function(x) length(x)!=0L)]
      } else {
        values[sapply(values, function(x) length(x)==0L)] <- NA
      }
      values <- unlist(values)
      if(field %in% c("true_track")){
        if(unwrapAngles){
          values <- unwrapAngles(values)
        }
      }
      return(values)
    },
    # get_differentials = function(field, removeNAs=FALSE, unwrapAngles) {
    #   if(!(field %in% c("requested_time", "last_position_update_time", "last_any_update_time",
    #                     "longitude", "latitude", "baro_altitude", "geo_altitude",
    #                     "on_ground", "velocity", "true_track", "vertical_rate"))){
    #     stop(paste(field, " is not a valid numeric openSkiesStateVector field name", sep=""))
    #   }
    #   diffs <- NULL
    #   values <- self$get_values(field, removeNAs, unwrapAngles)
    #   for(i in 2:length(values)){
    #     diff <- values[[i]] - values[[i-1]]
    #     diffs <- c(diffs, diff)
    #   }
    #   return(diffs)
    # },
    get_uniform_interpolation = function(n, fields, method="fmm") {
      result <- NULL
      for(field in fields){
        if(!(field %in% c("requested_time", "last_position_update_time", "last_any_update_time",
                          "longitude", "latitude", "baro_altitude", "geo_altitude",
                          "on_ground", "velocity", "true_track", "vertical_rate"))){
          stop(paste(field, " is not a valid numeric openSkiesStateVector field name", sep=""))
        }
        values <- self$get_values(field, removeNAs=TRUE, unwrapAngles=TRUE)
        if(grepl("time", field)){
          values <- unlist(lapply(values, function(t) as.POSIXct(t, origin="1970-1-1", tz = Sys.timezone())))
        }
        if(method == "linear"){
          values <- approx(values, n=n)$y
        } else {
          values <- spline(values, method=method, n=n)$y
        }
        if(is.null(result)){
          result <- data.frame(values)
          colnames(result)[1] <- field
        } else {
          result[[field]] <- values
        }
      }
      return(result)
    },
    get_time_points_interpolation = function(fields, time_field, timestamps, method="fmm") {
       result <- NULL
       times <- self$get_values(time_field, removeNAs=TRUE, unwrapAngles=TRUE)
       times <- unlist(lapply(times, function(t) as.POSIXct(t, origin="1970-1-1", tz = Sys.timezone())))
       for(field in fields){
         if(!(field %in% c("requested_time", "last_position_update_time", "last_any_update_time",
                           "longitude", "latitude", "baro_altitude", "geo_altitude",
                           "on_ground", "velocity", "true_track", "vertical_rate"))){
           stop(paste(field, " is not a valid numeric openSkiesStateVector field name", sep=""))
         }
         values <- self$get_values(field, removeNAs=TRUE)
         if(grepl("time", field)){
           values <- unlist(lapply(values, function(t) as.POSIXct(t, origin="1970-1-1", tz = Sys.timezone())))
         }
         if(method == "linear"){
           fun <- approxfun(times, values)
         } else {
           fun <- splinefun(times, values, method=method)
         }
         values = fun(timestamps)
         if(is.null(result)){
           result <- data.frame(values)
           colnames(result)[1] <- field
         } else {
           result[[field]] <- values
         }
       }
       return(result)
    },
    sort_by_field = function(field, decreasing=FALSE){
      self$state_vectors <- self$state_vectors[order(self$get_values(field,removeNAs = TRUE), 
                                                     decreasing=decreasing)]
      invisible(self)
    },
    split_into_flights = function(timeOnLandThreshold = 300, timeDiffThreshold = 1800){
      flights = list()
      aircraft_groups = groupByFunction(self$state_vectors, function(x) x$ICAO24)
      for(group in aircraft_groups) {
        vectorSet = openSkiesStateVectorSet$new(
          state_vectors_list = group,
          time_series = TRUE
        )
        vectorSet$sort_by_field("last_any_update_time")
        timeDiffs = c(0, diff(vectorSet$get_values("last_any_update_time")))
        firstFlightIndex = 1
        timeOnLand = 0
        firstOnAirFlag = FALSE
        firstOnAirIndex = 0
        lastOnAirFlag = FALSE
        lastOnAirIndex = 0
        for(i in 1:length(vectorSet$state_vectors)) {
          stateVector = vectorSet$state_vectors[[i]]
          if(!firstOnAirFlag & !stateVector$on_ground) {
            firstOnAirFlag = TRUE
            firstOnAirIndex = i
            accGroundTime = 0
            for(j in i:firstFlightIndex) {
              accGroundTime = accGroundTime + timeDiffs[j]
              if(accGroundTime >= timeOnLandThreshold) {
                firstFlightIndex = j
              }
            }
          }
          if(!lastOnAirFlag & firstOnAirFlag & stateVector$on_ground) {
            lastOnAirFlag = TRUE
            lastOnAirIndex = i-1
          }
          if(stateVector$on_ground) {
            timeOnLand = timeOnLand + timeDiffs[i]
          } else {
            timeOnLand = 0
            lastOnAirFlag = FALSE
          }
          if((timeOnLand >= timeOnLandThreshold & firstOnAirFlag) | i==length(vectorSet$state_vectors) | timeDiffs[i] >= timeDiffThreshold) {
            if(timeDiffs[i] >= timeDiffThreshold) {
              lastOnAirIndex = i-1
              i = i-1
            }
            flightStateVectors = openSkiesStateVectorSet$new(
              state_vectors_list = vectorSet$state_vectors[firstFlightIndex:i],
              time_series = TRUE
            )
            departureTime = as.character(as.POSIXct(vectorSet$get_values("last_any_update_time")[firstOnAirIndex], origin="1970-01-01", tz=Sys.timezone()))
            arrivalTime = if(lastOnAirIndex==0 | lastOnAirIndex < firstOnAirIndex) {
              "Unknown" } else {
                as.character(as.POSIXct(vectorSet$get_values("last_any_update_time")[lastOnAirIndex], origin="1970-01-01", tz=Sys.timezone()))
              }
            flight = openSkiesFlight$new(
              ICAO24 = stateVector$ICAO24,
              call_sign = stateVector$call_sign,
              state_vectors = flightStateVectors,
              departure_time = departureTime,
              arrival_time = arrivalTime
            )
            flights = append(flights, flight)
            timeOnLand = 0
            firstOnAirFlag = FALSE
            lastOnAirFlag = FALSE
            if(timeDiffs[i] < timeDiffThreshold){
              firstFlightIndex = i+1
            }
          }
        }
      }
      return(flights)
    }, 
    print = function(...) {
      cat("State vector set with ", length(self$state_vectors), " state vectors\n", sep = "")
      cat("Requested time: ", as.character(self$requested_time), " ", 
          Sys.timezone(), "\n", sep ="")
      cat("Last status update at: ", as.character(self$last_any_update_time), 
          " ", Sys.timezone(), "\n", sep ="")
      invisible(self)
    }
  )
)

openSkiesAircraft <- R6Class(
  "openSkiesAircraft",
  public = list(
    ICAO24 = character(),
    registration = character(),
    origin_country = character(),
    last_state_vector = NULL, #Should be an openSkiesStateVector object
    state_vector_history = NULL, #Should be an openSkiesStateVectorSet object with time_series=TRUE
    manufacturer_name = character(),
    manufacturer_ICAO = character(),
    model = character(),
    serial_number = character(),
    line_number = character(),
    ICAO_type_code = character(),
    ICAO_aircraft_class = character(),
    owner = character(),
    operator = character(),
    operator_call_sign = character(),
    operator_ICAO = character(),
    operator_IATA = character(),
    first_flight_date = character(),
    category_description = character(),
    initialize = function(ICAO24,
                          registration = NULL,
                          origin_country = NULL,
                          last_state_vector = NULL, 
                          state_vector_history = NULL,
                          manufacturer_name = NULL,
                          manufacturer_ICAO = NULL,
                          model = NULL,
                          serial_number = NULL,
                          line_number = NULL,
                          ICAO_type_code = NULL,
                          ICAO_aircraft_class = NULL,
                          owner = NULL,
                          operator = NULL,
                          operator_call_sign = NULL,
                          operator_ICAO = NULL,
                          operator_IATA = NULL,
                          first_flight_date = NULL,
                          category_description = NULL) {
      self$ICAO24 <- ICAO24
      self$registration <- registration
      self$origin_country <- origin_country
      self$last_state_vector <- last_state_vector
      self$state_vector_history <- state_vector_history
      self$manufacturer_name <- manufacturer_name
      self$manufacturer_ICAO <- manufacturer_ICAO
      self$model <- model
      self$serial_number <- serial_number
      self$line_number <- line_number
      self$ICAO_type_code <- ICAO_type_code
      self$ICAO_aircraft_class <- ICAO_aircraft_class
      self$owner <- owner
      self$operator <- operator
      self$operator_call_sign <- operator_call_sign
      self$operator_ICAO <- operator_ICAO
      self$operator_IATA <- operator_IATA
      self$first_flight_date <- first_flight_date
      self$category_description <- category_description
    },
    print = function(...) {
      cat("Aircraft with ICAO 24-bit address ", self$ICAO24, "\n", sep = "")
      if (!is.null(self$registration)) cat("Registration code ", self$registration, "\n", sep = "")
      invisible(self)
    },
    add_state_vector = function(state_vector) {
      self$last_state_vector <- state_vector
      self$state_vector_history$add_state_vector(state_vector)
      invisible(self)
    }
  )
)

openSkiesRoute <- R6Class(
  "openSkiesRoute",
  public = list(
    call_sign = character(),
    origin_airport = character(),
    destination_airport = character(), 
    operator_IATA = character(),
    flight_number = character(),
    initialize = function(call_sign,
                          origin_airport,
                          destination_airport,
                          operator_IATA = NULL,
                          flight_number = NULL) {
      self$call_sign <- call_sign
      self$origin_airport <- origin_airport
      self$destination_airport <- destination_airport
      self$operator_IATA <- operator_IATA
      self$flight_number <- flight_number
    },
    print = function(...) {
      cat("Flight route with call sign ", self$call_sign, "\n", sep = "")
      cat("Departing from airport with ICAO code ", self$origin_airport, "\n", sep ="")
      cat("Landing at airport with ICAO code ", self$destination_airport, sep = "")
      invisible(self)
    }
  )
)

openSkiesFlight <- R6Class(
  "openSkiesFlight",
  public = list(
    ICAO24 = character(),
    call_sign = character(),
    state_vectors = NULL,
    origin_airport = NULL, #Could be an openSkiesAirport object (future)
    destination_airport = NULL, #Could be an openSkiesAirport object (future)
    departure_time = character(),
    arrival_time = character(),
    initialize = function(ICAO24,
                          call_sign = NULL,
                          state_vectors = NULL,
                          origin_airport = NULL,
                          destination_airport = NULL,
                          departure_time,
                          arrival_time
                          ) {
      self$ICAO24 <- ICAO24
      self$call_sign <- call_sign
      self$origin_airport <- origin_airport
      self$destination_airport <- destination_airport
      self$departure_time <- departure_time
      self$arrival_time <- arrival_time
      self$state_vectors <- state_vectors
    },
    get_moment_state_vector = function(time, includeFuture = TRUE){
      nearest <- NULL
      smallestDifference <- NULL
      for(stateVector in self$state_vectors$state_vectors){
        if(!is.null(stateVector$last_any_update_time)){
          difference <- time - stateVector$last_any_update_time
          if(is.null(nearest)){
            nearest <- stateVector
            smallestDifference <- difference
          } else {
            if(difference>0 || includeFuture){
              if(abs(difference) < smallestDifference){
                nearest <- stateVector
                smallestDifference <- abs(difference)
              }
            }
          }
        }
      }
      return(nearest)  
    },
    get_duration = function(){
      departureTipe <- as.POSIXct(self$departure_time, origin="1970-1-1", tz = Sys.timezone())
      arrivalTime <- as.POSIXct(self$arrival_time, origin="1970-1-1", tz = Sys.timezone())
      duration <- arrivalTime - departureTime
      return(duration)
    },
    distance_to_flight = function(flight, numberSamples=15, samplesAggregationMethod="concatenated", method="euclidean", useAngles=FALSE){
      if(!(samplesAggregationMethod %in% c("concatenated", "average"))){
        stop(paste(samplesAggregationMethod, " is not a valid aggregation method.", sep=""))
      }
      features1 = getVectorSetFeatures(self$state_vectors, resamplingSize=numberSamples, useAngles=useAngles)
      features2 = getVectorSetFeatures(flight$state_vectors, resamplingSize=numberSamples, useAngles=useAngles)
      if(samplesAggregationMethod == "concatenated"){
        distance = dist(rbind(features1, features2), method)
      } else if(samplesAggregationMethod == "average"){
        if(useAngles){
          numFeatures = 3
        } else {
          numFeatures = 2
        }
        numPoints = length(features1)/numFeatures
        distancesSum = 0
        for (i in 1:numPoints) {
          point1 = features1[((i-1)*numFeatures+1):(i*numFeatures)]
          point2 = features2[((i-1)*numFeatures+1):(i*numFeatures)]
          distancesSum = distancesSum + dist(rbind(point1, point2))[[1]]
        }
        distance = distancesSum / numPoints
      }
      return(distance)
    },
    print = function(...) {
      cat("Flight performed by aircraft with ICAO 24-bit address ", self$ICAO24, "\n", sep = "")
      cat("Take-off time: ", as.character(self$departure_time), " ", Sys.timezone(), "\n", sep ="")
      cat("Landing time:  ", as.character(self$arrival_time), " ", if(as.character(self$arrival_time) != "Unknown") {Sys.timezone()}, sep = "")
      invisible(self)
    }
  )
)

# FUTURE
# openSkiesAirspace <- R6Class(
#   "openSkiesAirspace",
#   public = list(
#     max_latitude = double(),
#     min_latitude = double(),
#     max_longitude = double(),
#     min_longitude = double(),
#     current_aircrafts = list(), 
#     flights = list(),
#     initialize = function(max_latitude,
#                           min_latitude,
#                           max_longitude,
#                           min_longitude,
#                           current_aircrafts = NULL, 
#                           flights = NULL) {
#       self$max_latitude <- max_latitude
#       self$min_latitude <- min_latitude
#       self$max_longitude <- max_longitude
#       self$min_longitude <- min_longitude
#       self$current_aircrafts <- current_aircrafts
#       self$flights <- flights
#     },
#     print = function(...) {
#       cat("Airspace contained between latitudes ", self$min_latitude, " and ", self$max_latitude, ", \n", sep = "")
#       cat("longitudes ", self$min_longitude, " and ", self$max_longitude, "\n", sep ="")
#       cat("The airspace contains  ", length(self$current_planes), " planes \n", sep = "")
#       cat(length(self$flights), " flights logged over the airspace", sep="")
#       invisible(self)
#     }
#   )
# )
