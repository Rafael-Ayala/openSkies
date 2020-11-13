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
      cat("Location: ", self$city, ", ", self$country, "\n", sep ="")
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
    }
  )
)

openSkiesStateVectorSet <- R6Class(
  "openSkiesStateVectorSet",
  public = list(
    state_vectors = list(),
    time_series = logical(),
    initialize = function(state_vectors_list, time_series=FALSE) {
      stopifnot(all(sapply(state_vectors_list, class)) == "openSkiesStateVector",
                length(state_vectors_list < 1))
      self$state_vectors <- state_vectors_list
      self$time_series <- time_series
    },
    add_state_vector = function(state_vector) {
      self$state_vectors <- append(self$state_vectors, sate_vector)
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
    origin_airport = NULL, #Should be an openSkiesAirport object
    destination_airport = NULL, #Should be an openSkiesAirport object
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
      cat("Departing from ", self$origin_airport$name, "\n", sep ="")
      cat("Landing at  ", self$destination_airport$name, sep = "")
      invisible(self)
    }
  )
)

openSkiesFlight <- R6Class(
  "openSkiesFlight",
  public = list(
    ICAO24 = character(),
    call_sign = character(),
    origin_airport = NULL, #Should be an openSkiesAirport object
    destination_airport = NULL, #Should be an openSkiesAirport object
    departure_time = character(),
    arrival_time = character(),
    initialize = function(ICAO24,
                          call_sign = NULL,
                          origin_airport = NULL,
                          destination_airport = NULL,
                          departure_time,
                          arrival_time) {
      self$ICAO24 <- ICAO24
      self$call_sign <- call_sign
      self$origin_airport <- origin_airport
      self$destination_airport <- destination_airport
      self$departure_time <- departure_time
      self$arrival_time <- arrival_time
    },
    print = function(...) {
      cat("Flight performed by aircraft with ICAO 24-bit address ", self$ICAO24, "\n", sep = "")
      cat("Take-off time: ", self$departure_time, "\n", sep ="")
      cat("Landing time:  ", self$arrival_time, sep = "")
      invisible(self)
    }
  )
)


