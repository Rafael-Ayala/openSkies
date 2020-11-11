openSkiesAirport <- R6Class("openSkiesAirport", 
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
                              initialize = function(name, city, country, longitude, 
                                                    latitude, ICAO=NULL, IATA=NULL, 
                                                    altitude=NA, municipality=NULL,
                                                    region=NULL, continent=NULL,
                                                    type=NULL, website=NULL,
                                                    wikipedia_entry=NULL, reliable_position=TRUE,
                                                    GPS_code=NULL) {
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
                                private$reliable_position <- reliable_position
                                private$GPS_code <- GPS_code
                              }, 
                              print = function(...) {
                                cat("Airport name: ", self$name, "\n", sep="")
                                if(!is.null(self$ICAO)) cat("ICAO code: ", self$ICAO, "\n", sep="")
                                cat("Location: ", self$city, ", ", self$country, "\n", sep="")
                                cat("Geographic coordinates: latitude ", self$latitude, " degrees, longitude ",
                                    self$longitude, " degrees", sep="")
                              }), 
                            private = list(
                              reliable_position = logical(),
                              GPS_code = character())
                            )


