\name{openSkiesAircraft}
\alias{openSkiesAircraft}
\title{An \code{\link{R6Class}} object representing an aircraft}
\description{
\code{\link{R6Class}} object representing an aircraft. Contains information
about the ICAO 24-bit code of the aircraft, its registration code, its country
of origin, its manufacturer and its operator. New instances can be manually 
created by providing values for at least the \code{ICAO24} field. Alternatively, 
\code{\link{getAircraftMetadata}} will return an \code{openSkiesAirport} object 
corresponding to the airport with the provided ICAO 24-bit code.
}
\usage{
openSkiesAircraft
}
\section{Fields}{

\describe{
  \item{\code{ICAO24}}{String with the ICAO 24-bit aicraft address associated to
  the aircraft in hexadecimal format
  }
  \item{\code{registration}}{String with the aircraft registration code, also 
  called tail number
  }
  \item{\code{origin_country}}{String with the country where the aircraft is 
  registered
  }
  \item{\code{last_state_vector}}{An object of class \code{\link{openSkiesStateVector}}
  representing the last known state vector for the aircraft
  }
  \item{\code{state_vector_history}}{An object of class \code{\link{openSkiesStateVectorSet}}
  representing the history of all known state vectors for the aircraft
  }
  \item{\code{manufacturer_name}}{String with the name of the manufacturer of 
  the aircraft
  }
  \item{\code{manufacturer_ICAO}}{String with the ICAO code of the manufacturer 
  of the aircraft
  }
  \item{\code{model}}{String with the aicraft model
  }
  \item{\code{serial_number}}{String with the serial number of the aircraft
  }
  \item{\code{line_number}}{String with the line number of the aircraft.
  Usually only provided for Boeing aircrafts. Line numbers specify the order in 
  which airframes of a particular product line were assembled.
  }
  \item{\code{ICAO_type_code}}{String with the ICAO code for the model of 
  aircraft
  }
  \item{\code{ICAO_aircraft_class}}{String with the ICAO code for the type of 
  aircraft. ICAO aircraft classes provide more general groups than ICAO type 
  codes
  }
  \item{\code{owner}}{String with the name of the registered aircraft owner
  }
  \item{\code{operator}}{String with the name of the aircraft operator
  }
  \item{\code{operator_call_sign}}{String with the callsign of the aircraft 
  operator
  }
  \item{\code{operator_ICAO}}{String with the ICAO code of the aircraft operator
  }
  \item{\code{operator_IATA}}{String with the IATA code of the aircraft operator
  }
  \item{\code{first_flight_date}}{String with the date when the first flight 
  for the aircraft was registered. This information is usually not available
  when retrieving information from the OpenSky API
  }
  \item{\code{category_description}}{String with physical information about the 
  aircraft provided by the ADS-B emitter unit
  }
}}

\section{Methods}{

\describe{
  \item{
    \code{add_state_vector(state_vector)}}{
     This method adds a new state vector to the openSkiesAircraft object, which
     will be set as the new \code{last_state_vector} and will be added to
     \code{state_vector_history}. \code{state_vector} should be an
     \code{\link{openSkiesStateVector}} object
   }
}
}

\examples{

# Create an openSkiesAircraft object corresponding to the aircraft with
# ICAO 24-bit address 3922e2

test_aircraft <- getAircraftMetadata("3922e2")
test_aircraft
}
