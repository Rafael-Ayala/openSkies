\name{openSkiesStateVector}
\alias{openSkiesStateVector}
\title{An \code{\link{R6Class}} object representing an aircraft state vector}
\description{
\code{\link{R6Class}} object representing an aircraft state vector. Contains 
information about status at a given timepoint of an aircraft, including
its position, altitude and velocity. New instances can be manually 
created by providing values for at least the \code{ICAO24}, \code{longitude} and
\code{latitude} fields. Alternatively, \code{\link{getSingleTimeStateVectors}} 
will return an \code{openSkiesStateVector} object if a single state vector 
matching the query conditions is found.
}
\usage{
openSkiesStateVector
}
\section{Fields}{

\describe{
  \item{\code{ICAO24}}{String with the ICAO 24-bit aicraft address associated to
  the aircraft in hexadecimal format
  }
  \item{\code{call_sign}}{String with the callsign assigned to the aircraft
  }
  \item{\code{origin_country}}{String with the country where the aircraft is 
  registered
  }
  \item{\code{requested_time}}{String with the time point for which the state
  vector was requested
  }
  \item{\code{last_position_update_time}}{String with the time at which the last
  position update for the aircraft was received, or NULL if no position update 
  had been received in the past 15 s
  }
  \item{\code{last_any_update_time}}{String with the time at which the last 
  update (of any type) for the aircraft was received
  }
  \item{\code{longitude}}{Longitude value for the position of the aircraft
  }
  \item{\code{latitude}}{Latitude value for the position of the aircraft
  }
  \item{\code{baro_altitude}}{Barometric altitude of the aircraft in meters
  }
  \item{\code{geo_altitude}}{Geometric altitude of the aircraft in meters
  }
  \item{\code{on_ground}}{Logical indicating if the aircraft is at a surface 
  position
  }
  \item{\code{velocity}}{velocity of the aircraft over the ground in 
  meters/second
  }
  \item{\code{true_track}}{True track angle in degrees of the current aircraft 
  course. Measured clockwise from the North (0º)
  }
  \item{\code{vertical_rate}}{Vertical movement rate of the aircraft in 
  meters/second. Positive means the aircraft is climbing, and negative means it
  is descending
  }
  \item{\code{squawk}}{String with the squawk code for the aircraft transponder
  }
  \item{\code{special_purpose_indicator}}{Logical indicating if the transponder 
  of the aircraft has emitted a Special Purpose Indicator pulse
  }
  \item{\code{position_source}}{String with the source of the position 
  information for this state vector. Can be ADS-B (Automatic Dependent 
  Surveillance–Broadcast), ASTERIX (All Purpose Structured Eurocontrol 
  Surveillance Information Exchange) or MLA (Multilateration)
  }
}}

\examples{

# Obtain the state vector for aircraft with ICAO 24-bit address 403003 for
# the 8th of October, 2020 at 16:50 London time.

if(interactive()){
test_stateVector <- getSingleTimeStateVectors(aircraft="403003", 
time="2020-10-08 16:50:00", timeZone="Europe/London")
test_stateVector
}
}
