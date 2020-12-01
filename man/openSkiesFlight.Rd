\name{openSkiesFlight}
\alias{openSkiesFlight}
\title{An \code{\link{R6Class}} object representing a specific flight}
\description{
\code{\link{R6Class}} object representing a specific flight performed by a
certain aircraft. Contains information about the aircraft that performed the
flight, the airports of origin and destination, the times of departure and
arrival and the callsign under which the flight was performed. New instances can be manually created by providing values for at least the fields \code{ICAO24},  \code{departure_time} and  \code{arrival_time}. 
Alternatively, \code{\link{getAircraftFlights}}, \code{\link{getAirportDepartures}},
\code{\link{getAirportArrivals}} \code{\link{getIntervalFlights}} will all 
return lists of \code{openSkiesFlight} objects corresponding to the flights
that match the query conditions.
}
\usage{
openSkiesFlight
}
\section{Fields}{

\describe{
  \item{\code{ICAO24}}{String with the ICAO 24-bit aicraft address associated to
  the aircraft in hexadecimal format
  }
  \item{\code{call_sign}}{String with callsign under which the flight was
  performed
  }
  \item{\code{origin_airport}}{String with the ICAO 4-letter code of the airport
  of origin
  }
  \item{\code{destination_airport}}{String with the ICAO 4-letter code of the 
  destination airport
  }
  \item{\code{departure_time}}{String with the date and time at which the 
  aircraft took off
  }
  \item{\code{arrival_time}}{String with the date and time at which the 
  aircraft arrived at its destination
  }
}}

\examples{

# Create a list of openSkiesFlight objects corresponding to all the flights that 
# landed at Frankfurt International Airport on the 29th of January, 2018 between 
# 12 PM and 1 PM

if(interactive()){
test_flights <- getAirportArrivals(airport="EDDF", startTime="2018-01-29 12:00:00", 
endTime="2018-01-29 13:00:00", timeZone="Europe/Berlin")
test_flights
}
}
