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
  \item{\code{state_vectors}}{Object of class \code{\link{openSkiesStateVectorSet}}
  with field \code{time_series = TRUE} containing the state vectors received
  from the aircraft during the flight
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


\section{Methods}{

\describe{
  \item{
    \code{get_moment_state_vector(time, includeFuture = TRUE)}}{
     This method retrieves the state vector closest with the timestamp closest
     to the provided time, which must be supplied as a date-time string. In the 
     default behaviour, \code{includeFuture=TRUE} and the retrieved vector will 
     be the one with the closest timestamp, regardless of if this is earlier or
     later than the provided time. If \code{includeFuture=FALSE}, the closest
     earlier state vector will be retrieved.
   },
   \item{
    \code{get_duration()}}{
     This method returns the duration of the flight in seconds
   },
   \item{
    \code{distance_to_flight(flight, numberSamples=15, 
                             samplesAggregationMethod="concatenated", 
                             method="euclidean", additionalFields=NULL)}}{
     This method calculates the distance to the provided flight, which must be 
     another object of class openSkiesFlight. Both openSkiesFlight objects
     will be resampled to the number of points specified by 
     \code{numberSamples}. If \code{samplesAggregationMethod="concatenated"},
     a vector of values indicating the distance between the flights at each
     point is returned. If \code{samplesAggregationMethod="average"}, the
     average distance is returned. By default, \code{method="euclidean"} and
     euclidean distances are calculated. Other possible values of \code{method}
     are all values accepted by \code{\link{dist}}. By default, 
     \code{additionalFields=NULL}, and only latitude and longitude values will
     be included in the features vectors used to calculate distances. Additional
     fields can be specified by providing their names as a character vector
     through \code{additionalFields}. The names should be valid names of fields
     of \code{\link{openSkiesStateVector}} objects.
   }
   \item{
    \code{detect_phases(time_window, use_baro_altitude = FALSE)}}{
     This method detects the phases of the flight, applying the \code{\link{findFlightPhases}}
     function. A time window will be applied to calculate mean values of altitude,
     vertical rate and speed. Its length is provided in seconds through the 
     \code{time_window} argument. Setting this value to 1 effectively disables
     the usage of a time window. By default, \code{use_baro_altitude=FALSE} and
     geo altitude values will be used to calculate the flight phases. If 
     \code{use_baro_altitude=TRUE}, barometric altitude values will be used instead.
   }
}
}

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
