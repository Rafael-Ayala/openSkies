\name{openSkiesRoute}
\alias{openSkiesRoute}
\title{An \code{\link{R6Class}} object representing a flight route}
\description{
\code{\link{R6Class}} object representing a flight route, usually operated by
a commercial airline. Contains information about the callsign under which the 
route is operated, the operator itself and the airports of origin and 
destination. New instances can be manually created by providing values for at 
least the fields \code{call_sign},  \code{origin_airport} and  \code{destination_airport}. 
Alternatively, \code{\link{getRouteMetadata}} will return an \code{openSkiesRoute} 
object corresponding to the route with the provided callsign.
}
\usage{
openSkiesRoute
}
\section{Fields}{

\describe{
  \item{\code{call_sign}}{String with callsign of the route
  }
  \item{\code{origin_airport}}{String with the ICAO 4-letter code of the airport
  of origin
  }
  \item{\code{destination_airport}}{String with the ICAO 4-letter code of the 
  destination airport
  }
  \item{\code{operator_IATA}}{String with the IATA code for the operator of the 
  route
  }
  \item{\code{flight_number}}{String with the flight number for the route. The
  callsign is usually composed of an airline identifier and the flight number
  }
}}

\examples{

# Create an openSkiesRoute object corresponding to the American Airlines route
# with callsign AAL683

test_route <- getRouteMetadata("AAL683")
test_route
}
