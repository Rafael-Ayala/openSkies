\name{getFlightMetadata}
\alias{getFlightMetadata}
\title{Retrieve metadata for a specified route
}
\description{
Retrieves the available metadata for a specified flight. These include the 
airports of origin and destination, the operator IATA code and the flight
number.
}
\usage{
getFlightMetadata(flight)
}
\arguments{
  \item{route}{string with the call sign of a flight (for example, "AAL683" for 
  American Airlines flight number 683.
  }
}
\value{
A list with the following elements that provide information about the flight:
  \item{callSign }{Call sign of the flight
  }
  \item{originAirportICAO }{ICAO 4-letter code of the airport of origin
  }
  \item{destinationAirportICAO }{ICAO 4-letter code of the airport of origin
  }
  \item{operatorIATA }{IATA code for the operator of the flight
  }
  \item{flightNumber }{Flight number for the flight
  }
}
\references{
https://en.wikipedia.org/wiki/Flight_number
}
\examples{
# Obtain metadata for the flight with call sign AAL683

getFlightMetadata("AAL683")
}
