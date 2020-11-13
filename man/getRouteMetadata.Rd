\name{getRouteMetadata}
\alias{getRouteMetadata}
\title{Retrieve metadata for a specified route
}
\description{
Retrieves the available metadata for a specified flight route. These include the 
airports of origin and destination, the operator IATA code and the flight
number.
}
\usage{
getRouteMetadata(route)
}
\arguments{
  \item{route}{string with the call sign of a route (for example, "AAL683" for 
  American Airlines flight number 683.
  }
}
\value{
A list with the following elements that provide information about the route:
  \item{callSign }{Call sign of the route
  }
  \item{originAirportICAO }{ICAO 4-letter code of the airport of origin
  }
  \item{destinationAirportICAO }{ICAO 4-letter code of the airport of origin
  }
  \item{operatorIATA }{IATA code for the operator of the route
  }
  \item{flightNumber }{Flight number for the route
  }
}
\references{
https://en.wikipedia.org/wiki/Flight_number
}
\examples{
# Obtain metadata for the route with call sign AAL683

getRouteMetadata("AAL683")
}
