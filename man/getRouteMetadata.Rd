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
getRouteMetadata(route, includeAirportsMetadata=FALSE, timeOut=60, maxQueryAttempts=1)
}
\arguments{
  \item{route}{string with the call sign of a route (for example, "AAL683" for 
  American Airlines flight number 683.
  }
  \item{includeAirportsMetadata}{logical indicating if the data about the origin and destination
  airports of the route should also be retrieved. If not, only the ICAO24 code of the airports will be
  included.
  }
  \item{timeOut}{number of seconds after which the query will time out and return
  a NULL result. In the default behavior, timeout will be reached after 60 seconds.
  }
  \item{maxQueryAttempts}{On rare occassions, queries to the OpenSky Network
  live API can return malformed responses. This is the maximum number of attempts 
  to obtain a properly formatted response when carrying out the requested query. 
  It should be noted that the query will still terminate if a timeout is reached.
  In the default behavior, a single attempt will be performed. It is not recommended
  to change this to a very large number, since it can lead to long running times.
  }
}
\value{
An object of class \code{\link{openSkiesRoute}}. See the \code{\link{openSkiesRoute}}
documentation for details on the fields for the class.
}
\references{
https://en.wikipedia.org/wiki/Flight_number
}
\examples{
# Obtain metadata for the route with call sign AAL683

if(interactive()){
getRouteMetadata("AAL683")
}
}
