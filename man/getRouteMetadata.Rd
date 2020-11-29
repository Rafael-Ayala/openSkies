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
An object of class \code{\link{openSkiesRoute}}. See the \code{\link{openSkiesRoute}}
documentation for details on the fields for the class.
\references{
https://en.wikipedia.org/wiki/Flight_number
}
\examples{
# Obtain metadata for the route with call sign AAL683

getRouteMetadata("AAL683")
}
