\name{getAirportMetadata}
\alias{getAirportMetadata}
\title{Retrieve metadata for a specified airport
}
\description{
Retrieves the available metadata for a specified airport. These include its
IATA code, common name and location, among others. A single airport  must be 
specified using its ICAO code.
}
\usage{
getAirportMetadata(airport, timeOut=60, maxQueryAttempts=1)
}
\arguments{
  \item{airport}{string with the ICAO 4-letter code of an airport (for 
  example, \"LEZL\" for Sevilla Airport.
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
An object of class \code{\link{openSkiesAirport}}. See the \code{\link{openSkiesAirport}}
documentation for details on the fields for the class.
}
\references{
https://en.wikipedia.org/wiki/List_of_airports_by_IATA_and_ICAO_code
}
\examples{
# Obtain metadata for the airport with ICAO code LEZL

if(interactive()){
getAirportMetadata("LEZL")
}
}
