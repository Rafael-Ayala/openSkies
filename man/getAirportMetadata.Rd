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
getAirportMetadata(airport)
}
\arguments{
  \item{airport}{string with the ICAO 4-letter code of an airport (for 
  example, \"LEZL\" for Sevilla Airport.
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
