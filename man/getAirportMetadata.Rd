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
A list with the following elements that provide information about the airport:
  \item{ICAO }{ICAO 4-letter code of the airport
  }
  \item{IATA }{IATA 3-letter code of the airport
  }
  \item{name }{Common name of the airport
  }
  \item{city }{City where the airport is located
  }
  \item{municipality }{Municipality where the airport is located
  }
  \item{region }{ISO 3166-2 code for the region where the airport is located
  }
  \item{country }{ISO 3166-1 alpha-2 code for the country where the airport is 
  located
  }
  \item{continent }{ISO 3166-1 alpha-2 code for the continent where the airport 
  is located
  }
  \item{longitude }{Longitude value for the position of the airport
  }
  \item{latitude }{Latitude value for the position of the airport
  }
  \item{altitude }{Altitude value for the position of the airport
  }
  \item{reliablePosition }{Logical value indicating if the position of the 
  airport is reliable
  }
  \item{GPSCode }{GPS code of the airport
  }
  \item{type }{Information about the type of airport
  }
  \item{website }{Website for the airport. Can be NULL even if the airport
  actually has a website
  }
  \item{wikipediaEntry }{Link to the Wikipedia entry for the airport. Can be 
  NULL even if the airport actually has a Wikipedia entry
  }
}
\references{
https://en.wikipedia.org/wiki/List_of_airports_by_IATA_and_ICAO_code
}
\examples{
# Obtain metadata for the airport with ICAO code LEZL

getAirportMetadata("LEZL")
}
