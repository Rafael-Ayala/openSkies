\name{openSkiesAirport}
\alias{openSkiesAirport}
\title{An \code{\link{R6Class}} object representing an airport}
\description{
\code{\link{R6Class}} object representing an airport. Contains information
about the name of the airport, its IATA and ICAO codes, and its location.
New instances can be manually created by providing values for at least the 
fields \code{name}, \code{city}, \code{country}, \code{longitude} and 
\code{latitude}. Alternatively, \code{\link{getAirportMetadata}} will return an
\code{openSkiesAirport} object corresponding to the airport with the 
provided ICAO code.
}
\usage{
openSkiesAirport
}
\section{Fields}{

\describe{
  \item{\code{name}}{String with the name of the airport
  }
  \item{\code{ICAO}}{String with the ICAO code of the airport
  }
  \item{\code{IATA}}{String with the IATA code of the airport
  }
  \item{\code{longitude}}{Longitude of the position of the airport
  }
  \item{\code{latitude}}{Latitude of the position of the airport
  }
  \item{\code{altitude}}{Altitude of the position of the airport
  }
  \item{\code{city}}{String with the name of the city where the airport 
  is located
  }
  \item{\code{municipality}}{String with the ISO 3166-2 code where the 
  airport is located
  }
  \item{\code{region}}{String with the name of the region where the 
  airport is located
  }
  \item{\code{country}}{String with the ISO 3166-1 alpha-2 code of the country 
  where the airport is located
  }
  \item{\code{continent}}{String with the ISO 3166-1 alpha-2 code of the 
  continent where the airport is located
  }
  \item{\code{type}}{String with information about the type of airport
  }
  \item{\code{website}}{String with the URL for the website of the airport
  }
  \item{\code{wikipedia_entry}}{String with the URL for the Wikipedia entry 
  of the airport
  }
  \item{\code{reliable_position}}{Logical value indicating if the position of
  the airport is reliable
  }
  \item{\code{GPS_code}}{String with the GPS code of the airport
  }
}}

\examples{

# Create an openSkiesAirport object corresponding to Sevilla Airport

if(interactive){
test_airport <- getAirportMetadata("LEZL")
test_airport
}
}
