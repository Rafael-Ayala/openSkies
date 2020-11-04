\name{getAirportArrivals}
\alias{getAirportArrivals}
\title{Retrieve flight arrivals into a specified airport
}
\description{
Retrieves the list of flights that landed into a specified airport during a
certain time interval. The aiport must be specified using its ICAO identified.
Beginning and end times must be specified as date-time strings in any format 
that can be unambiguously converted to POSIXct (such as YYYY-MM-DD HH:MM:SS).
}
\usage{
getAirportArrivals(airport, startTime, endTime, timeZone=Sys.timezone(), 
                   username=NULL, password=NULL)
}
\arguments{
  \item{airport}{string with the ICAO identifier of an airport (for example,
  \"EDDF\" for Frankfurt International Airport.
  }
  \item{startTime}{date-time string indicating the starting time of the interval 
  for which arrivals should be retrieved. Must be in a format that can be 
  unambiguously converted into POSIXct time. Valid examples are
  \"2011-03-27 01:30:00\" and \"2011/03/27 01:30:00\".
  }
  \item{endTime}{date-time string indicating the ending time of the interval for
  which arrivals should be retrieved. Must be in a format that can be 
  unambiguously converted into POSIXct time. Valid examples are
  \"2011-03-28 01:30:00\" and \"2011/03/28 01:30:00\".
  }
  \item{timeZone}{string with the name of the time zone for startTime and 
  endTime. For details on supported time zones, see help(timezones). By default,
  the system time zone is used.
  }
  \item{username}{optional string with the username to use for authentication
  for the OpenSky API. By default, no authentication is performed.
  }
  \item{password}{optional string with the password to use for authentication
  for the OpenSky API. By default, no authentication is performed.
  }
}
\value{
A nested list where each element of the top-level list represents a flight that
landed at the specified airport during the specified time interval. Each of the 
top-level list elements is a list in itself, containing the following elements 
that provide information about the flight:
  \item{ICAO24 }{ICAO 24-bit aicraft address associated to the aircraft in
  hexadecimal format
  }
  \item{callSign }{Call sign assigned to the aircraft. Can be NULL if no call
  sign is received. If the aircraft transmitted multiple call signs during the
  flight, the most frequent is returned
  }
  \item{departureAirport }{ICAO code of the airport from which the aicraft
  departed. Can be NULL if the airport could not be identified
  }
  \item{arrivalAirport }{ICAO code of the airport at which the aicraft arrived.
  Matches the requested airport
  }
  \item{departureTime }{Estimated time of departure for the flight
  }
  \item{arrivalTime }{Estimated time of arrival for the flight
  }
}
\references{
https://opensky-network.org/apidoc/rest.html
}
\examples{
# Obtain a list with information for all the flights that landed at Frankfurt
# International Airport on the 29th of January, 2018 between 12 PM and 1 PM,
# local time.

getAirportArrivals(airport="EDDF", startTime="2018-01-29 12:00:00", 
endTime="2018-01-29 13:00:00", timeZone="Europe/Berlin")
}
