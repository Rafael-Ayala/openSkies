\name{getAirportDepartures}
\alias{getAirportDepartures}
\title{Retrieve flight departures from a specified airport
}
\description{
Retrieves the list of flights that departed from a specified airport during a
certain time interval. The aiport must be specified using its ICAO identified.
Beginning and end times must be specified as date-time strings in any format 
that can be unambiguously converted to POSIXct (such as YYYY-MM-DD HH:MM:SS).
}
\usage{
getAirportDepartures(airport, startTime, endTime, timeZone=Sys.timezone(), 
                     username=NULL, password=NULL)
}
\arguments{
  \item{airport}{string with the ICAO identifier of an airport (for example,
  \"LEZL\" for Seville Airport.
  }
  \item{startTime}{date-time string indicating the starting time of the interval 
  for which departures should be retrieved. Must be in a format that can be 
  unambiguously converted into POSIXct time. Valid examples are
  \"2011-03-27 01:30:00\" and \"2011/03/27 01:30:00\".
  }
  \item{endTime}{date-time string indicating the ending time of the interval for
  which departures should be retrieved. Must be in a format that can be 
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
departed from the specified airport during the specified time interval. Each of 
the top-level list elements is a list in itself, containing the following 
elements that provide information about the flight:
  \item{ICAO24 }{ICAO 24-bit aicraft address associated to the aircraft in
  hexadecimal format
  }
  \item{callSign }{Call sign assigned to the aircraft. Can be NULL if no call
  sign is received. If the aircraft transmitted multiple call signs during the
  flight, the most frequent is returned
  }
  \item{departureAirport }{ICAO code of the airport from which the aicraft
  departed. Matches the requested airport
  }
  \item{arrivalAirport }{ICAO code of the airport at which the aicraft arrived.
  Can be NULL if the airport could not be identified
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
# Obtain a list with information for all the flights that departed from Seville
# Airport on the 25th of July, 2019 between 9 AM and 11 AM, local time.

getAirportDepartures(airport="LEZL", startTime="2019-07-25 09:00:00", 
endTime="2019-07-25 11:00:00", timeZone="Europe/Madrid")
}
