\name{getIntervalFlights}
\alias{getIntervalFlights}
\title{Retrieve all flights registered during a time interval
}
\description{
Retrieves the list of all flights registered for any aircraft during a given
time interval. Beginning and end times must be specified as date-time strings in 
any format that can be unambiguously converted to POSIXct (such as 
YYYY-MM-DD HH:MM:SS).
}
\usage{
getIntervalFlights(startTime, endTime, timeZone=Sys.timezone(), username=NULL, 
                   password=NULL)
}
\arguments{
  \item{startTime}{date-time string indicating the starting time of the interval 
  for which flights should be retrieved. Must be in a format that can be 
  unambiguously converted into POSIXct time. Valid examples are
  \"2011-03-27 01:30:00\" and \"2011/03/27 01:30:00\".
  }
  \item{endTime}{date-time string indicating the ending time of the interval for
  which flights should be retrieved. Must be in a format that can be 
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
was registered during the specified time interval. Each of the top-level list 
elements is a list in itself, containing the following elements that provide 
information about the flight:
  \item{ICAO24 }{ICAO 24-bit aicraft address associated to the aircraft that
  performed the flight in hexadecimal format
  }
  \item{callSign }{Call sign assigned to the aircraft. Can be NULL if no call
  sign is received. If the aircraft transmitted multiple call signs during the
  flight, the most frequent is returned
  }
  \item{departureAirport }{ICAO code of the airport from which the aicraft
  departed. Can be NULL if the airport could not be identified
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
# Obtain a list with information for all the flights registered during the 16th
# of November, 2019 between 9 AM and 10 AM, London time.

flights <- getIntervalFlights(startTime="2019-11-16 09:00:00", 
endTime="2019-11-16 10:00:00", timeZone="Europe/London")

# Count the number of registered flights.

length(flights)
}
