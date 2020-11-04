\name{getAircraftFlights}
\alias{getAircraftFlights}
\title{Retrieve flights performed by a specified aircraft during a time interval
}
\description{
Retrieves the list of flights registered for a specified aircraft during a given
time interval. The aircraft must be specified using its ICAO 24-bit address.
Beginning and end times must be specified as date-time strings in any format 
that can be unambiguously converted to POSIXct (such as YYYY-MM-DD HH:MM:SS).
}
\usage{
getAircraftFlights(aircraft, startTime, endTime, timeZone=Sys.timezone(), 
                   username=NULL, password=NULL)
}
\arguments{
  \item{aircraft}{string with the ICAO 24-bit address of an aircraft (for 
  example, \"346190\" for Air Nostrum EC-NCD (ATR 72-600).
  }
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
was performed by the specified aircraft during the specified time interval. Each 
of the top-level list elements is a list in itself, containing the following 
elements that provide information about the flight:
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
# Obtain a list with information for all the flights registered for the aircraft
# with ICAO 24-bit address 346190 during the 26th of July, 2019.

getAircraftFlights("346190", startTime="2019-07-26 00:00:00", 
endTime="2019-07-26 23:59:59", timeZone="Europe/Madrid")
}
