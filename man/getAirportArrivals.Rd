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
  \item{includeStateVectors}{logical indicating if the set of state vectors for
  each flight should also be retrieved. By default, state vectors are not
  retrieved.
  }
  \item{timeResolution}{time resolution in seconds with which state vectors
  should be retrieved if \code{includeStateVectors=TRUE}.
  }
  \item{useImpalaShell}{logical indicating whether or not to use the OpenSky
  impala shell instead of the API to retrieve state vectors if \code{includeStateVectors=TRUE}. If used, the provided 
  username and password are used for the ssh connection. By default, the impala shell
  is not used.
  }
}
\value{
A list of objects of class \code{\link{openSkiesFlight}}, where each object
represents a flight that that landed at the specified airport during the 
specified time interval. See the \code{\link{openSkiesFlight}} documentation 
for details on the fields for the class.
}
\references{
https://opensky-network.org/apidoc/rest.html
}
\examples{
# Obtain a list with information for all the flights that landed at Frankfurt
# International Airport on the 29th of January, 2018 between 12 PM and 1 PM,
# local time.

if(interactive()){
getAirportArrivals(airport="EDDF", startTime="2018-01-29 12:00:00", 
endTime="2018-01-29 13:00:00", timeZone="Europe/Berlin")
}
}
