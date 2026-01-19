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
                     username=NULL, password=NULL, includeStateVectors=FALSE, 
                     timeResolution=NULL, useTrino=FALSE, 
                     includeAirportsMetadata=FALSE, timeOut=60, maxQueryAttempts=1)
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
  \item{includeStateVectors}{logical indicating if the set of state vectors for
  each flight should also be retrieved. By default, state vectors are not
  retrieved.
  }
  \item{timeResolution}{time resolution in seconds with which state vectors
  should be retrieved if \code{includeStateVectors=TRUE}.
  }
  \item{useTrino}{logical indicating whether or not to use the OpenSky
  Trino query interface instead of the API to retrieve state vectors if \code{includeStateVectors=TRUE}. If used, the provided 
  username and password are used for the Trino connection. By default, the Trino interface
  is not used.
  }
  \item{includeAirportsMetadata}{logical indicating if the data about the origin and destination
  airports of each flight should also be retrieved. If not, only the ICAO24 code of the airports will be
  included.
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
  \item{credentials}{Optional credentials object obtained via \code{getCredentials()} to authenticate
  with the OpenSky API using either username + password or client_id + client_secret.}
}
\value{
A list of objects of class \code{\link{openSkiesFlight}}, where each object
represents a flight that that departed from the specified airport during the 
specified time interval. See the \code{\link{openSkiesFlight}} documentation 
for details on the fields for the class.
}
\references{
https://opensky-network.org/apidoc/rest.html
}
\examples{
# Obtain a list with information for all the flights that departed from Seville
# Airport on the 25th of July, 2019 between 9 AM and 11 AM, local time.

if(interactive()){
getAirportDepartures(airport="LEZL", startTime="2019-07-25 09:00:00", 
endTime="2019-07-25 11:00:00", timeZone="Europe/Madrid")
}
}
