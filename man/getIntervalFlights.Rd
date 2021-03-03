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
represents a flight that was registered during the specified time interval. See 
the \code{\link{openSkiesFlight}} documentation for details on the fields for 
the class.
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

if(interactive()){
length(flights)
}
}
