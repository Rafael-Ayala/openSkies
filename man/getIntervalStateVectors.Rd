\name{getIntervalStateVectors}
\alias{getIntervalStateVectors}
\title{Retrieve all state vectors received during a time interval
}
\description{
Retrieves the list of all state vectors received from any or specified aircrafts
during an interval of time. A state vector is a collection of data elements that
characterize the status of an aircraft at a given point during a flight (such
as latitude, longitude, altitude, etc.)

The starting and end time points must be specified as date-time strings in any 
format that can be  unambiguously converted to POSIXct (such as 
YYYY-MM-DD HH:MM:SS). Results can be filtered to specific ranges of latitudes 
and/or longitudes. This function requires access to the OpenSky Network Impala
shell, and therefore can only be used by registered users that have been granted
access to the Impala shell.
}
\usage{
getIntervalStateVectors(aircraft=NULL, startTime, endTime,timeZone=Sys.timezone(),
                        minLatitude=NULL, maxLatitude=NULL, minLongitude=NULL,
                        maxLongitude=NULL, username, password)
}
\arguments{
  \item{aircraft}{string with the ICAO 24-bit address of an aircraft (for 
  example, \"346190\" for Air Nostrum EC-NCD (ATR 72-600), or a character vector
  with multiple ICAO 24-bit addresses. In the default behavior, data is 
  retrieved for any aircraft.
  }
  \item{startTime}{date-time string indicating the starting time point of the
  interval for which state vectors should be retrieved. Must be in a format that 
  can be unambiguously converted into POSIXct time. Valid examples are 
  \"2011-03-28 01:30:00\" and \"2011/03/28 01:30:00\".
  }
  \item{endTime}{date-time string indicating the end time point of the interval 
  for which state vectors should be retrieved. Must be in a format that can be 
  unambiguously converted into POSIXct time. Valid examples are 
  \"2011-03-28 01:30:00\" and \"2011/03/28 01:30:00\".
  }
  \item{timeZone}{string with the name of the time zone for time. For details on
  supported time zones, see help(timezones). By default, the system time zone is
  used.
  }
  \item{minLatitude}{minimum latitude to filter the retrieved state vectors.
  Must be a value between -180 and 180. Negative values denote south latitudes,
  and positive values denote north latitudes. By default, no filtering based on
  location is performed.
  }
  \item{maxLatitude}{maximum latitude to filter the retrieved state vectors.
  Must be a value between -180 and 180. Negative values denote south latitudes,
  and positive values denote north latitudes. By default, no filtering based on
  location is performed.
  }
  \item{minLongitude}{minimum longitude to filter the retrieved state vectors.
  Must be a value between -180 and 180. Negative values denote west longitudes,
  and positive values denote east longitudes. By default, no filtering based on
  location is performed.
  }
  \item{maxLongitude}{maximum longitude to filter the retrieved state vectors.
  Must be a value between -180 and 180. Negative values denote west longitudes,
  and positive values denote east longitudes. By default, no filtering based on
  location is performed.
  }
  \item{username}{string with the username to use for authentication for the 
  OpenSky API. The user must have been granted access to the Impala shell.
  }
  \item{password}{string with the password to use for authentication for the 
  OpenSky API. The user must have been granted access to the Impala shell.
  }
}
\value{
An \code{\link{openSkiesStateVectorSet}} object with field \code{time_series=FALSE},
which contains all the state vectors that matched the query parameters.
For details on the information stored in state vectors, see the documentation for
\code{\link{openSkiesStateVector}} and \code{\link{openSkiesStateVectorSet}}.
}
\references{
https://opensky-network.org/impala-guide
}
\examples{
# Obtain a list with the state vectors for all aircrafts that flew over the city
# of Seville the 21st of July, 2019 between 7 AM and 8 PM Spanish time.
# Note that the username and password should be substituted by your own,
# for which you should have received authorization to access the OpenSky
# Impala shell

\dontrun{
state_vectors <- getIntervalStateVectors(startTime = "2019-07-21 07:00:00",
                                         endTime = "2019-07-21 20:00:00",
                                         timeZone = "Europe/Madrid",
                                         minLatitude = 37.362796,
                                         minLongitude = -6.019819,
                                         maxLatitude = 37.416954,
                                         maxLongitude = -5.939724,
                                         username="your_username",
                                         password="your_password")
                                         
# Group the state vectors into flights

flights <- state_vectors$split_into_flights()

# Plot the flight paths

paths <- vector(mode = "list", length = length(flights))

for(i in 1:length(flights)) {
  paths[[i]] <- flights[[i]]$state_vectors
}

plotRoutes(paths, pathColors = rainbow(length(flights)))
}
}