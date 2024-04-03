\name{getSingleTimeStateVectors}
\alias{getSingleTimeStateVectors}
\title{Retrieve all state vectors received at a given time point
}
\description{
Retrieves the list of all state vectors received from any or specified aircrafts
at a single time point. A state vector is a collection of data elements that
characterize the status of an aircraft at a given point during a flight (such
as latitude, longitude, altitude, etc.)

The time point must be specified as a date-time string in any format that can be 
unambiguously converted to POSIXct (such as YYYY-MM-DD HH:MM:SS). Results can be 
filtered to specific ranges of latitudes and/or longitudes. The extent of the 
data than can be accessed varies depending on if login details are provided:
* For anonymous users:
    + If no aircraft is specified or multiple aircrafts are specified: 
    historical data cannot be retrieved. If a time point was specified, it will
    be ignored, and data for current time will be returned.
    + If a single aircraft is specified, historical data can be retrieved.
* For registered users:
    + If no aircraft is specified or multiple aircrafts are specified: 
    historical data of up to 1 hour ago from current time can be retrieved.
    + If a single aircraft is specified, historical data can be retrieved.
}
\usage{
getSingleTimeStateVectors(aircraft=NULL, time=NULL, timeZone=Sys.timezone(),
                          minLatitude=NULL, maxLatitude=NULL, minLongitude=NULL,
                          maxLongitude=NULL, username=NULL, password=NULL,
                          useTrino=FALSE, timeOut=60, maxQueryAttempts=1)
}
\arguments{
  \item{aircraft}{string with the ICAO 24-bit address of an aircraft (for 
  example, \"346190\" for Air Nostrum EC-NCD (ATR 72-600), or a character vector
  with multiple ICAO 24-bit addresses. In the default behavior, data is 
  retrieved for any aircraft.
  }
  \item{time}{date-time string indicating the time point for which state vectors
  should be retrieved. Must be in a format that can be  unambiguously converted 
  into POSIXct time. Valid examples are \"2011-03-28 01:30:00\" and 
  \"2011/03/28 01:30:00\". If no time point is specified, data is retrieved for
  current time.
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
  \item{username}{optional string with the username to use for authentication
  for the OpenSky API. By default, no authentication is performed.
  }
  \item{password}{optional string with the password to use for authentication
  for the OpenSky API. By default, no authentication is performed.
  }
  \item{useTrino}{logical indicating whether or not to use the OpenSky
  Trino query interface instead of the API to retrieve state vectors if \code{includeStateVectors=TRUE}. If used, the provided 
  username and password are used for the Trino connection. By default, the Trino interface
  is not used.
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
}
\value{
If a single state vector matching the query conditions is found, an
\code{\link{openSkiesStateVector}} object.
If multiple state vectors matching the query conditions are found,
an \code{\link{openSkiesStateVectorSet}} object with field \code{time_series=FALSE}.
For details on the information stored in state vectors, see the documentation for
\code{\link{openSkiesStateVector}} and \code{\link{openSkiesStateVectorSet}}.
}
\references{
https://opensky-network.org/apidoc/rest.html
}
\examples{
# Obtain a list with the state vectors for all aircrafts currently flying over
# an area covering Switzerland.

getSingleTimeStateVectors(minLatitude=45.8389, maxLatitude=47.8229, 
minLongitude=5.9962, maxLongitude=10.5226)

# Obtain the state vector for aircraft with ICAO 24-bit address 403003 for
# the 8th of October, 2020 at 16:50 London time.

if(interactive()){
getSingleTimeStateVectors(aircraft="403003", time="2020-10-08 16:50:00", 
timeZone="Europe/London")
}
}