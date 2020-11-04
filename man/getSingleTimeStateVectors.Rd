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
                          maxLongitude=NULL, username=NULL, password=NULL)
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
}
\value{
A nested list where each element of the top-level list represents a state vector 
that was received at the specified time point. Each of the top-level list 
elements is a list in itself, containing the following elements that provide 
information about the aircraft whose transponder emitted the information:
  \item{ICAO24 }{ICAO 24-bit aicraft address associated to the aircraft that
  whose transponder emitted the state vector
  }
  \item{callSign }{Call sign assigned to the aircraft. Can be NULL if no call
  sign is received. If the aircraft transmitted multiple call signs during the
  flight, the most frequent is returned
  }
  \item{originCountry }{Country of origin of the aircraft, inferred from the 
  ICAO 24-bit address
  }
  \item{requestedTime }{Time point for which the state vector was requested
  }
  \item{lastPositionUpdateTime }{Time at which the last position update for
  the aircraft was received, or NULL if no position update has been received in
  the past 15 s
  }
  \item{lastAnyUpdateTime }{Time at which the last update (of any type) for
  the aircraft was received
  }
  \item{longitude }{Longitude value for the position of the aircraft
  }
  \item{latitude }{Latitude value for the position of the aircraft
  }
  \item{baroAltitude }{Barometric altitude of the aircraft in meters
  }
  \item{geoAltitude }{Geometric altitude of the aircraft in meters
  }
  \item{onGround }{Boolean indicating if the aircraft is at a surface position
  }
  \item{velocity }{Velocity of the aircraft over the ground in meters/second
  }
  \item{trueTrack }{True track angle in degrees of the current aircraft course.
  Measured clockwise from the North (0º)
  }
  \item{verticalRate }{Vertical movement rate of the aircraft in meters/second.
  Positive means the aircraft is climbing, and negative means it is descending
  }
  \item{squawk }{Squawk code for the aircraft transponder
  }
  \item{specialPurposeIndicator }{Boolean indicating if the transponder of the
  aircraft has emitted a Special Purpose Indicator pulse 
  }
  \item{positionSource }{Source of the position information for this state
  vector. Can be ADS-B (Automatic Dependent Surveillance–Broadcast), ASTERIX
  (All Purpose Structured Eurocontrol Surveillance Information Exchange) or 
  MLAT (Multilateration)
  }
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

getSingleTimeStateVectors(aircraft="403003", time="2020-10-08 16:50:00", 
timeZone="Europe/London")
}