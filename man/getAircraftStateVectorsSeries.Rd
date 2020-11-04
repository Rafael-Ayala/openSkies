\name{getAircraftStateVectorsSeries}
\alias{getAircraftStateVectorsSeries}
\title{Retrieve a series of state vectors received from a specified aircraft
during a given time interval
}
\description{
Retrieves a time series o state vectors received from a specified aircraft 
during a given time interval. A state vector is a collection of data elements 
that characterize the status of an aircraft at a given point during a flight 
(such as latitude, longitude, altitude, etc.)

The time point must be specified as a date-time string in any format that can be 
unambiguously converted to POSIXct (such as YYYY-MM-DD HH:MM:SS). Time 
resolution for the time series of state vectors must be specified in seconds.
Time resolution is limited to 10 s for anonymous users, and 5 s for registered
users.
}
\usage{
getAircraftStateVectorsSeries(aircraft, startTime, endTime, 
                              timeZone=Sys.timezone(), timeResolution, 
                              username=NULL, password=NULL)
}
\arguments{
  \item{aircraft}{string with the ICAO 24-bit address of an aircraft (for 
  example, \"346190\" for Air Nostrum EC-NCD (ATR 72-600), or a character vector
  with multiple ICAO 24-bit addresses. In the default behavior, data is 
  retrieved for any aircraft.
  }
  \item{startTime}{date-time string indicating the starting time of the interval 
  for which state vectors should be retrieved. Must be in a format that can be 
  unambiguously converted into POSIXct time. Valid examples are
  \"2011-03-27 01:30:00\" and \"2011/03/27 01:30:00\".
  }
  \item{endTime}{date-time string indicating the ending time of the interval for
  which state vectors should be retrieved. Must be in a format that can be 
  unambiguously converted into POSIXct time. Valid examples are
  \"2011-03-28 01:30:00\" and \"2011/03/28 01:30:00\".
  }
  \item{timeZone}{string with the name of the time zone for time. For details on
  supported time zones, see help(timezones). By default, the system time zone is
  used.
  }
  \item{timeResolution}{time resolution in seconds to be used for the requested 
  timeseries of state vectors. Limited to 5 s for anonymous users and 10 s for
  registered users.
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
along the requested time series of state vectors for the specified aircraft. 
Each of the top-level list elements is a list with the state vector elements:
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
# Obtain a time series of state vectors for the aircraft with ICAO 24-bit 
# address 403003 for the 8th of October, 2020 between 16:50 and 16:53 (London
# time), with a time resolution of 1 minute.

getAircraftStateVectorsSeries("403003", startTime = "2020-10-08 16:50:00", 
endTime = "2020-10-08 16:52:00", timeZone="Europe/London", timeResolution=60)
}