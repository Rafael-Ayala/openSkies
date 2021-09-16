\name{getOSNCoverage}
\alias{getOSNCoverage}
\title{Retrieve coverage of the OpenSky Network for a given day
}
\description{
Retrieves the coverage of the OpenSky Network across all regions for a given
day. The date must be specified as a date-time string in any format that can
be unambiguously converted to POSIXct (such as YYYY-MM-DD).
}
\usage{
getOSNCoverage(time, timeZone=Sys.timezone(), timeOut=60, maxQueryAttempts=1)
}
\arguments{
  \item{time}{date-time string indicating the day for which coverage should be
  retrieved. Must be in a format that can be unambiguously converted into 
  POSIXct time. Valid examples are \"2011-03-27\" and \"2011/03/27\". The
  exact time of the day can also be supplied in the date-time string, but 
  coverage data is only available with single-day resolution.
  }
  \item{timeZone}{string with the name of the time zone for startTime and 
  endTime. For details on supported time zones, see help(timezones). By default,
  the system time zone is used.
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
A dataframe with three columns, named "latitude", "longitude" and "altitude".
Each row represents an area of coverage data. The first two columns indicate
the coordinates of the center of each area, which extends 0.1 degrees North and
South and 0.15 degrees East and West from its center. The third column,
"altitude", indicates the lowest altitude value received for any aircraft in the 
area. This provides an estimate of the coverage for that given area, with lower
values indicating a better coverage since low-flying aircraft are more difficult
to detect due to a higher chance that obstacles can block the line of sight
between the aircraft and the receptors in the area. 

The "altitude" values are obtained from the barometric altitude sensors, and 
therefore is prone to the same errors as such sensors (e.g., negative altitudes 
might be reported). Areas not covered by any of the rows in the dataframe do not
have any coverage.
}
\references{
https://opensky-network.org/forum/questions/640-interpreting-the-response-from-the-coverage-api-endpoint
}
\examples{
# Obtain a data frame with coverage of the OpenSky Network for the 13th of
# September, 2020.

if(interactive()){
getOSNCoverage("2020-09-13", timeZone="Europe/London")
}
}
