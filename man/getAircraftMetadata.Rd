\name{getAircraftMetadata}
\alias{getAircraftMetadata}
\title{Retrieve metadata for a specified aircraft
}
\description{
Retrieves the available metadata for a specified aircraft. These include the
registration ID for the aircraft, as well as information about its manufacturer,
owner, operator and country of registration, among others. A single aircraft 
must be specified using its ICAO 24-bit address.
}
\usage{
getAircraftMetadata(aircraft, timeOut=60, maxQueryAttempts=1)
}
\arguments{
  \item{aircraft}{string with the ICAO 24-bit address of an aircraft (for 
  example, \"3c6444\" for Lufthansa D-AIBD (Airbus A319).
  }
  \item{timeOut}{number of seconds after which the query will time out and return
  a NULL result. In the default behavior, timeout will be reached after 60 seconds.
  }
  \item{maxQueryAttempts}{Maximum number of attempts that will be performed when
  carrying out the requested query. Failed attempts include timeouts. In the
  default behavior, a single attempt will be performed. It should be noted
  that setting a large number of maximum attempts can lead to long running
  times.
  }
}
\value{
An object of class \code{\link{openSkiesAircraft}}. See the \code{\link{openSkiesAirport}}
documentation for details on the fields for the class.
}
\references{
https://www.icao.int/publications/doc8643/pages/search.aspx

https://www.eurocontrol.int/sites/default/files/content/documents/nm/asterix/archives/asterix-cat021-asterix-ads-b-messages-part-12-v1.4-072009.pdf
}
\examples{
# Obtain metadata for the aircraft with ICAO 24-bit address 3922e2 

if(interactive()){
getAircraftMetadata("3922e2")
}
}
