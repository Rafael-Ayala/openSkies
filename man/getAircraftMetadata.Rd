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
getAircraftMetadata(aircraft)
}
\arguments{
  \item{aircraft}{string with the ICAO 24-bit address of an aircraft (for 
  example, \"3c6444\" for Lufthansa D-AIBD (Airbus A319).
  }
}
\value{
A list with the following elements that provide information about the aircraft:
  \item{ICAO24 }{ICAO 24-bit aicraft address associated to the aircraft in
  hexadecimal format
  }
  \item{registration }{Aircraft registration code, also called tail number
  }
  \item{country }{Country where the aircraft is registered
  }
  \item{manufacturerName }{Manufacturer of the aircraft
  }
  \item{manufacturerICAO }{ICAO code of the manufacturer of the aircraft
  }
  \item{model }{Aircraft model
  }
  \item{serialNumber }{Serial number of the aircraft
  }
  \item{lineNumber }{Usually only provided for Boeing aircrafts. Line numbers
  specify the order in which airframes of a particular product line where 
  assembled
  }
  \item{ICAOtypeCode }{ICAO code for the model of aircraft
  }
  \item{ICAOaircraftClass }{ICAO code for the type of aircraft. ICAO aircraft
  classes provide more general groups than ICAO type codes
  }
  \item{owner }{Registered aircraft owner
  }
  \item{operator }{Aircraft operator
  }
  \item{operatorCallsign }{Callsign of the aircraft operator
  }
  \item{operatorICAO }{ICAO code of the aircraft operator
  }
  \item{operatorIATA }{IATA code of the aircraft operator
  }
  \item{firstFlightDate }{Date where the first flight for the aircraft was
  registered. Usually not available
  }
  \item{categoryDescription }{Physical information about the aircraft provided
  by the ADS-B emitter unit. Not always available 
  }
}
\references{
https://www.icao.int/publications/doc8643/pages/search.aspx

https://www.eurocontrol.int/sites/default/files/content/documents/nm/asterix/archives/asterix-cat021-asterix-ads-b-messages-part-12-v1.4-072009.pdf
}
\examples{
# Obtain metadata for the aircraft with ICAO 24-bit address 3922e2 

getAircraftMetadata("3922e2")
}
