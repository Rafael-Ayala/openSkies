\name{openSkiesStateVectorSet}
\alias{openSkiesStateVectorSet}
\title{An \code{\link{R6Class}} object representing an ensemble of aircraft 
state vectors}
\description{
\code{\link{R6Class}} object representing an ensemble of aircraft state vectors. 
Contains a list of objects of class \code{\link{openSkiesStateVector}}. The
ensemble can either represent a time series of state vectors of a single 
aircraft, or represent state vectors corresponding to multiple different
aircrafts. For details on the information stored in each state vector, see
the documentation for the \code{\link{openSkiesStateVector}} class. New instances can be manually 
created by providing a list of \code{\link{openSkiesStateVector}} objects.
Alternatively, \code{\link{getSingleTimeStateVectors}} and \code{\link{getAircraftStateVectorsSeries}} 
will return an \code{\link{openSkiesStateVectorSet}} object if multiple state
vectors matching the query conditions are found.
}
\usage{
openSkiesStateVectorSet
}
\section{Fields}{

\describe{
  \item{\code{state_vectors}}{List of \code{\link{openSkiesStateVector}} objects
  }
  \item{\code{time_series}}{Logical indicating if the \code{\link{openSkiesStateVectorSet}}
  object represents a time series of state vectors of a single aircraft.
  }
}}

\section{Methods}{

\describe{
  \item{
    \code{add_state_vector(state_vector)}}{
     This method adds a new state vector to the openSkiesAircraft object, which
     will be set as the new \code{last_state_vector} and will be added to
     \code{state_vector_history}. \code{state_vector} should be an
     \code{\link{openSkiesStateVector}} object
   }
}
}


\examples{

# Obtain a time series of state vectors for the aircraft with ICAO 24-bit 
# address 403003 for the 8th of October, 2020 between 16:50 and 16:53 (London
# time), with a time resolution of 1 minute.

test_stateVectorSet <- getAircraftStateVectorsSeries("403003", startTime="2020-10-08 16:50:00", 
endTime="2020-10-08 16:52:00", timeZone="Europe/London", timeResolution=60)
test_stateVectorSet
}
