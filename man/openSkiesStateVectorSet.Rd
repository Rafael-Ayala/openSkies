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
   \item{
    \code{get_uniform_interpolation(n, fields, method="fmm")}}{
     This method obtains a data frame with an interpolation of the specified fields along the
     route represented by the state vector set across n evenly distributed points. \code{fields}
     should be a character vector with the name of the fields that will be interpolated.
     Only numeric fields are accepted. \code{method} represents the interpolation method.
     "linear" will result in linear interpolation, while "fmm", "periodic", or "natural" will result
     in different types of spline interpolation.
   }
   \item{
    \code{get_time_points_interpolation(fields, time_field, timestamps, method="fmm")}}{
     This method obtains a data frame with an interpolation of the specified fields along the
     route represented by the state vector set across the specified timestamps. \code{fields}
     should be a character vector with the name of the fields that will be interpolated.
     Only numeric fields are accepted. \code{method} represents the interpolation method.
     "linear" will result in linear interpolation, while "fmm", "periodic", or "natural" will result
     in different types of spline interpolation. \code{time_field} indicates the
     name of the field of the openSkiesStateVectorSet object from which the 
     timestamps of the original state vectors will be retrieved. Possible values
     are \code{c("requested_time", "last_position_update_time", "last_any_update_time")}.
     The time points at which the interpolations should be calculated should be
     provided as a vector through the \code{timestamps} argument.
   }
   \item{
    \code{sort_by_field(field, decreasing=FALSE)}}{
     This method sorts the state vectors of the openSkiesStateVectorSet object
     by the values of the field provided through \code{field}. By default,
     \code{decreasing=FALSE} and the state vectors will be sorted by increasing 
     order of \code{field}. If \code{decreasing=TRUE}, decreasing order will be
     used.
   }
   \item{
    \code{split_into_flights(timeOnLandThreshold=300, timeDiffThreshold=1800)}}{
     This method automatically detects different flights contained in the
     openSkiesStateVectorSet object and returns a list of objects of class
     \code{\link{openSkiesFlight}}. Separate flights are detected based on two
     conditions: either the aircraft staying on ground for a given amount of
     time, or the aircraft not sending any status update for a given period. The
     thresholds are controlled, respectively, through the \code{timeOnLandThreshold}
     and \code{timeDiffThreshold} arguments. In both cases, the value should be
     provided in seconds.
   }
   \item{
    \code{remove_redundants(updateType="position")}}{
     This method removes redundant state vectors, i.e., those that do not
     contain updated information with respect to older state vectors. What is
     considered as a redundant state vector is defined by the \code{updateType} 
     argument. If \code{updateType="position"}, which is also the default
     behavior, state vectors for which there was no update of positional
     information will be considered as redundant (even if there might have been
     an update of other information). If \code{updateType="any"}, only state
     vectors for which no information was updated (positional or any other) will
     be considered as redundant. It should be noted that applying this method
     will also sort the state vectors from older to more recent. The method
     is intended to be applied to time series, and therefore a warning will be
     given if it is applied on an \code{\link{openSkiesStateVectorSet}} with
     field \code{time_series=FALSE}.
   }
}
}

\examples{

# Obtain a time series of state vectors for the aircraft with ICAO 24-bit 
# address 403003 for the 8th of October, 2020 between 16:50 and 16:53 (London
# time), with a time resolution of 1 minute.

if(interactive()){
test_stateVectorSet <- getAircraftStateVectorsSeries("403003", startTime="2020-10-08 16:50:00", 
endTime="2020-10-08 16:52:00", timeZone="Europe/London", timeResolution=60)
test_stateVectorSet
}
}
