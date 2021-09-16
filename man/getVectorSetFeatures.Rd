\name{getVectorSetFeatures}
\alias{getVectorSetFeatures}
\title{Get positional features of an openSkiesStateVectorSet object
}
\description{
Retrieves positional features of an \code{\link{openSkiesStateVectorSet}} object. 
Features will be uniformly interpolated from the observed values.
}
\usage{
getVectorSetFeatures(stateVectorSet, resamplingSize=15, method="fmm", fields=NULL)
}
\arguments{
  \item{stateVectorSet}{object of class \code{\link{openSkiesStateVectorSet}} for
  which positional features should be extracted.
  }
  \item{resamplingSize}{number of uniformly separated interpolation points at
  which the values of the position features should be calculated.
  }
  \item{method}{method to be used for interpolation. "linear" will result in 
  linear interpolation, while "fmm", "periodic", or "natural" will result in 
  different types of spline interpolation.
  }
  \item{fields}{character vector indicating the names of the fields of the
  \code{\link{openSkiesStateVector}} objects that should be included in the
  extracted positional features. In the default behavior, \code{fields=NULL}
  and only latitude and longitude values will be used.
  }
}
\value{
A vector with positional features of the provided \code{\link{openSkiesStateVectorSet}} object.
The vector alternates values of longitude and latitude at each interpolated 
point, unless a character vector with valid names of fields of \code{\link{openSkiesStateVector}} 
objects is provided through \code{fields}, in which case values of the specified 
fields are alternated, in the specified order.
}
\examples{
# Extract positional features for a time series of state vectors for the 
# aircraft with ICAO 24-bit address 403003 for the 8th of October, 2020 between 
# 16:50 and 16:53 (London time), with a time resolution of 1 minute.

if(interactive()){
vectors <- getAircraftStateVectorsSeries(aircraft="4ca7b3", 
startTime="2020-11-04 10:30:00", endTime="2020-11-04 12:00:00", 
timeZone="Europe/London", timeResolution=300)

features <- getVectorSetFeatures(vectors)
}
}
