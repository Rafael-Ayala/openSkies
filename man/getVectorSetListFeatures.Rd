\name{getVectorSetListFeatures}
\alias{getVectorSetListFeatures}
\title{Get positional features of a list of openSkiesStateVectorSet objects
}
\description{
Retrieves positional features of a list of \code{\link{openSkiesStateVectorSet}} objects. 
Features will be uniformly interpolated from the observed values for all the
\code{\link{openSkiesStateVectorSet}} objects.
}
\usage{
getVectorSetListFeatures(stateVectorSetList, resamplingSize=15, method="fmm", 
                         scale=TRUE, useAngles=FALSE)
}
\arguments{
  \item{stateVectorSetList}{list of objects of class \code{\link{openSkiesStateVectorSet}} 
  for which positional features should be extracted.
  }
  \item{resamplingSize}{number of uniformly separated interpolation points at
  which the values of the position features should be calculated.
  }
  \item{method}{method to be used for interpolation. "linear" will result in 
  linear interpolation, while "fmm", "periodic", or "natural" will result in 
  different types of spline interpolation.
  }
  \item{scale}{logical indicating if the matrix of features should be scaled
  by applying the \code{\link{scale}} function. This can be desirable if the
  features are going to be used for clustering.
  }
  \item{useAngles}{logical indicating if the angle of the trajectory of the
  aircraft should be included in the extracted positional features.
  }
}
\value{
A matrix with positional features of the provided list of \code{\link{openSkiesStateVectorSet}} 
objects. Each row of the matrix represents a vector of features for each of the
\code{\link{openSkiesStateVectorSet}} objects. Each vector alternates values of 
longitude and latitude at each interpolated point, unless \code{useAngles=TRUE}, 
in which case values of longitude, latitude and trajectory angle are alternated, 
in this order.
}
\examples{
# Extract positional features for a time series of state vectors for the 
# aircraft with ICAO 24-bit address 403003 for the 8th of October, 2020 between 
# 16:50 and 16:53 (London time), with a time resolution of 1 minute.

if(interactive()){
vectors1=getAircraftStateVectorsSeries(aircraft="345107", 
startTime="2020-11-04 11:55:00", endTime="2020-11-04 13:10:00", 
timeZone="Europe/London", timeResolution=300)

vectors2=getAircraftStateVectorsSeries(aircraft = "4ca7b3", 
startTime="2020-11-04 10:30:00", endTime="2020-11-04 12:00:00",
timeZone="Europe/London", timeResolution=300)

vectors_list=list(vectors1, vectors2)

features_matrix=getVectorSetListFeatures(vectors_list, scale=FALSE, useAngles=TRUE)
}
}
