\name{plotRoutes}
\alias{plotRoutes}
\title{Plot several aircraft routes
}
\description{
Draws the given routes on a ggmap object. The routes must be given as a list of 
objects of class \code{\link{openSkiesStateVectorSet}}, all of them with field 
\code{time_series=TRUE}.
}
\usage{
plotRoutes(stateVectorSetList, pathColors="blue", ggmapObject=NULL, 
plotResult=TRUE, paddingFactor=0.2, lineSize=1, lineAlpha=0.5, pointSize=0.3, 
pointAlpha=0.8, arrowLength=0.3)
}
\arguments{
  \item{stateVectorSetList}{list of objects of class \code{\link{openSkiesStateVectorSet}}
  with field \code{time_series=TRUE}, each of them containing positional 
  information of given aircraft along different timepoints.
  }
  \item{pathColors}{vector with the colors of the paths and points that will be 
  used to draw the routes. If the number of routes is greater than the number of 
  colors, these will be rotated. Each color must be a value accepted by ggmap's 
  color attributes.
  }
  \item{ggmapObject}{optional ggmap object on which the routes will be drawn. By 
  default, a new ggmap object will be created, covering the necessary space plus
  an amount of padding determined by the paddingFactor argument.
  }
  \item{plotResult}{wether or not the resulting ggmap object with the added 
  routes should be plotted.
  }
  \item{paddingFactor}{amount of padding to add to the map if no ggmap object is
  provided. The added padding will be equal to paddingFactor multiplied by the 
  height and width of the map (determined by the difference between the maximum 
  and minimum longitudes/latitudes).
  }
  \item{lineSize}{width of the line that connects the points of the routes in 
  the plot.
  }
  \item{lineAlpha}{opacity of the line that connects the points of the routes in
  the plot.
  }
  \item{pointSize}{size of the points of the routes in the plot.
  }
  \item{pointAlpha}{opacity of the points of the routes in the plot.
  }
  \item{arrowLength}{length of the segment arrows in centimeters.
  }
}
\value{
A ggmap object with added paths and points representing the routes.
}
\references{
https://opensky-network.org/apidoc/rest
}
\examples{
# Plot the routes followed by two aircrafts departing from Sevilla airport the
# 4th of November, 2020.

if(interactive()){
vectors1=getAircraftStateVectorsSeries(aircraft="345107", 
startTime="2020-11-04 11:55:00", endTime="2020-11-04 13:10:00", 
timeZone="Europe/London", timeResolution=300)

vectors2=getAircraftStateVectorsSeries(aircraft = "4ca7b3", 
startTime="2020-11-04 10:30:00", endTime="2020-11-04 12:00:00",
timeZone="Europe/London", timeResolution=300)

plotRoutes(list(vectors1, vectors2), pathColors=c("red", "blue"))
}
}
