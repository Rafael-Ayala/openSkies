\name{plotRoute}
\alias{plotRoute}
\title{Plot a single aircraft given as a list of state vectors
}
\description{
Draws a given route on a ggmap object. The route must be given as a list of
state vectors, each represented by a list with at least longitude and latitude
fields.
}
\usage{
plotRoute(stateVectors, pathColor="blue", ggmapObject=NULL, plotResult=TRUE, 
          paddingFactor=0.2, lineSize=1, lineAlpha=0.5, pointSize=0.3, 
          pointAlpha=0.8)
}
\arguments{
  \item{stateVectors}{list of state vectors. Each state vector must be 
  represented by a list with, at least, fields "longitude" and "latitude".
  }
  \item{pathColor}{color of the path and points that will be used to draw the 
  route. Must be a value accepted by ggmap's color attributes.
  }
  \item{ggmapObject}{optional ggmap object on which the route will be drawn. 
  By default, a new ggmap object will be created, covering the necessary space 
  plus an amount of padding determined by the paddingFactor argument.
  }
  \item{plotResult}{whether or not the resulting ggmap object with the added 
  route should be plotted.
  }
  \item{paddingFactor}{amount of padding to add to the map if no ggmap object is
  provided. The added padding will be equal to paddingFactor multiplied by the 
  height and width of the map (determined by the difference between the maximum 
  and minimum longitudes/latitudes).
  }
  \item{lineSize}{width of the line that connects the points of the route in the 
  plot.
  }
  \item{lineAlpha}{opacity of the line that connects the points of the route in 
  the plot.
  }
  \item{pointSize}{size of the points of the route in the plot.
  }
  \item{pointAlpha}{opacity of the points of the route in the plot.
  }
}
\value{
A ggmap object with added paths and points representing the route.
}
\examples{
# Plot the route followed by the aircraft with ICAO address 4ca7b3 
# during the 4th of November, 2020.

\donttest{
vectors <- getAircraftStateVectorsSeries(aircraft="4ca7b3", 
startTime="2020-11-04 10:30:00", endTime="2020-11-04 12:00:00", 
timeZone="Europe/London", timeResolution=300)

plotRoute(vectors)
}
}
