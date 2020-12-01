\name{plotPlanes}
\alias{plotPlanes}
\title{Plot the location of a set of aircrafts
}
\description{
Draws the location of a set of aircrafts given in their state vectors on a ggmap 
object. The planes will be oriented according to the path they are following.
}
\usage{
plotPlanes(stateVectors, ggmapObject=NULL, plotResult=TRUE, paddingFactor=0.2, 
           iconSize=1)
}
\arguments{
  \item{stateVectors}{list of state vectors. Each state vector must be 
  represented by a list with, at least, fields "longitude", "latitude" and
  "trueTrack".
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
  \item{iconSize}{scaling factor for the size of the plane icons.
  }
}
\value{
A ggmap object with added paths and points representing the route.
}
\examples{
# Plot the position of aircrafts currently flying over an area covering 
# Switzerland.

if(interactive()){
vectors <- getSingleTimeStateVectors(minLatitude=45.8389, maxLatitude=47.8229, 
                                minLongitude=5.9962, maxLongitude=10.5226)

plotPlanes(vectors)
}
}
