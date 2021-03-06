\name{clusterRoutes}
\alias{clusterRoutes}
\title{Cluster aircraft trajectories based on positional features
}
\description{
Performs clustering of aircraft trajectories positional based on their positional
features with several available methods. The input should be either a list of \code{\link{openSkiesStateVectorSet}}
or an already computed features matrix as returned by \code{\link{getVectorSetListFeatures}}. 
If the input is a list of vector sets, features will be computed with default settings.
}
\usage{
clusterRoutes(featuresMatrix, method="dbscan", eps=0.5, numberClusters=NULL, ...)
}
\arguments{
  \item{input}{input to be clustered, given as either a list of \code{\link{openSkiesStateVectorSet}}, or a
  matrix of positional features extracted from a list of
  \code{\link{openSkiesStateVectorSet}} objects with \code{\link{getVectorSetListFeatures}},
  that will be used to identify clusters.
  }
  \item{method}{clustering method that will be applied to the positional features.
  Accepted methods are: dbscan, kmeans, hclust, fanny, clara, agnes}
  \item{eps}{Size of the epsilon neighborhood to be passed to \code{\link{dbscan}.This argument is 
  only used if the selected clustering method is dbscan.}.
  }
  \item{numberClusters}{number of expected clusters. If NULL or a value lesser than 2 is passed,
  the number of clusters will be estimated. This argument is only used if the selected clustering
  method is kmeans, hclust, fanny, clara, or agnes}
  \item{...}{additional arguments accepted by the selected clustering method}.
}
\value{
An object with clustering results, containing at least a "cluster". For additional details, 
see the documentation of \code{\link{cluster}}.
}
\examples{
if(interactive()){
# Retrieve series of state vectors for 7 instances of flights between 
# Cagliari-Elmas airport and Parma airport

vectors1=getAircraftStateVectorsSeries(aircraft="4d2219", 
startTime="2020-11-06 09:20:00", endTime="2020-11-06 10:30:00", 
timeZone="Europe/London", timeResolution=300)

vectors2=getAircraftStateVectorsSeries(aircraft="4d226c", 
startTime="2020-10-30 09:20:00", endTime="2020-10-30 10:30:00", 
timeZone="Europe/London", timeResolution=300)

vectors3=getAircraftStateVectorsSeries(aircraft="4d225b", 
startTime="2020-10-29 07:15:00", endTime="2020-10-29 08:25:00", 
timeZone="Europe/London", timeResolution=300)

vectors4=getAircraftStateVectorsSeries(aircraft="4d225b", 
startTime="2020-10-25 06:25:00", endTime="2020-10-25 07:35:00", 
timeZone="Europe/London", timeResolution=300)

vectors5=getAircraftStateVectorsSeries(aircraft="4d224e", 
startTime="2020-10-19 09:30:00", endTime="2020-10-19 10:40:00", 
timeZone="Europe/London", timeResolution=300)

vectors6=getAircraftStateVectorsSeries(aircraft="4d225b", 
startTime="2020-10-16 09:30:00", endTime="2020-10-16 10:30:00", 
timeZone="Europe/London", timeResolution=300)

vectors7=getAircraftStateVectorsSeries(aircraft="4d227d", 
startTime="2020-10-12 09:30:00", endTime="2020-10-12 10:30:00", 
timeZone="Europe/London", timeResolution=300)

# Retrieve state vectors for an outlier flight, corresponding to a flight 
# between the airports of Sevilla and Palma de Mallorca

vectors8=getAircraftStateVectorsSeries(aircraft = "4ca7b3", 
startTime="2020-11-04 10:30:00", endTime="2020-11-04 12:00:00",
timeZone="Europe/London", timeResolution=300)

## Group all the openSkiesStateVectorSet objects in a single list

vectors_list=list(vectors1, vectors2, vectors3, vectors4, vectors5, vectors6, vectors7, vectors8)

## Extract the matrix of features

features_matrix=getVectorSetListFeatures(vectors_list, scale=TRUE, useAngles=FALSE)

## Perform clustering

clustering=clusterRoutes(features_matrix, "dbscan", eps=5)

## Display clustering results with flights colored by assigned cluster

plotRoutes(vectors_list, pathColors=clustering$cluster, literalColors=FALSE)
}
}
