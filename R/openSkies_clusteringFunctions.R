getVectorSetFeatures <- function(stateVectorSet, resamplingSize=15, method="fmm", useAngles=FALSE){
  features = NULL
  if(useAngles){
    featuresDF <- stateVectorSet$get_uniform_interpolation(resamplingSize, c("longitude", "latitude", 
                                                                             "true_track"), 
                                                           method=method)
  } else {
    featuresDF <- stateVectorSet$get_uniform_interpolation(resamplingSize, c("longitude", "latitude"), 
                                                           method=method)
  }
  for(i in 1:nrow(featuresDF)){
    features = c(features, as.numeric(featuresDF[i,]))
  }
  return(features)
}

getVectorSetListFeatures <- function(stateVectorSetList, resamplingSize=15, method="fmm",
                                     scale=TRUE, useAngles=FALSE) {
  featuresMatrix <- list()
  for(stateVectorSet in stateVectorSetList){
    features <- getVectorSetFeatures(stateVectorSet, resamplingSize, method, useAngles)
    featuresMatrix[[length(featuresMatrix)+1]] = features
  }
  featuresMatrix <- do.call(rbind, featuresMatrix)
  if(scale){
    featuresMatrix <- scale(featuresMatrix)
  }
  return(featuresMatrix)
}

clusterRoutesDBSCAN <- function(featuresMatrix, eps=0.5, ...) {
  res <- dbscan(featuresMatrix, eps, ...)
  return(res)
}