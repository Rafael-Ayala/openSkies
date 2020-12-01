getVectorSetFeatures <- function(stateVectorSet, resamplingSize=15, method="fmm", useAngles=FALSE){
  features = NULL
  if(useAngles){
    featuresDF <- stateVectorSet$get_uniform_interpolation(resamplingSize, c("longitude", "latitude", "true_track"), method=method)
  } else {
    featuresDF <- stateVectorSet$get_uniform_interpolation(resamplingSize, c("longitude", "latitude"), method=method)
  }
  
  for(i in 1:nrow(featuresDF)){
    features = c(features, as.numeric(featuresDF[i,]))
  }
  return(features)
}

getFeatures <- function(stateVectorSetList, resamplingSize=15, scale=TRUE) {
  featuresMatrix <- list()
  for(stateVectorSet in stateVectorSetList){
    features <- getVectorSetFeatures(stateVectorSet, resamplingSize)
    featuresMatrix[[length(featuresMatrix)+1]] = features
  }
  featuresMatrix <- do.call(rbind, featuresMatrix)
  if(scale){
    featuresMatrix <- scale(featuresMatrix)
  }
  return(featuresMatrix)
}

clusterRoutesDBSCAN <- function(stateVectorSetList, eps=0.5, resamplingSize=15, scale=TRUE) {
  features <- getFeatures(stateVectorSetList, resamplingSize, scale)
  res <- dbscan(features, eps)
  return(res)
}
