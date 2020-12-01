getVectorSetFeatures <- function(stateVectorSet, resamplingSize=15, method="fmm"){
  features = NULL
  featuresDF <- stateVectorSet$get_uniform_interpolation(resamplingSize, c("longitude", "latitude", "true_track"), method=method)
  for(i in 1:nrow(featuresDF)){
    features = c(features, as.numeric(featuresDF[i,]))
  }
  return(features)
}

getFeatures <- function(stateVectorSetList, resamplingSize=15) {
  featuresMatrix = list()
  for(stateVectorSet in stateVectorSetList){
    features = getvectorSetFeatures(stateVectorSet, resamplingSize)
    featuresMatrix[length(featuresMatrix)+1] = features
  }
}


for(i in 1916:2150){
  vectors = readRDS(paste0("StateVectors/", i, ".rds"))[2]
  print(vectors$state_vectors)
}