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

clusterRoutesDBSCAN <- function(stateVectorSetList, eps=0.5) {
  features <- getFeatures(stateVectorSetList)
  res <- dbscan(features, eps)
  return(res)
}

# 
# stateVectorsList = list()
# for(i in 1:3000){
#   vectors = readRDS(paste0("StateVectors/", i, ".rds"))$stateVectorSet
#   for(stateVector in vectors$state_vectors){
#     stateVectorsList[[length(stateVectorsList)+1]] <- stateVector
#   }
# }
# 
# groups <- openSkies:::groupByFunction(stateVectorsList, function(x) x$ICAO24)
# groups = groups[sapply(groups, function(x) length(x)>1)]
# stateVectorSetList <- sapply(groups, function(group) openSkies:::openSkiesStateVectorSet$new(group, TRUE))
# clusters <- clusterRoutesDBSCAN(stateVectorSetList)
# 
# plotRoutes(stateVectorSetList, arrowLength = 0.1, paddingFactor = 0.05, lineAlpha = 0.2, pointAlpha = 0.2)

