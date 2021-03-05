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


hclustK <- function(featuresMatrix, k, ...){
  return(list(cluster=cutree(hclust(dist(featuresMatrix), ...), k)))
}

agnesK <- function(featuresMatrix, k, ...){
  return(list(cluster=cutree(agnes(featuresMatrix, ...), k)))
}

clusterRoutesDBSCAN <- function(featuresMatrix, eps=0.5, ...) {
  res <- dbscan(featuresMatrix, eps, ...)
  return(res)
}

clusterRoutesNumberClusters <- function(featuresMatrix, numberClusters, method="kmeans", ...) {
  if(is.null(numberClusters) || numberClusters <= 1){
    if(method=="kmeans"){
      cg <- clusGap(featuresMatrix, kmeans, K.max=nrow(featuresMatrix)/2-1)
    } else if(method=="hclust"){
      cg <- clusGap(featuresMatrix, hclustK, K.max=nrow(featuresMatrix)/2-1)
    } else if(method=="fanny"){
      cg <- clusGap(featuresMatrix, fanny, K.max=nrow(featuresMatrix)/2-1)
    } else if(method=="clara"){
      cg <- clusGap(featuresMatrix, clara, K.max=nrow(featuresMatrix)/2-1)
    } else if(method=="agnes"){
      cg <- clusGap(featuresMatrix, agnesK, K.max=nrow(featuresMatrix)/2-1)
    }
    numberClusters <- maxSE(f = cg$Tab[, "gap"], SE.f = cg$Tab[, "SE.sim"])
  }
  
  if(method=="kmeans"){
    res <- kmeans(featuresMatrix, numberClusters, ...)
  } else if(method=="hclust"){
    res <- hclustK(featuresMatrix, numberClusters, ...)
  } else if(method=="fanny"){
    res <- fanny(featuresMatrix, k=numberClusters, diss=FALSE, ...)
  } else if(method=="clara"){
    res <- clara(featuresMatrix, numberClusters, ...)
  } else if(method=="agnes"){
    res <- agnesK(featuresMatrix, numberClusters, ...)
  }
  
  return(res)
}

clusterRoutes <- function(input, method="dbscan", eps=0.5, numberClusters=NULL, ...){
  if(!(method %in% c("dbscan", "kmeans", "hclust", "fanny", "clara", "agnes"))){
    stop(paste(method, " is not a valid clustering method", sep=""))
  }
  
  if(class(input)[1]=="matrix"){
    featuresMatrix = input
  } else if(class(input)[1]=="list"){
    featuresMatrix = getVectorSetListFeatures(input)
  } else {
    stop("Unsupported input type. Provide either a list of openSkiesStateVectorSet or an already computed features matrix")
  }
  
  if(method=="dbscan"){
    return(clusterRoutesDBSCAN(featuresMatrix, eps, ...))
  } else {
    return(clusterRoutesNumberClusters(featuresMatrix, numberClusters, method, ...))
  }
  
}