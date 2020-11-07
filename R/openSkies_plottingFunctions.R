plotRoute <- function(stateVectors, pathColor="blue", ggmapObject=NULL, 
                      plotResult=TRUE, paddingFactor=0.2, lineSize=1, 
                      lineAlpha=0.5, pointSize=0.3, pointAlpha=0.8) {
  longitudes <- unlist(sapply(stateVectors, function(stateVector) stateVector["longitude"]))
  latitudes <- unlist(sapply(stateVectors, function(stateVector) stateVector["latitude"]))
  if(length(longitudes) == 0) {
    stop(strwrap("Unable to plot route: no non-NULL state vectors available.", 
                 initial="", prefix="\n"))
  }
  data <- data.frame(lat=latitudes, lon=longitudes)
  if (is.null(ggmapObject)){
    limits <- getMapLimits(longitudes, latitudes, paddingFactor)
    map <- get_map(limits)
    ggmapObject <- ggmap(map)
  }
  ggmapObject <- ggmapObject +
    geom_path(data=data, aes(x=lon, y=lat), color=pathColor, size=lineSize, alpha=lineAlpha) + 
    geom_point(data=data, aes(x=lon, y=lat), color=pathColor, size=pointSize, alpha=pointAlpha)
  if(plotResult) {
    ggmapObject
  }
  return(ggmapObject)
}

plotRoutes <- function(stateVectorsList, pathColors="blue", ggmapObject=NULL, 
                       plotResult=TRUE, paddingFactor=0.2, lineSize=1, 
                       lineAlpha=0.5, pointSize=0.3, pointAlpha=0.8) {
  longitudes <- sapply(stateVectorsList, function(stateVectors) unlist(sapply(stateVectors, function(stateVector) stateVector["longitude"])))
  latitudes <- sapply(stateVectorsList, function(stateVectors) unlist(sapply(stateVectors, function(stateVector) stateVector["latitude"])))
  if(length(longitudes[!sapply(longitudes, is.null)]) == 0){
    stop(strwrap("Unable to plot routes: no route with non-NULL state vectors 
                  available.", initial="", prefix="\n"))
  }
  if (is.null(ggmapObject)) {
    limits <- getMapLimits(longitudes, latitudes, paddingFactor)
    map <- get_map(limits)
    ggmapObject <- ggmap(map)
  }
  data <- data.frame(lat=numeric(), lon=numeric(), group=numeric()) 
  for (i in 1:length(stateVectorsList)){
    lat <- latitudes[[i]]
    lon <- longitudes[[i]]
    pathColor <- pathColors[i %% length(pathColors) + 1]
    if (!is.null(lat)){
      newData <- data.frame(lat=lat, lon=lon, group=i, pathColor=pathColor)
      data <- rbind(data, newData)
    }
  } 
  ggmapObject <-ggmapObject +
    geom_path(data=data, aes(x=lon, y=lat, group=group, color=pathColor), size=lineSize, alpha=lineAlpha) + 
    geom_point(data=data, aes(x=lon, y=lat, color=pathColor), size=pointSize, alpha=pointAlpha) +
    scale_color_manual(values <- pathColors)
  if(plotResult){
    ggmapObject
  }
  return(ggmapObject)
}
