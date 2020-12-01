plotRoute <- function(stateVectorSet, pathColor="blue", ggmapObject=NULL, 
                      plotResult=TRUE, paddingFactor=0.2, lineSize=1, 
                      lineAlpha=0.5, pointSize=0.3, pointAlpha=0.8,
                      arrowLength=0.3) {
  checkOpenSkiesStateVectorSet(stateVectorSet, checkTimeSeries=TRUE)
  longitudes <- stateVectorSet$get_values("longitude")
  latitudes <- stateVectorSet$get_values("latitude")
  if(length(longitudes) == 0 | length(latitudes) == 0) {
    stop(strwrap("Unable to plot route: no non-NULL state vectors available.", 
                 initial="", prefix="\n"))
  }
  data <- data.frame(lat=na.omit(latitudes), lon=na.omit(longitudes))
  if (is.null(ggmapObject)){
    limits <- getMapLimits(longitudes, latitudes, paddingFactor)
    map <- get_map(limits)
    ggmapObject <- ggmap(map)
  }
  ggmapObject <- ggmapObject +
    #geom_segment(data=data, aes(x=c(head(lon, n=-1), NA), y=c(head(lat, n=-1), NA),
    geom_segment(data=data, aes(x=lon, y=lat, xend=c(tail(lon, n=-1), NA), yend=c(tail(lat, n=-1), NA)), 
                 color=pathColor, size=lineSize, alpha=lineAlpha, 
                 arrow=arrow(length=unit(arrowLength, 'cm')),
                 na.rm=TRUE) + 
    geom_point(data=data, aes(x=lon, y=lat), color=pathColor, size=pointSize, alpha=pointAlpha)
  if(plotResult) {
    ggmapObject
  }
  return(ggmapObject)
}

plotRoutes <- function(stateVectorSetList, pathColors="blue", ggmapObject=NULL, 
                       plotResult=TRUE, paddingFactor=0.2, lineSize=1, 
                       lineAlpha=0.5, pointSize=0.3, pointAlpha=0.8, includeArrows=FALSE,
                       arrowLength=0.3, manualColors=TRUE) {
  for (stateVectorSet in stateVectorSetList) {
    checkOpenSkiesStateVectorSet(stateVectorSet, checkTimeSeries=TRUE)
  }
  longitudes <- lapply(stateVectorSetList, function(stateVectorSet) stateVectorSet$get_values("longitude"))
  latitudes <- lapply(stateVectorSetList, function(stateVectorSet) stateVectorSet$get_values("latitude"))
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
  breakPoints <- numeric(length(stateVectorSetList))
  for (i in 1:length(stateVectorSetList)){
    breakPoints[i] <- nrow(data)
    lat <- na.omit(latitudes[[i]])
    lon <- na.omit(longitudes[[i]])
    colorIndex <- i
    while(colorIndex>length(pathColors)){
      colorIndex <- colorIndex - length(pathColors)
    }
    pathColor <- pathColors[[colorIndex]]
    if (!is.null(lat)){
      newData <- data.frame(lat=lat, lon=lon, group=i, pathColor=pathColor)
      data <- rbind(data, newData)
    }
  }
  if(includeArrows){
    ggmapObject <- ggmapObject +
      geom_segment(data=data, aes(x=replace(lon, breakPoints, NA),
                                  y=replace(lat, breakPoints, NA), 
                                  xend=c(replace(lon, breakPoints+1, NA)[-1], NA), 
                                  yend=c(replace(lat, breakPoints+1, NA)[-1], NA), 
                                  color=as.factor(pathColor)), 
                   size=lineSize, 
                   alpha=lineAlpha, 
                   arrow=arrow(length=unit(arrowLength, 'cm')),
                   na.rm=TRUE)
  } else {
    ggmapObject <- ggmapObject +
      geom_path(data=data, aes(x=lon, y=lat, group=group, color=as.factor(pathColor)),
                size=lineSize,
                alpha=lineAlpha,
                na.rm=TRUE)
  }
  ggmapObject <- ggmapObject + 
    geom_point(data=data, aes(x=lon, y=lat, color=as.factor(pathColor)), size=pointSize, alpha=pointAlpha)
  if(manualColors){
    ggmapObject <- ggmapObject +  scale_color_manual(values=pathColors)
  }
  if(plotResult){
    ggmapObject
  }
  return(ggmapObject)
}

plotPlanes <- function(stateVectors, ggmapObject=NULL, plotResult=TRUE, 
                       paddingFactor=0.2, iconSize=1) {
  checkOpenSkiesStateVectorSet(stateVectors)
  longitudes <- stateVectors$get_values("longitude")
  latitudes <- stateVectors$get_values("latitude")
  aircrafts <- stateVectors$get_values("ICAO24")
  trueTracks <- stateVectors$get_values("true_track")
  if(length(longitudes) == 0 | length(latitudes) == 0) {
    stop(strwrap("Unable to plot location of aircrafts: no non-NULL state 
                  vectors available.", initial="", prefix="\n"))
  }
  data <- data.frame(lat=latitudes, lon=longitudes, planes=aircrafts, angles=trueTracks)
  if (is.null(ggmapObject)){
    limits <- getMapLimits(longitudes, latitudes, paddingFactor)
    iconScaleFactor <- abs(limits["right"] - limits["left"])/4.05356
    print(limits)
    map <- get_map(limits)
    ggmapObject <- ggmap(map)
  }
  plane <- magick::image_read(system.file("images", "airplane-4-48.png", package="openSkies"))
  plane <- magick::image_background(plane, "#FF000000")
  ggmapObject <- ggmapObject +
    mapply(function(x, y, angle) {
      ggmap::inset(grid::rasterGrob(magick::image_rotate(plane, angle)),
                                    xmin=x-iconSize*0.08*(1+abs(sin(angle*2*pi/180)*((2/sqrt(2))-1)))*iconScaleFactor,
                                    xmax=x+iconSize*0.08*(1+abs(sin(angle*2*pi/180)*((2/sqrt(2))-1)))*iconScaleFactor,
                                    ymin=y-iconSize*0.08*(1+abs(sin(angle*2*pi/180)*((2/sqrt(2))-1)))*iconScaleFactor,
                                    ymax=y+iconSize*0.08*(1+abs(sin(angle*2*pi/180)*((2/sqrt(2))-1)))*iconScaleFactor)
    }, longitudes, latitudes, trueTracks) 
  if(plotResult) {
    ggmapObject
  }
  return(ggmapObject)
}
