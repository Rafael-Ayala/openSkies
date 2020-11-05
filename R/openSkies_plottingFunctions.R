get_map_limits = function(longitudes, latitudes, margin_factor){
  min_lon = min(unlist(longitudes))
  max_lon = max(unlist(longitudes))
  min_lat = min(unlist(latitudes))
  max_lat = max(unlist(latitudes))
  width = max_lon - min_lon
  height = max_lat - min_lat
  map_limits = c(
    left = min_lon - width * margin_factor,
    right = max_lon + width * margin_factor,
    top = max_lat + height * margin_factor,
    bottom = min_lat - height * margin_factor
  )
  return(map_limits)
}

plot_route = function(state_vectors, path_color="blue", ggmap_object=NULL, plot_result=TRUE, margin_factor=0.2, line_size=1, line_alpha=0.5, point_size=0.3, point_alpha=0.8){
  longitudes = unlist(sapply(state_vectors, function(state_vector) state_vector["longitude"]))
  latitudes = unlist(sapply(state_vectors, function(state_vector) state_vector["latitude"]))
  if(length(longitudes) == 0){
    stop(strwrap("Unable to plot route: no non-NULL state vectors available.", initial="",
                 prefix="\n"))
  }
  data = data.frame(lat=latitudes, lon=longitudes)
  if (is.null(ggmap_object)){
    limits = get_map_limits(longitudes, latitudes, margin_factor)
    map = get_map(limits)
    ggmap_object = ggmap(map)
  }
  ggmap_object = ggmap_object +
    geom_path(data=data, aes(x=lon, y=lat), color=path_color, size=line_size, alpha=line_alpha) + 
    geom_point(data=data, aes(x=lon, y=lat), color=path_color, size=point_size, alpha=point_alpha)
  if(plot_result){
    ggmap_object
  }
  return(ggmap_object)
}

plot_routes = function(state_vectors_list, path_color="blue", ggmap_object=NULL, plot_result=TRUE, margin_factor=0.2, line_size=1, line_alpha=0.5, point_size=0.3, point_alpha=0.8){
  longitudes = sapply(state_vectors_list, function(state_vectors) unlist(sapply(state_vectors, function(state_vector) state_vector["longitude"])))
  latitudes = sapply(state_vectors_list, function(state_vectors) unlist(sapply(state_vectors, function(state_vector) state_vector["latitude"])))
  if(length(longitudes[!sapply(longitudes, is.null)]) == 0){
    stop(strwrap("Unable to plot routes: no route with non-NULL state vectors available.", initial="",
                 prefix="\n"))
  }
  if (is.null(ggmap_object)){
    limits = get_map_limits(longitudes, latitudes, margin_factor)
    map = get_map(limits)
    ggmap_object = ggmap(map)
  }
  data = data.frame(lat=numeric(), lon=numeric(), group=numeric()) 
  for (i in 1:length(state_vectors_list)){
    lat = latitudes[[i]]
    lon = longitudes[[i]]
    if (!is.null(lat)){
      new_data = data.frame(lat=lat, lon=lon, group=i)
      data = rbind(data, new_data)
    }
  } 
  ggmap_object =  ggmap_object +
    geom_path(data=data, aes(x=lon, y=lat, group=group), color=path_color, size=line_size, alpha=line_alpha) + 
    geom_point(data=data, aes(x=lon, y=lat), color=path_color, size=point_size, alpha=point_alpha)
  if(plot_result){
    ggmap_object
  }
  return(ggmap_object)
}
