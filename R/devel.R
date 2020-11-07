# library(maptools)
# library(raster)
# library(elevatr)
# 
# help(getData)
# 
# getSingleTimeStateVectors(aircraft="780a66")
# 
# adm <- getData('GADM', country='PHL', leve=1)
# 
# latitudes <- seq(from=37.408, to=37.428, by=0.001)
# longitudes <- seq(from=-5.89301, to=-5.89321, by=-0.001)
# 
# lats_longs_datframe <- data.frame(x=longitudes, y=latitudes)
# 
# prj_dd <- "+proj=longlat"
# 
# df_elev_epqs <- get_elev_point(lats_longs_datframe, prj = prj_dd, src = "epqs")
# 
# arrivals=getAirportArrivals("LEZL", startTime = "2020-10-06 00:00:00", 
#                    endTime = "2020-10-06 23:12:00", timeZone = "Europe/Madrid")
# 
# landings = list()
# 
# for(arrival in arrivals) {
#   aircraft=arrival$ICAO24
#   initialTime=as.POSIXct(openSkies:::stringToEpochs(arrival$arrivalTime, 
#                                                     timeZone="British Summer Time")
#                          - 200, origin="1970-01-01")
#   finalTime=as.POSIXct(openSkies:::stringToEpochs(arrival$arrivalTime, 
#                                                     timeZone="British Summer Time")
#                          + 20, origin="1970-01-01")
#   new_trajectory = getAircraftStateVectorsSeries(aircraft, initialTime, finalTime, timeZone = "British Summer Time",
#                                                  timeResolution=5, username="ra3014", password="Gorbachov2006")
#   landings = c(landings, new_trajectory)
#   print(aircraft)
# } 
# 
# landing1
# 
# for(arrival in arrivals[1]) {
#   aircraft=arrival$ICAO24
#   initialTime=as.POSIXct(openSkies:::stringToEpochs(arrival$arrivalTime, 
#                                                     timeZone="British Summer Time")
#                          - 200, origin="1970-01-01")
#   finalTime=as.POSIXct(openSkies:::stringToEpochs(arrival$arrivalTime, 
#                                                   timeZone="British Summer Time")
#                        + 20, origin="1970-01-01")
#   new_trajectory = getAircraftStateVectorsSeries(aircraft, initialTime, finalTime, timeZone = "British Summer Time",
#                                                  timeResolution=5, username="ra3014", password="Gorbachov2006")
#   landing1 = new_trajectory
#   print(aircraft)
# } 
# 
# altitude=sapply(landings, "[[", "geoAltitude")
# altitude2 = altitude
# 
# 
# landings_df = data.frame(aircraft=sapply(landings, "[[", "ICAO24"),
#                          longitude=sapply(landings, "[[", "longitude"),
#                          latitude=sapply(landings, "[[", "latitude"),
#                          altitude=unlist(altitude2))
# 
# reshape(landings_df, idvar="aircraft", direction="wide")
# 
# landings_df1=landings_df[landings_df[,1]=="4ca2d7",]
# 
# lines3D(x=landings_df1$longitude, y=landings_df1$latitude, z=landings_df1$altitude)
# 
# getLandingTrajectory = function(aircraft, arrivalTime, timeZone) {
#   
# }