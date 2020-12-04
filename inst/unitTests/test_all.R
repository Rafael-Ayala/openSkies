library(RUnit)

getAirportArrivalsTest <- getAirportArrivals(airport="EDDF", startTime="2018-01-29 12:00:00", 
                                             endTime="2018-01-29 12:40:00", timeZone="Europe/Berlin")

getAirportDeparturesTest <- getAirportDepartures(airport="EDDF", startTime="2018-01-29 12:00:00", 
                                                 endTime="2018-01-29 13:00:00", timeZone="Europe/Berlin")

getAircraftFlightsTest <- getAircraftFlights("346190", startTime="2019-07-26 00:00:00", 
                                             endTime="2019-07-26 23:59:59", 
                                             timeZone="Europe/Madrid")

getIntervalFlightsTest <- getIntervalFlights(startTime="2019-11-16 09:00:00",
                                             endTime="2019-11-16 10:00:00", 
                                             timeZone="Europe/London")

getSingleTimeStateVectorsTest <- getSingleTimeStateVectors(aircraft="403003", 
                                                           time="2020-10-08 16:50:00", 
                                                           timeZone="Europe/London")

getAircraftMetadataTest <- getAircraftMetadata("3922e2")

## Test getAirportArrivals

checkTrue(length(getAirportArrivalsTest) == 24 | is.null(getAirportArrivalsTest))

## Test getAirportDepartures

checkTrue(length(getAirportDeparturesTest) == 35 | is.null(getAirportDeparturesTest))

## Test getAircraftFlights

checkTrue(identical(getAircraftFlightsTest[[5]]$destination_airport, "LEZL") | is.null(getAircraftFlightsTest))

## Test getIntervalFlights

checkTrue(length(getIntervalFlightsTest) == 514 | is.null(getIntervalFlightsTest))

## Test getSingleTimeStateVectors

checkTrue(identical(getSingleTimeStateVectorsTest$velocity, 30.26) | is.null(getSingleTimeStateVectorsTest))

## Test getAircraftMetadata

checkTrue(identical(getAircraftMetadataTest$ICAO24, "3922e2") | is.null(getAircraftMetadataTest))
