library(RUnit)

## Test getAirportArrivals

checkTrue(length(getAirportArrivals(airport="EDDF", startTime="2018-01-29 12:00:00", 
                                    endTime="2018-01-29 12:40:00", timeZone="Europe/Berlin")) == 24)

## Test getAirportDepartures

checkTrue(length(getAirportDepartures(airport="EDDF", startTime="2018-01-29 12:00:00", 
                                      endTime="2018-01-29 13:00:00", timeZone="Europe/Berlin")) == 35)

## Test getAircraftFlights

checkEquals("LEZL", getAircraftFlights("346190", startTime="2019-07-26 00:00:00", 
                                       endTime="2019-07-26 23:59:59", 
                                       timeZone="Europe/Madrid")[[5]]$arrivalAirport)

## Test getIntervalFlights

checkTrue(length(getIntervalFlights(startTime="2019-11-16 09:00:00",
                                    endTime="2019-11-16 10:00:00", 
                                    timeZone="Europe/London")) == 514)

## Test getSingleTimeStateVectors

checkEquals(30.26, getSingleTimeStateVectors(aircraft="403003", 
                                             time="2020-10-08 16:50:00", 
                                             timeZone="Europe/London")$velocity)

## Test getAircraftMetadata

checkEquals("3922e2", getAircraftMetadata("3922e2")$ICAO24)