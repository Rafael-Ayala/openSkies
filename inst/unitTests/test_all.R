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

## Test ADSBdecoder

message1 <- "8D40621D58C386435CC412692AD6"
message2 <- "8D40621D58C382D690C8AC2863A7"

# Test conversion of messages to binary

message1_bin <- ADSBDecoder$hexToBits(message1)
message2_bin <- ADSBDecoder$hexToBits(message2)

checkTrue(identical(message1_bin[1], as.raw(1)))
checkTrue(identical(message2_bin[33], as.raw(0)))

# Test sequential decoding of messages

message1_decoded <- ADSBDecoder$decodeMessage(message1_bin)
message2_decoded <- ADSBDecoder$decodeMessage(message2_bin)

checkEquals(message2_decoded$data$lon, 3.919373, tolerance = 0.0001)

# Test batch decoding of messages

all_messages_decoded <- ADSBDecoder$decodeMessages(list(message1_bin, message2_bin))

checkTrue(identical(all_messages_decoded[[2]]$icao, "0246b8"))

## Test generation of Impala shell queries

ImpalaTestQuery <-
    openSkies:::makeImpalaQueryStateVectorsInterval(
        aircraft = "0246b8",
        startTime = "2019-07-01 11:00:00",
        endTime = "2019-07-01 15:00:00",
        timeZone = "Europe/Madrid",
        minLatitude = 37.337917,
        maxLatitude = 37.447867,
        minLongitude = -6.049417,
        maxLongitude = -5.861130
    )

checkTrue(grepl("hour>=1561971600", ImpalaTestQuery))

## Test generateTimePoints

testTimePoints <- openSkies:::generateTimePoints("2019-07-01 11:00:00", 
                                                 "2019-07-01 15:00:00",
                                                 timeZone = "Europe/Madrid", 
                                                 timeResolution = 60)

checkTrue(identical(testTimePoints[241], 1561986000))

## Test getMapLimits

testMapLimits <- openSkies:::getMapLimits(c(37.337917, 37.447867),
                                          c(-6.049417, -5.861130),
                                          0.8)

checkEquals(testMapLimits["bottom"], -6.200047, tolerance = 0.0001, checkNames = FALSE)
