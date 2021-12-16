\name{findFlightPhases}
\alias{findFlightPhases}
\title{Find the phases of a flight based on altitude, vertical rate and speed
}
\description{
Identifies the different phases of a flight based on the altitude, vertical 
rate and speed of the aircraft reported in a time series of state vectors.
Identification of flight phases is performed using a fuzzy logic approach as
described in https://arc.aiaa.org/doi/10.2514/1.I010520. Currently, five 
different phases are considered: ground, climb, cruise, descent and level flight.
Flight phase identification can also be performed by calling the \code{detect_phases}
method of an \code{\link{openSkiesFlight}} object.
}
\usage{
findFlightPhases(times, altitudes, verticalRates, speeds, window=60)
}
\arguments{
  \item{times}{vector of times in seconds corresponding to the altitude, vertical
  rate and speed values.
  }
  \item{altitudes}{vector of altitude values in meters}
  \item{verticalRates}{vector of vertical rate values in meters/second.}.
  \item{speeds}{vector of speed values (i.e., the speed at which the aircraft
  is moving with respect to the ground) in meters/second}.
  \item{window}{time window in seconds to compute mean values before detecting
  flight phases. It is recommended to apply a window in order to reduce the impact
  of spurious wrong values, but window application can be effectively turned off
  by setting this argument to 1}.
}
\value{
A character vector where each element indicates the phase corresponding to each
of the time points.
}
\examples{
# In the following example, we will retrieve all state vectors for a flight 
# along route SCX624, from Harlingen to Minneapolis. We will then identify 
# the different phases of the flight, and plot it together with altitude values.
# Note that when retrieving the state vectors, the username and password should 
# be substituted by your own, for which you should have received authorization 
# to access the OpenSky Impala shell

\dontrun{
state_vectors <- getIntervalStateVectors(aircraft = "ab3da7",
                                         startTime = "2021-12-12 04:20:00",
                                         endTime = "2021-12-12 07:40:00",
                                         username="your_username",
                                         password="your_password")

flights <- state_vectors$split_into_flights()
length(flights)

# Only one flight identified in the time period, as expected

flight <- flights[[1]]

# Let's extract the data required for detection of flight phases

data <- flight$state_vectors$get_values(c("requested_time", "baro_altitude",
                                          "vertical_rate", "velocity"))
data$requested_time <- data$requested_time - data$requested_time[1]

# We can now identify flight phases. We will use a time window of 60 s

phases <- findFlightPhases(times=data$requested_time,
                           altitudes=data$baro_altitude,
                           verticalRates=data$vertical_rate,
                           speeds=data$velocity,
                           window=60)

# We can now plot the phases together with the altitude values

library(ggplot2)
data <- cbind(data, phases)
ggplot(data[!is.na(data$baro_altitude), ], aes(x = requested_time, y = baro_altitude)) +
    geom_line() +
    geom_point(aes(color=phases))     
}
}
