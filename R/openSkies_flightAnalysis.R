findFlightPhases <- function(times, altitudes, verticalRates, speeds, window=60) {
    # First we convert altitudes to feet, vertical rates to feet/min and 
    # speeds to knots
    altitudes <- altitudes * 3.28084
    verticalRates <- verticalRates * 196.85
    speeds <- speeds * 1.94384
    times <- times - times[1]
    time_windows <- times %/% window
    defuzzed_states <- numeric(length(times))
    for(i in unique(time_windows)) {
        altitudes_window <- altitudes[time_windows == i]
        verticalRates_window <- verticalRates[time_windows == i]
        speeds_window <- speeds[time_windows == i]
        altitude <- max(min(mean(altitudes_window, na.rm=TRUE), 40000), 0)
        verticalRate <- max(min(mean(verticalRates_window, na.rm=TRUE), 4000), -4000)
        speed <- max(min(mean(speeds_window, na.rm=TRUE), 600), 0)
        H_ground <- Z_membership(altitude, 0, 200)
        H_low <- G_membership(altitude, 10000, 10000)
        H_high <- G_membership(altitude, 35000, 20000)
        climbRate_0 <- G_membership(verticalRate, 0, 100)
        climbRate_positive <- S_membership(verticalRate, 10, 1000)
        climbRate_negative <- Z_membership(verticalRate, -1000, -10)
        speed_low <- G_membership(speed, 0, 50)
        speed_mid <- G_membership(speed, 300, 100)
        speed_high <- G_membership(speed, 600, 100)
        fuzzy_ground <- pmin(min(H_ground, speed_low, climbRate_0), phase_membership_ground)
        fuzzy_climb <- pmin(min(H_low, speed_mid, climbRate_positive), phase_membership_climb)
        fuzzy_cruise <- pmin(min(H_high, speed_high, climbRate_0), phase_membership_cruise)
        fuzzy_descent <- pmin(min(H_low, speed_mid, climbRate_negative), phase_membership_descent)
        fuzzy_level <- pmin(min(H_low, speed_mid, climbRate_0), phase_membership_level)
        fuzzy_combined <- apply(
            rbind(fuzzy_ground, fuzzy_climb, fuzzy_cruise, fuzzy_descent, fuzzy_level),
            2, max
        )
        defuzzed_state <- round(phase_state_values[which.max(fuzzy_combined)])
        if(defuzzed_state < 1) defuzzed_state <- 1
        defuzzed_states[time_windows == i] <- defuzzed_state
    }
    labels <- phase_labels[defuzzed_states]
    return(labels)
}
