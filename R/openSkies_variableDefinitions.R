openskyApiRootURL <- "https://opensky-network.org/api/"
openskyAuthURL <-"https://auth.opensky-network.org/auth/realms/opensky-network/protocol/openid-connect/token"

globalVariables(c("lat", "lon", "group"))

localOS <- Sys.info()["sysname"]

if (localOS == "Linux") {
  set_config(config(ssl_cipher_list="DEFAULT@SECLEVEL=1"))
}

phase_state_values <- seq(0, 6, by = 0.01)
phase_membership_ground <- G_membership(phase_state_values, 1, 0.1)
phase_membership_climb <- G_membership(phase_state_values, 2, 0.1)
phase_membership_cruise <- G_membership(phase_state_values, 3, 0.1)
phase_membership_descent <- G_membership(phase_state_values, 4, 0.1)
phase_membership_level <- G_membership(phase_state_values, 5, 0.1)
phase_labels <- c("Ground", "Climb", "Cruise", "Descent", "Level")
