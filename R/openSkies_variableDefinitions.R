openskyApiRootURL <- "https://opensky-network.org/api/"

globalVariables(c("lat", "lon", "group"))

localOS <- Sys.info()["sysname"]

if (localOS == "Linux") {
  set_config(config(ssl_cipher_list="DEFAULT@SECLEVEL=1"))
}
