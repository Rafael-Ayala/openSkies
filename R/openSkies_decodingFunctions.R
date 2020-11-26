NZ <- 15

hexToBits <- function(hex) {
  stopifnot((nchar(hex) %% 2) == 0)
  if (substr(hex, 1, 2) == "0x")
    hex <- substr(hex, 3, nchar(hex))
  as.vector(sapply(seq_len(nchar(hex)/2), function(x) rev(rawToBits(as.raw(as.numeric(paste0("0x", substr(hex, (x-1)*2+1, x*2), "")))))))
}

bitsToInt <- function(bits) {
  return(sum(sapply(1:length(bits), function(i) as.integer(bits[length(bits)-i+1])*(2**(i-1)))))
}

getCallSign <- function(bits) {
  charactersTable <- "#ABCDEFGHIJKLMNOPQRSTUVWXYZ#####_###############0123456789######"
  aircraftCallSign <- ""
  for (i in 0:7){
    bitsFragment <- bits[((i*6)+1):((i*6)+6)]
    position <- bitsToInt(bitsFragment)+1
    character <- substring(charactersTable, position, position)
    aircraftCallSign <- paste0(aircraftCallSign, character)
  }
  return(aircraftCallSign)
}

NL <- function(lat){
  if(abs(lat) > 87){
    res <- 1
  } else {
    epsilon <- 1e-10
    x <- 1 - (1-cos(pi/(2*NZ))) / (cos(pi/180*lat)**2)
    if(abs(x-round(x)) <= epsilon){
      x <- round(x)
    }
    res <-  floor(2*pi / acos(x))
  }
  return(res);
}

decodeCPR <- function(cprLatEven, cprLonEven, cprLatOdd, cprLonOdd, isAirborne=TRUE){
  cprLatEven = cprLatEven / 131072
  cprLonEven = cprLonEven / 131072
  cprLatOdd = cprLatOdd / 131072
  cprLonOdd = cprLonOdd / 131072
  
  j <- floor(59 * cprLatEven - 60 * cprLatOdd + 0.5)
  dLatEven <- 360/(4*NZ)
  dLatOdd <- 360/(4*NZ-1)
  if(!isAirborne){
    dLatEven <- dLatEven / 4
    dLatOdd <- dLatOdd / 4
  }
  latEven <- dLatEven * ((j %% 60) + cprLatEven)
  latOdd <- dLatOdd * ((j %% 59) + cprLatOdd)
  if(latEven >= 270){
    latEven <- latEven - 360
  }
  if(latOdd >= 270){
    latOdd <- latOdd - 360
  }
  
  nlLatEven <- NL(latEven)
  nlLatOdd <- NL(latOdd)
  
  if(nlLatEven != nlLatOdd){
    lonEven <- NULL
    lonNull <- NULL
  } else {
    niEven <- max(nlLatEven, 1)
    dLonEven <- 360/niEven
    if(!isAirborne){
      dLonEven <- dLonEven / 4
    }
    mEven <- floor(cprLonEven*(nlLatEven-1) - cprLonOdd*nlLatEven + 0.5)
    lonEven <- dLonEven * (mEven%%niEven + cprLonEven)
    if(lonEven >= 180){
      lonEven <- lonEven - 360
    }
    
    niOdd <- max(nlLatOdd-1, 1)
    dLonOdd <- 360/niOdd
    if(!isAirborne){
      dLonOdd <- dLonOdd / 4
    }
    mOdd <- floor(cprLonEven*(nlLatOdd-1) - cprLonOdd*nlLatOdd + 0.5)
    lonOdd <- dLonOdd * (mOdd%%niOdd + cprLonOdd)
    if(lonOdd >= 180){
      lonOdd <- lonOdd - 360
    }
  }
  
  return(c(latEven, lonEven, latOdd, lonOdd))
}

decodeMovement <- function(encodedSpeed){
  speed <- NULL
  if(encodedSpeed <= 8){
    speed <- encodedSpeed * 0.125
  } else if(encodedSpeed <= 12){
    speed <- 1 + (encodedSpeed-9)*0.25
  } else if(encodedSpeed <= 38){
    speed <- 2 + (encodedSpeed-13)*0.5
  } else if(encodedSpeed <= 93){
    speed <- 15 + (encodedSpeed-39)*1.0
  } else if(encodedSpeed <= 108){
    speed <- 70 + (encodedSpeed-94)*2.0
  } else if(encodedSpeed <= 123){
    speed <- 100 + (encodedSpeed-109)*5.0
  } else {
    speed <- 175
  }
  
  return(speed)
}

lastEvenPosition <- NULL
lastOddPosition <- NULL

decodeAircraftIdentifierMessage <- function(bits){
  message <- list()
  
  df <- bitsToInt(bits[1:5])
  message$df <- df
  ca <- bitsToInt(bits[6:8])
  message$ca <- ca
  icao <- paste0(packBits(bits[9:32]), collapse="")
  message$icao <- icao
  piid <- bitsToInt(bits[89:112])
  message$pi <- piid
  
  message$data <- list()
  data <- bits[33:88]
  tc <- bitsToInt(data[1:5])
  message$data$tc <- tc
  aircraftCategory <- bitsToInt(data[6:8])
  aircraftCallsign <- getCallSign(data[9:length(data)])
  message$data$aircraftCategory <- aircraftCategory
  message$data$aircraftCallsign <- aircraftCallsign
  
  return(message)
}

decodeAirbornePositionMessage <- function(bits){
  message <- list()
  
  df <- bitsToInt(bits[1:5])
  message$df <- df
  ca <- bitsToInt(bits[6:8])
  message$ca <- ca
  icao <- paste0(packBits(bits[9:32]), collapse="")
  message$icao <- icao
  piid <- bitsToInt(bits[89:112])
  message$pi <- piid
  
  message$data <- list()
  data <- bits[33:88]
  tc <- bitsToInt(data[1:5])
  message$data$tc <- tc
  
  ss <- bitsToInt(data[6:7])
  message$data$ss <- ss
  
  nicsb <- bitsToInt(data[8])
  message$data$nicsb <- ss
  
  qBit <- bitsToInt(data[16])
  if(qBit == 1){
    alt <- bitsToInt(c(data[9:15], data[17:20])) * 25 - 1000
    message$data$alt <- alt
  }
  
  t <- bitsToInt(data[21])
  message$data$t <- t
  
  f <- bitsToInt(data[22])
  message$data$f <- f
  
  lat_cpr <- bitsToInt(data[23:39])
  message$data$lat_cpr <- lat_cpr
  
  lon_cpr <- bitsToInt(data[40:56])
  message$data$lon_cpr <- lon_cpr
  
  
  if(f == 0){
    if(!is.null(lastOddPosition) &&
       lastOddPosition$df == df &&
       lastOddPosition$data$tc == tc){
      positions <- decodeCPR(message$data$lat_cpr, 
                             message$data$lon_cpr,
                             lastOddPosition$data$lat_cpr,
                             lastOddPosition$data$lon_cpr)
      message$data$lat <- positions[1]
      message$data$lon <- positions[2]
    }
    lastEvenPosition <<- message
  } else {
    if(!is.null(lastEvenPosition) &&
       lastEvenPosition$df == df &&
       lastEvenPosition$data$tc == tc){
      positions <- decodeCPR(lastEvenPosition$data$lat_cpr, 
                             lastEvenPosition$data$lon_cpr,
                             message$data$lat_cpr,
                             message$data$lon_cpr)
      message$data$lat <- positions[3]
      message$data$lon <- positions[4]
    }
    lastOddPosition <<- message
  }
  
  return(message)
}

decodeGroundPositionMessage <- function(bits){
  message <- list()
  
  df <- bitsToInt(bits[1:5])
  message$df <- df
  ca <- bitsToInt(bits[6:8])
  message$ca <- ca
  icao <- paste0(packBits(bits[9:32]), collapse="")
  message$icao <- icao
  piid <- bitsToInt(bits[89:112])
  message$pi <- piid
  
  message$data <- list()
  data <- bits[33:88]
  tc <- bitsToInt(data[1:5])
  message$data$tc <- tc
  
  mov <- decodeMovement(bitsToInt(data[6:12]))
  message$data$mov <- mov
  
  s <- bitsToInt(data[13])
  message$data$s <- s
  
  trk <- 360*bitsToInt(data[14:20])/128
  message$data$trk <- trk
  
  t <- bitsToInt(data[21])
  message$data$t <- t
  
  f <- bitsToInt(data[22])
  message$data$f <- f
  
  lat_cpr <- bitsToInt(data[23:39])
  message$data$lat_cpr <- lat_cpr
  
  lon_cpr <- bitsToInt(data[40:56])
  message$data$lon_cpr <- lon_cpr
  
  
  if(f == 0){
    if(!is.null(lastOddPosition) &&
       lastOddPosition$df == df &&
       lastOddPosition$data$tc == tc){
      positions <- decodeCPR(message$data$lat_cpr, 
                             message$data$lon_cpr,
                             lastOddPosition$data$lat_cpr,
                             lastOddPosition$data$lon_cpr,
                             isAirborne=FALSE)
      message$data$lat <- positions[1]
      message$data$lon <- positions[2]
    }
    lastEvenPosition <<- message
  } else {
    if(!is.null(lastEvenPosition) &&
       lastEvenPosition$df == df &&
       lastEvenPosition$data$tc == tc){
      positions <- decodeCPR(lastEvenPosition$data$lat_cpr, 
                             lastEvenPosition$data$lon_cpr,
                             message$data$lat_cpr,
                             message$data$lon_cpr,
                             isAirborne=FALSE)
      message$data$lat <- positions[3]
      message$data$lon <- positions[4]
    }
    lastOddPosition <<- message
  }
  
  return(message)
}

decodeAirborneVelocityMessage <- function(bits){
  message <- list()
  
  df <- bitsToInt(bits[1:5])
  message$df <- df
  ca <- bitsToInt(bits[6:8])
  message$ca <- ca
  icao <- paste0(packBits(bits[9:32]), collapse="")
  message$icao <- icao
  piid <- bitsToInt(bits[89:112])
  message$pi <- piid
  
  message$data <- list()
  data <- bits[33:88]
  tc <- bitsToInt(data[1:5])
  message$data$tc <- tc
  st <- bitsToInt(data[6:8])
  message$data$st <- st
  
  ic <- bitsToInt(data[9])
  message$data$ic <- ic
  
  nac <- bitsToInt(data[11:13])
  message$data$nac <- nac
  
  vrsrc <- bitsToInt(data[36])
  message$data$vrsrc <- vrsrc
  
  s_vr <- bitsToInt(data[37])
  message$data$s_vr <- s_vr
  
  vr <- bitsToInt(data[38:46])
  vr <- (vr-1)*64
  message$data$vr <- vr
  
  s_dif <- bitsToInt(data[49])
  message$data$s_dif <- s_dif
  
  dif <- bitsToInt(data[50:56])
  message$data$dif <- dif
  
  if(st == 1){
    s_ew <- bitsToInt(data[14])
    message$data$s_ew <- s_ew
    
    v_ew <- bitsToInt(data[15:24])
    message$data$v_ew <- v_ew
    
    s_ns <- bitsToInt(data[25])
    message$data$s_ns <- s_ns
    
    v_ns <- bitsToInt(data[26:35])
    message$data$v_ns <- v_ns
    
    if(s_ew == 1){
      vwe <- -1*(v_ew-1)
    } else {
      vwe <- v_ew-1
    }
    
    if(s_ns == 1){
      vsn <- -1*(v_ns-1)
    } else {
      vsn <- v_ns-1
    }
    
    v <- sqrt(vwe**2 + vsn**2)
    h <- atan2(vwe, vsn)*360/(2*pi)
    
    message$data$v <- v
    message$data$h <- h
    
  } else if(st == 3){
    s_hdg <- bitsToInt(data[14])
    message$data$s_hdg <- s_hdg
    
    hdg <- bitsToInt(data[15:24])
    message$data$hdg <- hdg
    
    if(s_hdg == 1){
      h <- hdg/1024*360
      message$data$h <- h
    }
    
    as_t <- bitsToInt(data[25])
    message$data$as_t <- as_t
    
    v <- bitsToInt(data[26:35])
    message$data$v <- v
  }
}

decodeOperationStatusMessage <- function(bits){
  message <- list()
  
  df <- bitsToInt(bits[1:5])
  message$df <- df
  ca <- bitsToInt(bits[6:8])
  message$ca <- ca
  icao <- paste0(packBits(bits[9:32]), collapse="")
  message$icao <- icao
  piid <- bitsToInt(bits[89:112])
  message$pi <- piid
  
  message$data <- list()
  data <- bits[33:88]
  tc <- bitsToInt(data[1:5])
  message$data$tc <- tc
  
  subtype <- bitsToInt(data[6:8])
  message$data$subtype <- subtype
  
  if(subtype == 0){
    acc_codes <- bitsToInt(data[9:24])
    message$data$acc_codes <- acc_codes
  } else {
    scc_codes <- bitsToInt(data[9:20])
    message$data$scc_codes <- scc_codes
    
    lw_codes <- bitsToInt(data[21:24])
    message$data$lw_codes <- lw_codes
  }
  
  op_mode_code <- bitsToInt(data[25:40])
  message$data$op_mode_code <- op_mode_code
  
  adsb_version <- bitsToInt(data[41:43])
  message$data$adsb_version <- adsb_version
  
  nicsa <- bitsToInt(data[44])
  message$data$nicsa <- nicsa
  
  nacp <- bitsToInt(data[45:48])
  message$data$nacp <- nacp
  
  if(subtype == 0){
    gva <- bitsToInt(data[49:50])
    message$data$gva <- gva
  }
  
  sil <- bitsToInt(data[51:52])
  message$data$sil <- sil
  
  if(subtype == 0){
    nic_baro <- bitsToInt(data[53])
    message$data$nic_baro <- nic_baro
  } else {
    trk_hdg <- bitsToInt(data[53])
    message$data$trk_hdg <- trk_hdg
  }
  
  hrd <- bitsToInt(data[54])
  message$data$hrd <- hrd
  
  silsb <- bitsToInt(data[55])
  message$data$silsb <- silsb
  
  return(message)
}

decodeMessage <- function(bits){
  
  data <- bits[33:88]
  tc <- bitsToInt(data[1:5])
  
  if(tc <= 4){
    message <- decodeAircraftIdentifierMessage(bits)
    
  } else if(tc <= 8){
    # Ground position
    message <- decodeGroundPositionMessage(bits)
    
  } else if(tc <= 18){
    # Airborne position
    message <- decodeAirbornePositionMessage(bits)
    
  } else if(tc <= 19){
    # Airborne velocity
    message <- decodeAirborneVelocityMessage(bits)
    
  } else if(tc <= 22){
    
  } else if(tc <= 27){
    
  } else if(tc <= 28){
    
  } else if(tc <= 29){
    
  } else {
    # Operation status
    message <- decodeOperationStatusMessage(bits)
  }
  
  return(message)
}

decodeMessages <- function(messages){
  decodedMessages <- vector("list", length(messages))
  for(i in 1:length(messages)){
    bits <- messages[[i]]
    tc <- bitsToInt(bits[33:37])
    
    if(tc > 4 && tc <= 8){
      # Ground position
      
      decodedMessages[[i]] <- list()
      
      df <- bitsToInt(bits[1:5])
      decodedMessages[[i]]$df <- df
      ca <- bitsToInt(bits[6:8])
      decodedMessages[[i]]$ca <- ca
      icao <- paste0(packBits(bits[9:32]), collapse="")
      decodedMessages[[i]]$icao <- icao
      piid <- bitsToInt(bits[89:112])
      decodedMessages[[i]]$pi <- piid
      
      decodedMessages[[i]]$data <- list()
      data <- bits[33:88]
      tc <- bitsToInt(data[1:5])
      decodedMessages[[i]]$data$tc <- tc
      
      mov <- decodeMovement(bitsToInt(data[6:12]))
      decodedMessages[[i]]$data$mov <- mov
      
      s <- bitsToInt(data[13])
      decodedMessages[[i]]$data$s <- s
      
      trk <- 360*bitsToInt(data[14:20])/128
      decodedMessages[[i]]$data$trk <- trk
      
      t <- bitsToInt(data[21])
      decodedMessages[[i]]$data$t <- t
      
      f <- bitsToInt(data[22])
      decodedMessages[[i]]$data$f <- f
      
      lat_cpr <- bitsToInt(data[23:39])
      decodedMessages[[i]]$data$lat_cpr <- lat_cpr
      
      lon_cpr <- bitsToInt(data[40:56])
      decodedMessages[[i]]$data$lon_cpr <- lon_cpr
      
      if(f == 0){
        evenIndex <- i;
      } else {
        oddIndex <- i;
      }
      
      j=i-1;
      while(j>0){
        formerMessage <- decodedMessages[[j]]
        if(formerMessage$icao==icao && 
           formerMessage$df==df && 
           formerMessage$data$tc==tc && 
           formerMessage$data$f != f){
          if(f == 0){
            oddIndex <- j;
          } else {
            evenIndex <- j;
          }
          
          evenMessage <- decodedMessages[[evenIndex]]
          oddMessage <- decodedMessages[[oddIndex]]
          
          positions <- decodeCPR(evenMessage$data$lat_cpr, 
                                 evenMessage$data$lon_cpr,
                                 oddMessage$data$lat_cpr,
                                 oddMessage$data$lon_cpr,
                                 isAirborne=FALSE)
          
          decodedMessages[[evenIndex]]$data$lat <- positions[1]
          decodedMessages[[evenIndex]]$data$lon <- positions[2]
          decodedMessages[[oddIndex]]$data$lat <- positions[3]
          decodedMessages[[oddIndex]]$data$lon <- positions[4]
          break
        }
        j <- j-1
      }
    }else if(tc > 8 && tc <= 18){
      # Airborne position
      
      decodedMessages[[i]] <- list()
      
      df <- bitsToInt(bits[1:5])
      decodedMessages[[i]]$df <- df
      ca <- bitsToInt(bits[6:8])
      decodedMessages[[i]]$ca <- ca
      icao <- paste0(packBits(bits[9:32]), collapse="")
      decodedMessages[[i]]$icao <- icao
      piid <- bitsToInt(bits[89:112])
      decodedMessages[[i]]$piid <- piid
      
      decodedMessages[[i]]$data <- list()
      data <- bits[33:88]
      tc <- bitsToInt(data[1:5])
      decodedMessages[[i]]$data$tc <- tc
      
      ss <- bitsToInt(data[6:7])
      decodedMessages[[i]]$data$ss <- ss
      
      nicsb <- bitsToInt(data[8])
      decodedMessages[[i]]$data$nicsb <- ss
      
      qBit <- bitsToInt(data[16])
      if(qBit == 1){
        alt <- bitsToInt(c(data[9:15], data[17:20])) * 25 - 1000
        decodedMessages[[i]]$data$alt <- alt
      }
      
      t <- bitsToInt(data[21])
      decodedMessages[[i]]$data$t <- t
      
      f <- bitsToInt(data[22])
      decodedMessages[[i]]$data$f <- f
      
      lat_cpr <- bitsToInt(data[23:39])
      decodedMessages[[i]]$data$lat_cpr <- lat_cpr
      
      lon_cpr <- bitsToInt(data[40:56])
      decodedMessages[[i]]$data$lon_cpr <- lon_cpr
      
      if(f == 0){
        evenIndex <- i;
      } else {
        oddIndex <- i;
      }
      
      j=i-1;
      while(j>0){
        formerMessage <- decodedMessages[[j]]
        if(formerMessage$icao==icao && 
           formerMessage$df==df && 
           formerMessage$data$tc==tc && 
           formerMessage$data$f != f){
          if(f == 0){
            oddIndex <- j;
          } else {
            evenIndex <- j;
          }
          
          evenMessage <- decodedMessages[[evenIndex]]
          oddMessage <- decodedMessages[[oddIndex]]
          
          positions <- decodeCPR(evenMessage$data$lat_cpr, 
                                 evenMessage$data$lon_cpr,
                                 oddMessage$data$lat_cpr,
                                 oddMessage$data$lon_cpr)
          
          decodedMessages[[evenIndex]]$data$lat <- positions[1]
          decodedMessages[[evenIndex]]$data$lon <- positions[2]
          decodedMessages[[oddIndex]]$data$lat <- positions[3]
          decodedMessages[[oddIndex]]$data$lon <- positions[4]
          
          break
        }
        j <- j-1
      }
      
    } else {
      decodedMessages[[i]] <- decodeMessage(bits)
    }
  }
  
  return(decodedMessages)
}