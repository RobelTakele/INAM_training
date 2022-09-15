

############################################################################################################################################
############################################################################################################################################

#the two functions below estimate the ea/ep
#based on Jones (1987)
#ea/ep: actual to potential evapotranspiration ratio
#
eabyep_calc <- function(soilcp, cropfc, avail, rain, evap) {

  avail <- min(c(avail, soilcp))
  eratio <- eabyep(soilcp, avail)
  demand <- eratio*cropfc*evap
  result <- avail + rain - demand
  runoff <- result - soilcp
  avail <- min(c(soilcp,result))
  avail <- max(c(avail,0))
  runoff <- max(c(runoff,0))

  out <- data.frame(AVAIL=avail,DEMAND=demand,ERATIO=eratio,RAIN=rain,RUNOFF=runoff)

  return(out)
}


############################################################################################################################################
############################################################################################################################################

#ea/ep function
eabyep <- function(soilcp, avail) {

  percwt <- min(c(100,avail/soilcp*100))
  percwt <- max(c(1,percwt))
  eratio <- min(c(percwt/(97-3.868*sqrt(soilcp)),1))
  return(eratio)
}


###########################################################################################################################################

#wrapper to calculate the water balance modeling variables

 watBal <- function(WTH, soilWHC)  {

   coords = grid.schema[x_i, c(2,3)]

   watBal <- WTH
   watBal$ETo <- watBal$AVAIL <-  watBal$ERATIO <-  watBal$RUNOFF <-  watBal$DEMAND <- watBal$CUM_RAIN <- NA

  if (!is.na(watBal$SRAD[1])&!is.na(watBal$SRAD[1])) {

    PET <- sebkc::ETo(Tmax = as.numeric(watBal$TMAX),
                      Tmin = as.numeric(watBal$TMIN),
                      DOY = watBal$DATE,
                      latitude = coords[, 2],
                      longitude = coords[, 1],
                      z = 2,
                      uz = as.numeric(watBal$U2),
                      altitude = grid.schema[x_i, 4],
                      RHmax = as.numeric(watBal$RHmax),
                      RHmin = as.numeric(watBal$RHmin),
                      n = NULL,
                      #  Krs = 0.16,
                      #  albedo = 0.23,
                      #   as = 0.25,
                      #  bs = 0.5,
                      Rs = as.numeric(watBal$SRAD),
                      Rn = NULL,
                      #   G = 0,
                      EF = NULL,
                      wmo = NULL,
                      airport = NULL,
                      Kc = 1,
                      surface = "alfalfa")

    watBal$ETo <- as.numeric(PET$ETo)

  }

  for (d in 1:nrow(watBal)) {

    if (d==1) {

      watBal$CUM_RAIN[d] <- watBal$RAIN[d]
      sfact <- eabyep_calc(soilcp=soilWHC, cropfc=1, avail=0, rain=watBal$RAIN[d], evap=watBal$ETo[d])
      watBal$AVAIL[d] <- sfact$AVAIL
      watBal$ERATIO[d] <- sfact$ERATIO
      watBal$RUNOFF[d] <- sfact$RUNOFF
      watBal$DEMAND[d] <- sfact$DEMAND

    } else {

      watBal$CUM_RAIN[d] <- watBal$CUM_RAIN[d-1] + watBal$RAIN[d]
      sfact <- eabyep_calc(soilcp=soilWHC,cropfc=1,avail=watBal$AVAIL[d-1],rain=watBal$RAIN[d],evap=watBal$ETo[d])
      watBal$AVAIL[d] <- sfact$AVAIL
      watBal$ERATIO[d] <- sfact$ERATIO
      watBal$RUNOFF[d] <- sfact$RUNOFF
      watBal$DEMAND[d] <- sfact$DEMAND
    }
  }

  return(watBal)
}

###########################################################################################################################################

