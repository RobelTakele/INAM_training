
######################################################################################################################################################
# ***** onset date based on climatic definition (Marteau et al., 2011; Segele1 & Lamb, 2005;  Mohamed et al., 2002 )

onsetClim <- function(x, mindate, maxdate, Rthresh, RwXday) {

  # ***** calculate rain of 3-days, if >= Rthresh then assume onset on third day
  # ***** then count number of days in next 7 days after onset that have < 1 mm rain,
  # ***** if the number of dry days > 7assume resowing, and update sowing date

  onset <- NA
  onset2 <- NA
  for (d in mindate:(nrow(x)- 12)) {

    if (is.na(onset) & !is.na(x$RAIN[1]) & !is.na(RwXday)) {

      sum3day <- sum(x$RAIN[d:(d+2)])
      rain7day <- x$RAIN[(d+3):(d+12)]
      rday7day <- length(which(rain7day < Rthresh))

      if ((sum3day) > RwXday & rday7day < 8) {

        onset <- d + 3

        }  else (next)
    }

  }

 # if (!is.na(onset) & onset > maxdate) {onset <- maxdate}

  if (!is.na(onset)) {onset2 <- as.numeric(x$DOY[onset])}
  return(c(onset2, onset))
}

######################################################################################################################################################
# ***** end of season (cessation) date based on climatic definition (Mohamed et al., 2002; SIVAKUMAR, 1988))

cessationClim <- function(x, mindate, maxdate, Rthresh = 1, Rx20) {

  #rainy day after which 20-day rain is < 5 mm
  #calculate 20-day rainfall, if < 5mm then assign as harvest date

  edate <- NA
  edate2 <- NA
  for (d in mindate:(nrow(x)-19)) {

    if (is.na(Rx20)) {Rx20 = 10}

    if (is.na(edate) & !is.na(x$RAIN[1])) {

      if (x$RAIN[d] >= Rthresh) {

        sum20day <- sum(x$RAIN[(d):(d+19)])

        if (sum20day < Rx20) {

          edate <- d + 1

        } else (next)
      }
    }
  }

 # if (!is.na(edate) & edate > maxdate) {edate <- maxdate}

  if (!is.na(edate)) {edate2 <- as.numeric(x$DOY[edate])}

  return(c(edate2, edate))
}

######################################################################################################################################################
# ***** onset date based on agroClimatic definition


onsetAgroClim <- function(x, mindate, maxdate, e_thresh, AW_thr, soilcp) {

  #season has ended once 12 consecutive nongrowing days (eratio<0.2, here) have occurred
  #calculate 12-day count, if < 5mm then assign as planting date

  onset <- NA
  onset2 <- NA
  for (d in mindate:(nrow(x)-1)) {

    if (is.na(onset) & !is.na(x$ERATIO[2]) & (x$ERATIO[d] >= e_thresh)) {

      etseq <- x$AVAIL[d:(d+28)]
      etcount <- length(which(etseq > AW_thr))

      if (etcount > 19) {

        onset <- d

      }  else (next)
    }

  }

 # if (!is.na(onset) & onset > maxdate) {onset <- maxdate}

  if (!is.na(onset)) {onset2 <- as.numeric(x$DOY[onset])}

  onset <- ifelse(soilcp < 10, NA, onset)

  return(c(onset2, onset))
}

######################################################################################################################################################
# ***** end of season (cessation) date based on climatic definition (Mohamed et al., 2002; SIVAKUMAR, 1988))

cessationAgroClim <- function(x, mindate, maxdate, e_thresh, AW_thr, soilcp) {

  #season has ended once 12 consecutive nongrowing days (eratio<0.2, here) have occurred
  #calculate 12-day count, if < 5mm then assign as

  edate <- NA
  edate2 <- NA
  for (d in mindate:(nrow(x)- 12)) {

    if (is.na(edate) & !is.na(x$ERATIO[2])  & (x$ERATIO[d] <= e_thresh)) {

      etseq <- x$AVAIL[(d):(d+12)]
      etcount <- length(which(etseq <= AW_thr))

      if (etcount > 11) {

        edate <- d+1

      } else (next)
    }
  }

 # if (!is.na(edate) & edate > maxdate) {edate <- maxdate}

  if (!is.na(edate)) {edate2 <- as.numeric(x$DOY[edate])}

  edate <- ifelse(soilcp < 10, NA, edate)

  return(c(edate2, edate))
}


######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
