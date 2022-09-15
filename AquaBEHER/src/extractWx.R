# !/usr/bin/Rscript
######################################################################################################################################################

extractWx <- function(years, rain.nc.file, tmax.nc.file, tmin.nc.file, srad.nc.file, u10.nc.file, Tdew.nc.file, xy, r) {

  require(rgdal)
  require(raster)
  require(ncdf4)
  require(udunits2)
  require(SciViews)
  require(doMC)

  # setting working projection

  my.proj <- CRS("+proj=longlat +datum=WGS84")

######################################################################################################################################################

startdate <- paste0((years - 1), "-09-01")
enddate <- paste0(years, "-08-31")

 date.vec <- format(seq.Date(as.Date(startdate), as.Date(enddate),"day"), "%Y-%m-%d")

######################################################################################################################################################
 # getting data

# ********* selecting years using cdo

system(paste('cdo seldate,', startdate, ',', enddate, ' ' , rain.nc.file, ' ',
             tmpDir(), 'rain.nc', sep=''))

data.rain <- raster::resample(brick(paste0(tmpDir(), 'rain.nc')), r, method = "bilinear")

# ***********************

system(paste('cdo seldate,', startdate, ',', enddate, ' ' , tmax.nc.file, ' ',
             tmpDir(), 'tmax.nc', sep=''))

data.tmax <- raster::resample(brick(paste0(tmpDir(), 'tmax.nc')), r, method = "bilinear")
raster::values(data.tmax) <- udunits2::ud.convert(raster::values(data.tmax), "K", "Celsius")

# ******************

system(paste('cdo seldate,', startdate, ',', enddate, ' ' , tmin.nc.file, ' ',
             tmpDir(), 'tmin.nc', sep=''))

data.tmin <- raster::resample(brick(paste0(tmpDir(), 'tmin.nc')), r, method = "bilinear")
raster::values(data.tmin) <- ud.convert(raster::values(data.tmin), "K", "Celsius")

# *******************

system(paste('cdo seldate,', startdate, ',', enddate, ' ' , srad.nc.file, ' ',
             tmpDir(), 'srad.nc', sep=''))

data.srad <- raster::resample(brick(paste0(tmpDir(), 'srad.nc')), r, method = "bilinear")
raster::values(data.srad) <- ud.convert(raster::values(data.srad), "J/m2/day", "MJ/m2/day")

# ******************

 system(paste('cdo seldate,', startdate, ',', enddate, ' ' , u10.nc.file, ' ',
              tmpDir(), 'u10.nc', sep=''))

 data.u10 <- raster::resample(brick(paste0(tmpDir(), 'u10.nc')), r, method = "bilinear")

# ******************

 system(paste('cdo seldate,', startdate, ',', enddate, ' ' , Tdew.nc.file, ' ',
              tmpDir(), 'Tdew.nc', sep=''))

 data.Tdew <- raster::resample(brick(paste0(tmpDir(), 'Tdew.nc')), r, method = "bilinear")
 raster::values(data.Tdew) <- ud.convert(raster::values(data.Tdew), "K", "Celsius")

######################################################################################################################################################
 # extract grid cells

 grid.pts <- xy
 coordinates(grid.pts) <- ~Longitude+Latitude
 proj4string(grid.pts) <- my.proj

 srad.schema <- raster::extract(data.srad, grid.pts, method = 'bilinear', na.rm = TRUE)
 rain.schema <- raster::extract(data.rain, grid.pts, method = 'bilinear', na.rm = TRUE)
 tmax.schema <- raster::extract(data.tmax, grid.pts, method = 'bilinear', na.rm = TRUE)
 tmin.schema <- raster::extract(data.tmin, grid.pts, method = 'bilinear', na.rm = TRUE)
 u10.schema <- raster::extract(data.u10, grid.pts, method = 'bilinear', na.rm = TRUE)
 Tdew.schema <- raster::extract(data.Tdew, grid.pts, method = 'bilinear', na.rm = TRUE)

 grid.srad.schemas <- data.frame(t(srad.schema))
 grid.rain.schemas <- data.frame(t(rain.schema))
 grid.tmax.schemas <- data.frame(t(tmax.schema))
 grid.tmin.schemas <- data.frame(t(tmin.schema))
 grid.u10.schemas <- data.frame(t(u10.schema))
 grid.Tdew.schemas <- data.frame(t(Tdew.schema))

######################################################################################################################################################
# ***** adjust wind speed data to standard height of 2m using a logarithmic wind speed profile (FAO-56)

 grid.u2.schemas <- grid.u10.schemas * (4.87/ln((67.8 * 10) - 5.42))

######################################################################################################################################################
# ***** DEBUGING Wx

# ***** Adjustment of Rain *************************
# *** i.e  Rainy Day: a day receving at least 1 mm of rainfall.

 grid.rain.schemas[grid.rain.schemas < 1] <- 0

# ***** Adjustment of Tmax, Tmin and Tdew (FAO-56) **********************************************
# ***** correct Tmax and Tmin in proportion to the difference (Tmin - Tdew),
# Since Tdew defines the actual vapour pressure (ea = eº (Tdew)), correcting Tdew also provides an adjustment for VPD

# ko <- 0  #

for (g in 1:ncol(grid.tmax.schemas)) {

  # T.delta <- grid.tmin.schemas[,g] - grid.Tdew.schemas[,g]
  #
  # T.delta.flg <- which( T.delta  > ko)
  # grid.tmax.schemas[T.delta.flg,g] <- grid.tmax.schemas[T.delta.flg,g] - ((T.delta[T.delta.flg] - ko)/2)
  # grid.tmin.schemas[T.delta.flg,g] <- grid.tmin.schemas[T.delta.flg,g] - ((T.delta[T.delta.flg] - ko)/2)
 # grid.Tdew.schemas[T.delta.flg,g] <- grid.Tdew.schemas[T.delta.flg,g] - ((T.delta[T.delta.flg] - ko)/2)

  flag.t <- (which(grid.tmax.schemas[,g] <= grid.tmin.schemas[,g]))
  if (length(flag.t) > 0) ( break)


}

 # ***** Adjustment of wind (FAO-56)

 grid.u2.schemas[grid.u2.schemas < 0.5] <- 0.5

######################################################################################################################################################
# ******* Actual vapor pressure (ea ) derived from dewpoint temperature

  grid.VPa.schemas <- 0.6108 * exp((17.27 * grid.Tdew.schemas)/(grid.Tdew.schemas + 237.3))

  es.tx <- 0.6108 * exp((17.27 * grid.tmax.schemas)/(grid.tmax.schemas + 237.3))
  es.tn <- 0.6108 * exp((17.27 * grid.tmin.schemas)/(grid.tmin.schemas + 237.3))

# ******* relative humidity derived from vapour pressure

 grid.RHmax.schemas <- (grid.VPa.schemas/es.tn) * 100
 grid.RHmin.schemas <- (grid.VPa.schemas/es.tx) * 100

 grid.RHmax.schemas[grid.RHmax.schemas > 99.9] <- 99.9 # *** Setting cup for RH

 # ****** DEBUGING RH

 for (g in 1:ncol(grid.tmax.schemas)) {

   flag.rh <- (which(grid.RHmax.schemas[,g] <= grid.RHmin.schemas[,g]))
   if (length(flag.rh) > 0) ( break)
 }

######################################################################################################################################################
######################################################################################################################################################

 #  registerDoParallel(cores=4)

   Wx.dF.lst <- foreach(cellID = 1:length(xy$gridID)) %do% {

                         cbind.data.frame("DATE" = factor(as.character(date.vec)),
                                          "YEAR" = strftime(date.vec, "%Y"),
                                          "DOY" =  strftime(date.vec, "%j"),
                                          "RAIN" = round(as.double(grid.rain.schemas[,cellID]),1),
                                          "TMAX" = round(as.double(grid.tmax.schemas[,cellID]),1),
                                          "TMIN" = round(as.double(grid.tmin.schemas[,cellID]),1),
                                          "SRAD" = round(as.double(grid.srad.schemas[,cellID]),1),
                                          "U2" = round(as.double(grid.u2.schemas[,cellID]),1),
                                          "Tdew" = round(as.double(grid.Tdew.schemas[,cellID]),1),
                                          "RHmax" = round(as.double(grid.RHmax.schemas[,cellID]),1),
                                          "RHmin" = round(as.double(grid.RHmin.schemas[,cellID]),1))

                   }

   return(Wx.dF.lst)

}



######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################

