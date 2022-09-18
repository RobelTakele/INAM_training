#!/usr/local/bin/Rscript
######################################################################################################################################################

rm(list=ls())

 library(rworldxtra)
 library(rgdal)
 library(raster)
 library(terra)
 library(ncdf4)
 library(udunits2)
 library(maptools)
 library(rgeos)
 library(cleangeo)
 library(tictoc)
 library(SciViews)

 library(snow)
 library(doMC)
 library(doParallel)
 library(future)

######################################################################################################################################################
# input prams

# >>>>>>>>>>>> Setting location of AquaBEHER root directory >>>>>>

 rootDir = "/home/robel/EU_FOCUSafrica/gitRepo/INAM_training/AquaBEHER/"

 # >>>>>>>>>>>> select experiment >>>>>>

 # 1. "Observed"
 # 2. "Seasonal Forecast"
 # 3. "Climate Projection"

 experiment = 1

# >>>>>>>>>>>> select country >>>>>>

# 1. "Malawi"
# 2. "Mozambique"
# 3. "Tanzania"
# 4. "South eastern Africa": i.e  c("Malawi", "Mozambique", "Tanzania")
# 5. "Custom

# if selected option is custom, the provide the location of your custom shapfile

 shp <- subset(readOGR(paste0(rootDir, "data/GADM/sPDF/gadm36_MOZ_1.shp")), NAME_1 == "Nampula")

 runDomain = 5

 # >>>>>>>>>>>> select seasons >>>>>>

 year.start = 2018    # <<<<< starting year of the run [YYYY] <<<<<<<<<<<<<<<

 year.end = 2020    # <<<<< ending year of the run [YYYY] <<<<<<<<<<<<<<<


 # >>>>>>>>>>>> select method for estimating rainy season calander >>>>>>

 # 1. "Climatic"
 # 2. "AgroClimatic"

 # rainSeas.method = 1

######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
# ***** setting prams ***************

 location.list <- list("Malawi", "Mozambique", "Tanzania", "SouthEasternAfrica", "Custom")
 experiment.list <- list("Observed", "SeasonalForecast", "ClimateProjection")

 #----------------------------------------------------------------------------------------------------
 # ***  directories
 #----------------------------------------------------------------------------------------------------

 rootDir <- rootDir
 wd <- "data/"
 soil_dir <- paste(wd,"/soilData",sep="")
 runDataDir <- paste(wd,"/runData",sep="")

 #----------------------------------------------------------------------------------------------------
 # *** source all functions
 #----------------------------------------------------------------------------------------------------

 source(paste(rootDir,"/src/extractWx.R",sep=""))
 source(paste(rootDir,"/src/calcWatBal.R",sep=""))
 source(paste(rootDir,"/src/calc_seasParams.R",sep=""))

######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
# load domain boundary
#----------------------------------------------------------------------------------------------------

 data(countriesHigh)
 sPdF <- countriesHigh

 malawi.sPdF <- spChFIDs(cleangeo::clgeo_Clean(subset(sPdF, ADMIN == "Malawi")), "Malawi")
 mozambique.sPdF <- spChFIDs(cleangeo::clgeo_Clean(subset(sPdF, ADMIN == "Mozambique")), "Mozambique")
 tanzania.sPdF <- spChFIDs(cleangeo::clgeo_Clean(subset(sPdF, ADMIN == "United Republic of Tanzania")), "United Republic of Tanzania")

 reg.list <-  list(malawi.sPdF, mozambique.sPdF, tanzania.sPdF)
 reg.sPdF <- Reduce(spRbind, reg.list)

 Domains <- list(malawi.sPdF, mozambique.sPdF, tanzania.sPdF, reg.sPdF, shp)

 dom.sPdF <- Domains[[runDomain]]

 # *** get coordinate reference System

 my.proj <- crs(dom.sPdF)

# **** creating template raster ***************************************

 r <- raster(extent(dom.sPdF), crs=my.proj, resolution=0.1, vals=1)

# ****************************************************************************************************************************************************
# ***** creating grid schema

r.SpGdF <- as(r, "SpatialGridDataFrame")
r.dF <- as.data.frame(r, xy=TRUE)

grid.schema <- data.frame(gridID = paste0("E", substr(as.character(round(r.dF$x, 2)), 1, 2),
                                          substr(as.character(round(r.dF$x, 2)), 4, 5), "S",
                                          substr(as.character(round(r.dF$y, 3)), 2, 3),
                                          substr(as.character(round(r.dF$y, 3)), 5, 6)),
                          Longitude = r.dF$x,
                          Latitude = r.dF$y)

######################################################################################################################################################
######################################################################################################################################################
# ********* list of years

 year.vec <- year.start:year.end
 season.vec <- (year.vec[[1]]):year.vec[[length(year.vec)]]

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# ********* creating run directory

location <- location.list[[runDomain]]
expTyp <- experiment.list[[experiment]]

main.runs.dir <- paste0(rootDir,"/runDirectory")
if (!file.exists(main.runs.dir)) {dir.create(main.runs.dir)}

dataOUT.dir <- paste0(rootDir,"/dataOUT")
if (!file.exists(dataOUT.dir)) {dir.create(dataOUT.dir)}

runs.dir <- paste0(rootDir,"/runDirectory/", location, "_Runs")
if (!file.exists(runs.dir)) {dir.create(runs.dir)}

exp.dir <- paste0(runs.dir, "/", expTyp, "_", year.vec[[1]], "T", year.vec[[length(year.vec)]])
if (!file.exists(exp.dir)) {dir.create(exp.dir)}

######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
#
# > > > > > > > > > Task-1: ***** extracting weather > > > > >
#
######################################################################################################################################################

rain.nc.file <- paste0(rootDir, "data/climate/observed/Rain/moz_AgERA5_rain_dly_1980T2020.nc")
tmax.nc.file <- paste0(rootDir, "data/climate/observed/Tmax/moz_AgERA5_tmax_dly_1980T2020.nc")
tmin.nc.file <- paste0(rootDir, "data/climate/observed/Tmin/moz_AgERA5_tmin_dly_1980T2020.nc")
srad.nc.file <- paste0(rootDir, "data/climate/observed/SRAD/moz_AgERA5_srad_dly_1980T2020.nc")
u10.nc.file <-  paste0(rootDir, "data/climate/observed/U10/moz_AgERA5_U10_dly_1980T2020.nc")
Tdew.nc.file <-  paste0(rootDir, "data/climate/observed/Tdew/moz_AgERA5_Tdew_dly_1980T2020.nc")

# ****************************************************************************************************************************************************

if (!dir.exists(paste(exp.dir,"/Weather/", sep=""))) {dir.create(paste(exp.dir,"/Weather/", sep=""))}

job.start <- Sys.time()

for (seas in season.vec) {

        tic("       CPU time for this cell")

        print(paste0("[",seas,"/",season.vec[length(season.vec)],"] ","Processing year: ",
                     as.character(seas)), quote = FALSE)

        cat("\n...extracting weather data for season =", seas,"\n")

        if (!file.exists(paste(exp.dir,"/Weather/", location, "_WTH_", seas,".RData",sep=""))) {

                 xy.wth <- extractWx(years = seas,
                                     rain.nc.file = rain.nc.file,
                                     tmax.nc.file = tmax.nc.file,
                                     tmin.nc.file = tmin.nc.file,
                                     srad.nc.file = srad.nc.file,
                                     u10.nc.file = u10.nc.file,
                                     Tdew.nc.file = Tdew.nc.file,
                                     xy = grid.schema,
                                     r = r)

                # *** save object

                save(list=c("xy.wth","grid.schema"),
                     file = paste(exp.dir,"/Weather/", location, "_WTH_", seas,".RData",sep=""))
        }

        job.time <- Sys.time()
        cat("\n")
        print(paste0("***********************************************************************"))
        print(paste0("Job Running Since: ", job.start, "      Now: ", job.time))
        toc()
        print(paste0("***********************************************************************"))
        cat("\n")

}


######################################################################################################################################################
######################################################################################################################################################
#
# > > > > > > > > > Task-2: ***** extracting of Elevation, rainfall threshold and Soil > > > > >
#
######################################################################################################################################################

elev <- raster(paste0(rootDir, "data/Elevation/SRTMv4_5min_elev.tif"))

elev <- raster::resample(raster::crop(elev, r), r, "bilinear")
xy.elev <- grid.schema
xy.elev$elev <- round(raster::extract(elev, data.frame(x=xy.elev$Longitude, y=xy.elev$Latitude), method = "bilinear"))
grid.schema$Elevation <- xy.elev$elev

# ***** extract rainfall threshold for locations

Rw3day <- raster::resample(brick(paste0(rootDir, "data/thresHolds/Rw3day_1991T2020.nc")), r, "bilinear")
xy.Rw3day <- grid.schema
xy.Rw3day$Rw3day <- round(raster::extract(Rw3day, data.frame(x=xy.Rw3day$Longitude, y=xy.Rw3day$Latitude)))
grid.schema$Rw3day <- xy.Rw3day$Rw3day

# *******************************

Rx20day <- raster::resample(brick(paste0(rootDir, "data/thresHolds/Rx20day_1991T2020.nc")), r, "bilinear")
xy.Rx20day <- grid.schema
xy.Rx20day$Rx20day <- round(raster::extract(Rx20day, data.frame(x=xy.Rx20day$Longitude, y=xy.Rx20day$Latitude)))
grid.schema$Rx20day <- xy.Rx20day$Rx20day

save(grid.schema, file=paste(runs.dir,"/", location, "_gridSchema.RData",sep=""))

######################################################################################################################################################
# ***** extract soil data

if (!dir.exists(paste(runs.dir,"/Soil/", sep=""))) {dir.create(paste(runs.dir,"/Soil/", sep=""))}

if (!file.exists(paste(runs.dir, "/Soil/" , location, "_Soil.RData",sep=""))) {

        root_depth <- raster::resample(brick(paste(rootDir, "/data/Soil/af_erzd__m_1km.tif",sep="")), r, "bilinear")
        xy_soil <- grid.schema
        xy_soil$rdepth <- round(raster::extract(root_depth, data.frame(x=xy_soil$Longitude, y=xy_soil$Latitude)))

        taw.mm <- raster::resample(brick(paste(rootDir, "/data/Soil/af_agg_erzd_tawcpf23mm_1km.tif",sep="")), r, "bilinear")
        xy_soil$soilcp <- round(raster::extract(taw.mm, data.frame(x=xy_soil$Longitude, y=xy_soil$Latitude)))
        xy_soil$soilcp[xy_soil$soilcp <= 5] <- NA

        save(xy_soil, file=paste(runs.dir, "/Soil/" , location, "_Soil.RData",sep=""))

} else {

        load(file=paste(runs.dir, "/Soil/" , location, "_Soil.RData",sep=""))
}

######################################################################################################################################################
# ****************************************** # calculate water balance #*****************************************************************************#
######################################################################################################################################################

load(file=paste(runs.dir, "/Soil/" , location, "_Soil.RData",sep=""))
load(file=paste(runs.dir,"/", location, "_gridSchema.RData",sep=""))

if (!dir.exists(paste(exp.dir,"/watBal/", sep=""))) {dir.create(paste(exp.dir,"/watBal/", sep=""))}

job.start <- Sys.time()

for (seas in season.vec) {

        tic("       CPU time for this cell")

        print(paste0("[",seas,"/",season.vec[length(season.vec)],"] ","Processing year: ",
                     as.character(seas)), quote = FALSE)

        cat("\n...calculating watbal for season =", seas,"\n")

        if (!file.exists(paste(exp.dir, "/watBal/", location, "_watbal_", seas,".RData",sep=""))) {

                if (exists("xy.wth")) {rm("xy.wth")}
                load(file=paste(exp.dir,"/Weather/", location, "_WTH_", seas,".RData",sep=""))

                for (x_i in 1:length(xy.wth)) {

                        xy.wth[[x_i]] <- watBal(WTH = xy.wth[[x_i]], soilWHC = xy_soil$soilcp[x_i])

                        # plot(xy.wth[[x_i]]$ERATIO*100, ty="l")
                        # lines(xy.wth[[x_i]]$ETo,col="red")
                        # lines(xy.wth[[x_i]]$RAIN,col="blue")

                }

                save(xy.wth, file=paste(exp.dir, "/watBal/", location, "_watbal_", seas,".RData",sep=""))
        }

        job.time <- Sys.time()
        cat("\n")
        print(paste0("***********************************************************************"))
        print(paste0("Job Running Since: ", job.start, "      Now: ", job.time))
        toc()
        print(paste0("***********************************************************************"))
        cat("\n")

}

######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
# ***** estimate intera-seasonal parameters

load(file=paste(runs.dir, "/Soil/" , location, "_Soil.RData",sep=""))
load(file=paste(runs.dir,"/", location, "_gridSchema.RData",sep=""))

if (!dir.exists(paste(exp.dir,"/seasCalandar/", sep=""))) {dir.create(paste(exp.dir,"/seasCalandar/", sep=""))}

if (!dir.exists(paste(rootDir,"/dataOUT/seasCalandar/",  sep="")))
{dir.create(paste(rootDir,"/dataOUT/seasCalandar/",  sep=""))}

if (!dir.exists(paste(rootDir,"/dataOUT/seasCalandar/", expTyp, sep="")))
{dir.create(paste(rootDir,"/dataOUT/seasCalandar/", expTyp, sep=""))}

if (!dir.exists(paste(rootDir,"/dataOUT/seasCalandar/", expTyp, "/Onset", sep="")))
{dir.create(paste(rootDir,"/dataOUT/seasCalandar/", expTyp, "/Onset", sep=""))}

if (!dir.exists(paste(rootDir,"/dataOUT/seasCalandar/", expTyp, "/Cessation", sep="")))
{dir.create(paste(rootDir,"/dataOUT/seasCalandar/",expTyp, "/Cessation", sep=""))}

if (!dir.exists(paste(rootDir,"/dataOUT/seasCalandar/", expTyp, "/seasDuration", sep="")))
{dir.create(paste(rootDir,"/dataOUT/seasCalandar/", expTyp, "/seasDuration", sep=""))}

######################################################################################################################################################

job.start <- Sys.time()


for (seas in season.vec) {

        min_ini.doy <- (format(as.Date(paste0((seas - 1),"/09/1")) ,"%j")) # Sep 01
        max_ini.doy <- (format(as.Date(paste0(seas,"/1/31")) ,"%j")) #  Jan 31
        max_end.doy <- (format(as.Date(paste0(seas,"/7/31")) ,"%j")) # July 31

        tic("       CPU time for this cell")

        print(paste0("[",seas,"/",season.vec[length(season.vec)],"] ","Processing year: ",
                     as.character(seas)), quote = FALSE)

        cat("\n...calculating calandar of the rainy season =",seas,"\n")

        if (!file.exists(paste(exp.dir, "/seasCalandar/", location, "_seasCalandar_", seas,".RData",sep=""))) {

                if (exists("xy.wth")) {rm("xy.wth")}

                load(file=paste(exp.dir, "/watBal/", location, "_watbal_", seas,".RData",sep=""))

                out_seas <- data.frame()
                r.SpGdF@data$layer <- NA

                for (x_i in 1:length(xy.wth)) {

                        min_ini <- as.numeric(which(xy.wth[[x_i]]$DOY == min_ini.doy))
                        max_ini <- as.numeric(which(xy.wth[[x_i]]$DOY == max_ini.doy))
                        max_end <- as.numeric(which(xy.wth[[x_i]]$DOY == max_end.doy))


                        onset.clim <- onsetClim(x = xy.wth[[x_i]], mindate = min_ini, maxdate = max_ini, Rthresh = 1, RwXday = grid.schema$Rw3day[x_i])

                        onset.agroClim <- onsetAgroClim(x = xy.wth[[x_i]], mindate = min_ini, soilcp = xy_soil$soilcp[x_i],
                                                        maxdate = max_ini, e_thresh = 0.25, AW_thr = 10)

                        min_end <- max_ini + 60
                        end.clim <- cessationClim(x = xy.wth[[x_i]], mindate = min_end, maxdate = max_end, Rthresh = 1, Rx20 = grid.schema$Rx20day[x_i])

                        end.agroClim <- cessationAgroClim(x = xy.wth[[x_i]], mindate = min_end, soilcp = xy_soil$soilcp[x_i],
                                                          maxdate = max_end, e_thresh = 0.25, AW_thr = 10)


                        seasdur.clim <- NA
                        if(!is.na(onset.clim[1]) & !is.na(end.clim[1])) {

                                seasdur.clim <- length(which(xy.wth[[x_i]]$DOY == sprintf("%03d", onset.clim[1])):
                                                               which(xy.wth[[x_i]]$DOY == sprintf("%03d", end.clim[1])))

                        }

                        seasdur.agroClim <- NA
                        if(!is.na(onset.agroClim[1]) & !is.na(end.agroClim[1])) {

                                seasdur.agroClim <- length(which(xy.wth[[x_i]]$DOY == sprintf("%03d", onset.agroClim[1])):
                                                                   which(xy.wth[[x_i]]$DOY == sprintf("%03d", end.agroClim[1])))

                        }


                        # png(paste0(rootDir, "/graphics/seasCalandar.png"), height = 7, width = 10, units = "in",
                        #      bg = "white", pointsize = 10, res = 1200)
                        #
                        # ggplot() +
                        #   geom_line(aes(y = xy.wth[[x_i]]$AVAIL, x = as.Date(xy.wth[[x_i]]$DATE)), size = 0.8, color = "grey30") +
                        #   geom_area(aes(y = xy.wth[[x_i]]$RAIN, x = as.Date(xy.wth[[x_i]]$DATE)), fill = "blue", size = 0.8, alpha = 0.7) +
                        #   geom_vline(xintercept = as.Date(xy.wth[[x_i]]$DATE[which(xy.wth[[x_i]]$DOY == sprintf("%03d", onset.clim[1]))]),
                        #              color = "darkorange3", size = 1.5, linetype=4) +
                        #   geom_vline(xintercept = as.Date(xy.wth[[x_i]]$DATE[which(xy.wth[[x_i]]$DOY == sprintf("%03d", end.clim[1]))]),
                        #              color = "gold3", size = 1.5, linetype=4) +
                        #   geom_vline(xintercept = as.Date(xy.wth[[x_i]]$DATE[which(xy.wth[[x_i]]$DOY == sprintf("%03d", onset.agroClim[1]))]),
                        #              color = "green4", size = 1.5, linetype=2) +
                        #   geom_vline(xintercept = as.Date(xy.wth[[x_i]]$DATE[which(xy.wth[[x_i]]$DOY == sprintf("%03d", end.agroClim[1]))]),
                        #              color = "seagreen3", size = 1.5, linetype=2) +
                        #
                        #   scale_x_date(date_breaks = "1 month", date_labels =  "%b-%Y")  +
                        #   scale_y_continuous(expand = c(0, 2))  +
                        #   labs(y="Moisture (mm)", x=NULL) +
                        #
                        #   theme_linedraw() +
                        #
                        #   theme(axis.title = element_text(size = 14, colour = "black", family = "Times New Roman"),
                        #         axis.text = element_text(size = 10, colour = "black", family = "Times New Roman"))
                        #
                        # dev.off()

######################################################################################################################################################

                        #output row and append to data.frame

                        out_row <- data.frame(gridID = grid.schema$gridID[x_i],
                                              Longitude = grid.schema$Longitude[x_i],
                                              Latitude = grid.schema$Latitude[x_i],
                                              Year = seas,
                                              OnsetClim = onset.clim[1],
                                              OnsetAgroClim = onset.agroClim[1],
                                              CesationClim = end.clim[1],
                                              CesationAgroClim = end.agroClim[1],
                                              SeasDurClim = seasdur.clim,
                                              SeasDurAgroClim = seasdur.agroClim,
                                              OnsetClim_index = onset.clim[2],
                                              OnsetAgroClim_index = onset.agroClim[2],
                                              CesationClim_index = end.clim[2],
                                              CesationAgroClim_index = end.agroClim[2])

                        out_seas <- rbind(out_seas,out_row)

                        #   out_seas_onset <- rbind(out_seas_onset, out_seas)

                }

# ****************************************************************************************************************************************************
                #

                save(out_seas, file=paste(exp.dir, "/seasCalandar/", location, "_seasCalandar_", seas,".RData",sep=""))

                r.SpGdF@data <- out_seas[,5:14]
                seasParams.rasBRK <- brick(r.SpGdF)
                seasParams.rasBRK[[4]] <- raster::mask(seasParams.rasBRK[[4]], seasParams.rasBRK[[2]])

                x.1 <- terra::rast(seasParams.rasBRK[[1]])
                time(x.1) <- as.Date(paste0((seas), "-06-01"))
                x.7 <- terra::rast(seasParams.rasBRK[[7]])
                time(x.7) <- as.Date(paste0((seas), "-06-01"))
                x.OC <- terra::sds(x.1, x.7)
                names(x.OC) <- c("onsetClim", "onsetClim_Index")
                units(x.OC) <- c("DOY", "days")
                longnames(x.OC) <-  c("Onset date of a rainy season based on climatological definition (DOY)",
                                      "Onset index of a rainy season based on climatological definition (days from September 1st)")

                terra::writeCDF(x = x.OC,
                                filename = paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/Onset/", location, "_onsetClim_", seas, ".nc"),
                                zname = 'time',
                                compression = 9,
                                overwrite = TRUE)

                x.2 <- terra::rast(seasParams.rasBRK[[2]])
                time(x.2) <- as.Date(paste0((seas), "-06-01"))
                x.8 <- terra::rast(seasParams.rasBRK[[8]])
                time(x.8) <- as.Date(paste0((seas), "-06-01"))
                x.OaC <- terra::sds(x.2, x.8)
                names(x.OaC) <- c("onsetAgroClim", "onsetAgroClim_Index")
                units(x.OaC) <- c("DOY", "days")
                longnames(x.OaC) <-  c("Onset date of a rainy season based on agroClimatological definition (DOY)",
                                       "Onset index of a rainy season based on agroClimatological definition (days from September 1st)")

                terra::writeCDF(x = x.OaC,
                                filename = paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/Onset/", location, "_onsetAgroClim_", seas, ".nc"),
                                zname = 'time',
                                compression = 9,
                                overwrite = TRUE)

                x.3 <- terra::rast(seasParams.rasBRK[[3]])
                time(x.3) <- as.Date(paste0((seas), "-06-01"))
                x.9 <- terra::rast(seasParams.rasBRK[[9]])
                time(x.9) <- as.Date(paste0((seas), "-06-01"))
                x.CC <- terra::sds(x.3, x.9)
                names(x.CC) <- c("CessationClim", "CessationClim_Index")
                units(x.CC) <- c("DOY", "days")
                longnames(x.CC) <-  c("Cessation date of a rainy season based on Climatological definition (DOY)",
                                      "Cessation index of a rainy season based on Climatological definition (days from September 1st)")

                terra::writeCDF(x = x.CC,
                                filename = paste0(rootDir, "/dataOUT/seasCalandar/",expTyp, "/Cessation/", location, "_CessationClim_", seas, ".nc"),
                                zname = 'time',
                                compression = 9,
                                overwrite = TRUE)

                x.4 <- terra::rast(seasParams.rasBRK[[4]])
                time(x.4) <- as.Date(paste0((seas), "-06-01"))
                x.10 <- terra::rast(seasParams.rasBRK[[10]])
                time(x.10) <- as.Date(paste0((seas), "-06-01"))
                x.CaC <- terra::sds(x.4, x.10)
                names(x.CaC) <- c("CessationAgroClim", "CessationAgroClim_Index")
                units(x.CaC) <- c("DOY", "days")
                longnames(x.CaC) <-  c("Cessation date of a rainy season based on Climatological definition (DOY)",
                                       "Cessation index of a rainy season based on Climatological definition (days from September 1st)")

                terra::writeCDF(x = x.CaC,
                                filename = paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/Cessation/", location, "_CessationAgroClim_", seas, ".nc"),
                                zname = 'time',
                                compression = 9,
                                overwrite = TRUE)

                writeRaster(brick(seasParams.rasBRK[[5]]),
                            filename = paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/seasDuration/", location, "_seasDurationClim_", seas, ".nc"),
                            format = 'CDF',
                            varname = "seasDurationClim",
                            longname = "seasDuration of a rainy season based on Climatological definition",
                            varunit = 'days',
                            zname = 'time',
                            zunit = paste0("years since ", (seas - 1), "-12-01 12:00:00"),
                            overwrite = TRUE)

                writeRaster(brick(seasParams.rasBRK[[6]]),
                            filename = paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/seasDuration/", location, "_seasDurationAgroClim_", seas, ".nc"),
                            format = 'CDF',
                            varname = "seasDurationAgroClim",
                            longname = "seasDuration of a rainy season based on agroClimatological definition",
                            varunit = 'days',
                            zname = 'time',
                            zunit = paste0("years since ", (seas - 1), "-12-01 12:00:00"),
                            overwrite = TRUE)


        }


        job.time <- Sys.time()
        cat("\n")
        print(paste0("***********************************************************************"))
        print(paste0("Job Running Since: ", job.start, "      Now: ", job.time))
        toc()
        print(paste0("***********************************************************************"))
        cat("\n")

}

######################################################################################################################################################
######################################################################################################################################################
# ***** Temporal Aggregation

system(paste('cdo mergetime ', paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/Onset/", location, "_onsetClim_*.nc"),  ' ' ,
             paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/", location, "_onsetClim_", season.vec[1],
                    "T", season.vec[length(season.vec)], ".nc"), se = ''))

system(paste('cdo mergetime ', paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/Onset/", location, "_onsetAgroClim_*.nc"),  ' ' ,
             paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/", location, "_onsetAgroClim_", season.vec[1],
                    "T", season.vec[length(season.vec)], ".nc"), se = ''))

system(paste('cdo mergetime ', paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/Cessation/", location, "_CessationClim_*.nc"),
             ' ' , paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/", location, "_CessationClim_", season.vec[1],
                          "T", season.vec[length(season.vec)], ".nc"), se = ''))

system(paste('cdo mergetime ', paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/Cessation/", location, "_CessationAgroClim_*.nc"),
             ' ' ,  paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/", location, "_CessationAgroClim_", season.vec[1],
                           "T", season.vec[length(season.vec)], ".nc"), se = ''))

system(paste('cdo mergetime ', paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/seasDuration/", location, "_seasDurationClim_*.nc"),
             ' ' , paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/", location, "_seasDurationClim_", season.vec[1],
                          "T", season.vec[length(season.vec)], ".nc"), se = ''))

system(paste('cdo mergetime ', paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/seasDuration/", location,
                                      "_seasDurationAgroClim_*.nc"),  ' ' ,
             paste0(rootDir, "/dataOUT/seasCalandar/", expTyp, "/", location, "_seasDurationAgroClim_", season.vec[1],
                    "T", season.vec[length(season.vec)], ".nc"), se = ''))


######################################################################################################################################################
######################################################################################################################################################










######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
