# ------------------------------------------------
# This script checks that the appropriate packages are installed for running AquaBEHER.
# 
# ------------------------------------------------

AquaBEHER.packages <- c("devtools", "sp","rgdal","raster","ncdf4","udunits2","SciViews","foreach","doParallel", "doMC", "snow", "rworldxtra", "terra", "rgeos", "maptools", "cleangeo", "tictoc", "future")

#print(installed.packages()[,"Package"])

print("",quote=FALSE)
print("Checking for required R packages.",quote=FALSE)
print("******************************",quote=FALSE)

print("",quote=FALSE)
print("This is a unix-based OS, checking for additional R packages.",quote=FALSE)
print("******************************",quote=FALSE)
for (package in 1:length(AquaBEHER.packages)) {
        if(AquaBEHER.packages[package] %in% installed.packages()[,"Package"]) { print(paste(AquaBEHER.packages[package],"... installed.",sep=""),quote=FALSE)
        } else { print(paste(AquaBEHER.packages[package],"... not installed. Installing...",sep=""),quote=FALSE)
        install.packages(AquaBEHER.packages[package]) }
}

remotes::install_github("gowusu/sebkc")

print("",quote=FALSE)
print("******************************",quote=FALSE)
print(paste("R version ",as.character(getRversion())," detected.",sep=""),quote=FALSE)
print("Checking complete.",quote=FALSE)
