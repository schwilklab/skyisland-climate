# Tools for reading reconstructions from rds files, exporting ascii grids for
# arcgis, etc

library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(tidyr)
library(ggplot2)

gcms  <-  c("CCSM4.r6i1p1", "CNRM-CM5.r1i1p1", "CSIRO-Mk3-6-0.r2i1p1",
        "HadGEM2-CC.r1i1p1", "inmcm4.r1i1p1", "IPSL-CM5A-LR.r1i1p1",
        "MIROC5.r1i1p1", "MPI-ESM-LR.r1i1p1", "MRI-CGCM3.r1i1p1")
gcms.short <- "CCSM4.r6i1p1"

scenarios <- c("rcp45", "rcp85")
timeps  <-  c("ref", "2020s", "2050s", "2080s")

BIOCLIM_RECS_DIR <- "../../skyisland-climate/results/reconstructions/"
SOIL_RECS_DIR <- "../../skyisland-climate/results/soil/"


# take a data frame with x y coords in WGS84 and turn into a raster brick
clim_data_2_brick <- function(df) {
  sp::coordinates(df) <- ~ x + y # converts object to "SpatialPointsDataFrame"
  #let's be explicit about projections:
  projection(df) <- CRS("+proj=longlat +ellps=WGS84") 
  df <- as(df, "SpatialPixelsDataFrame")
  return <- raster::brick(df)
}

# function to retrieve bioclim and gswc projections by mtn range, gcm, scenario
# and time period. These data to retrieve are all stored as rds files in the
# skyisland-climate repo. Return as a data frame
retrieve_reconstruction_df <- function(mtn, gcm=NULL, scenario=NULL, timep=NULL) {

  base_name <- paste(mtn, gcm, scenario, timep, sep="_")

  if(is.null(gcm) ) {
    base_name <- paste(base_name, "_19612000", ".RDS", sep="")
  } else {
    base_name <- paste(base_name, ".RDS", sep="")
  }

  res <- data.frame(readRDS(file.path(BIOCLIM_RECS_DIR, base_name)))
  soild <- data.frame(readRDS(file.path(SOIL_RECS_DIR, base_name)))
  res <- dplyr::left_join(res, soild) # merge in gswc column
  return(res)
}

build_climate_df <- function(mtn) {
  hist_df <- retrieve_reconstruction_df(mtn)
  res <- mutate(hist_df, gcm="hist", scenario="hist", period="ref")

  for (gcm in gcms.short) {
    for (sce in scenarios) {
      for (period in timeps) {
        ndf <- retrieve_reconstruction_df(mtn, gcm, sce, period)
        ndf <- mutate(ndf, gcm=gcm, scenario=sce, period=period)
        res <- rbind(res, ndf)
      }
    }
  }

  return(res)
}


export_gtiff_reconstruction <- function(mtn, gcm=NULL, scenario=NULL, timep=NULL) {
  reconstruction <- retrieve_reconstruction_df(mtn, gcm, scenario, timep)
  reconstruction <- clim_data_2_brick(reconstruction)
  writeRaster(CM, file.path(BIOCLIM_RECS_DIR, paste(mtn, gcm, scenario, timep, sep="_")),
              bylayer=FALSE, format="GTiff")
  return()
}

# example":
# export_gtiff_reconstruction("GM")
