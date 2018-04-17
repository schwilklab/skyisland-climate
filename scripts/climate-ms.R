# analysis for micor climate manuscript

library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(tidyr)

gcms  <-  c("CCSM4.r6i1p1", "CNRM-CM5.r1i1p1", "CSIRO-Mk3-6-0.r2i1p1",
        "HadGEM2-CC.r1i1p1", "inmcm4.r1i1p1", "IPSL-CM5A-LR.r1i1p1",
        "MIROC5.r1i1p1", "MPI-ESM-LR.r1i1p1", "MRI-CGCM3.r1i1p1")
gcms.short <- "CCSM4.r6i1p1"

scenarios <- c("rcp45", "rcp85")
timeps  <-  c("ref", "2020s", "2050s", "2080s")

BIOCLIM_RECS_DIR <- "../../skyisland-climate/results/reconstructions/"
SOIL_RECS_DIR <- "../../skyisland-climate/results/soil/"

OUT_DIR <- "../results/sdms/"

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


DM <- build_climate_df("DM")

# check ref vs hist
DM.ref.hist <- filter(DM, period =="ref" & scenario!="rcp85") %>%
  select(x,y,march_may_min, gcm, period) %>% 
  spread(gcm, march_may_min)

ggplot(DM.ref.hist, aes(hist, CCSM4.r6i1p1)) + geom_point(alpha=0.5)
# super tight


ggplot(filter(DM, scenario!="hist"), aes(march_may_min, gswc, color=period)) +
  geom_point(alpha=0.3) + facet_grid(. ~ scenario)
ggsave("../results/plots/DM-freeze_gswc_hist_projected.jpg")

ggplot(filter(DM, scenario!="hist"), aes(x=BIO9, color=period)) +
  geom_density() + facet_grid(. ~ scenario)

ggplot(filter(DM, scenario!="hist"), aes(x=period, y=BIO9)) +
  geom_violin() + facet_grid(. ~ scenario)


# historical reconstruction raster bricks
CM <- retrieve_reconstruction_df("CM")
DM <- retrieve_reconstruction_df("DM")
GM <- retrieve_reconstruction_df("GM")

library(ggplot2)
GM.hist.df <- as.data.frame(GM)
GM.CCSM4.rcp85 <- as.data.frame(retrieve_reconstruction("GM", "CCSM4.r6i1p1", "rcp45", "2080s"))

GM.hist.df$period <- "hist"
GM.CCSM4.rcp85$period <- "2080s"
GM.all <- rbind(GM.hist.df, GM.CCSM4.rcp85)
ggplot(GM.all, aes(march_may_min, color=period)) + geom_density()

plot(CM$march_may_min)
