gcms  <-  c("CCSM4.r6i1p1", "CNRM-CM5.r1i1p1", "CSIRO-Mk3-6-0.r2i1p1",
        "HadGEM2-CC.r1i1p1", "inmcm4.r1i1p1", "IPSL-CM5A-LR.r1i1p1",
        "MIROC5.r1i1p1", "MPI-ESM-LR.r1i1p1", "MRI-CGCM3.r1i1p1")
scenarios <- c("rcp45", "rcp85")
mtns  <-  c("CM", "DM", "GM")
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
# skyisland-climate repo. Return as a raster brick.
retrieve_reconstruction <- function(mtn, gcm=NULL, scenario=NULL, timep=NULL) {

  base_name <- paste(mtn, gcm, scenario, timep, sep="_")
  
  if(is.null(gcm) ) {
    base_name <- paste(base_name, "_19612000", ".RDS", sep="")
  } else {
    base_name <- paste(base_name, ".RDS", sep="")
  }

  res <- data.frame(readRDS(file.path(BIOCLIM_RECS_DIR, base_name)))
  soild <- data.frame(readRDS(file.path(SOIL_RECS_DIR, base_name)))
  res <- dplyr::left_join(res, soild) # merge in gswc column
  #res <- clim_data_2_brick(res)
  return(res)
}



library(ggplot2)

DM.hist <- retrieve_reconstruction("DM") %>% mutate(tp = "1961-2000")
DM.Had2080s <- retrieve_reconstruction("DM", "HadGEM2-CC.r1i1p1", "rcp85", "2080s") %>% mutate(tp="2071-2100")

DM.bioclim <- rbind(DM.hist, DM.Had2080s) #%>% mutate(tp = factor(tp, levels=c("1961-2000", "2071-2100")))
                                                                 



ggplot(DM.bioclim, aes(x,y, fill=march_may_min)) + geom_raster() + facet_grid(. ~ tp) +
  xlab("Longitude") +ylab("Latitude") +
  guides(fill=guide_legend(title="Spring minimum (C)")) +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1))
ggsave("../results/plots/DM_hist_2080s_mmmin.png", width=10, height=5)


DM.df <- topostacks[["DM"]]
DM.df <- as.data.frame(DM.df, xy=TRUE)

DM.df <- left_join(DM.hist, DM.df)

ggplot(DM.df, aes(elev, gswc)) + geom_point()
