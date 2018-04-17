library(sp)
library(raster)
library(rgdal)
library(dplyr)

#library(EML)
#library(rjson)

mtns <- c("CM", "DM", "GM")
mtns.long<- list("CM"="Chisos Mountains", "DM"="Davis Mountains", "GM"="Guadalupe Mountains"
                 )
gcms  <-  c("CCSM4.r6i1p1", "CNRM-CM5.r1i1p1", "CSIRO-Mk3-6-0.r2i1p1",
        "HadGEM2-CC.r1i1p1", "inmcm4.r1i1p1", "IPSL-CM5A-LR.r1i1p1",
        "MIROC5.r1i1p1", "MPI-ESM-LR.r1i1p1", "MRI-CGCM3.r1i1p1")

scenarios <- c("rcp45", "rcp85")
timeps <- c("2020s", "2050s", "2080s", "ref")
timeps.long <- list("2020s"="2011 to 2040", "2050s"="2041 to 2070", "2080s"="2071 to 2100")

# load details
## details <- fromJSON(paste(readLines("details.JSON"), collapse=""))
## rx_md <- read.csv("./reconstruction-metadata.csv", stringsAsFactors=FALSE)
## cmip5 <- read.csv("./cmip5_mods.csv", stringsAsFactors=FALSE)


BIOCLIM_RECS_DIR <- "../results/reconstructions/"
SOIL_RECS_DIR <- "../results/soil/"
OUT_DIR <- "./export/reconstructions/"


camel_case <- function(x) {
 s <- strsplit(x, " ")[[1]]
 paste(tolower(s[1]),
       paste(toupper(substring(s[-1], 1,1)), substring(s[-1], 2), sep="", collapse=""),
       sep="")
}


capitalize <- function(x) {
   paste(toupper(substring(x, 1,1)), substring(x, 2), sep="")
}


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
# skyisland-climate repo. Return as a df
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
#  res <- clim_data_2_brick(res)
  return(res)
}


## mdfn1 <- "../microclimate/sensors-metadata.csv"
## md1 <- read_md_file(mdfn1)
## eml1 <- make_dataTable("../microclimate/sensors.csv", md1, "test")


## # try a reconstruction
## CM <- retrieve_reconstruction("CM", "CCSM4.r6i1p1", "rcp45", "2020s")
## md2 <- read_md_file("./reconstruction-metadata.csv")
## write.csv(CM, "CM_CCSM4_rcp45_2020s.csv", row.names=FALSE)
## dt2 <- make_dataTable("CM_CCSM4_rcp45_2020s.csv", md2, "Reconstructed future microclimate for th CHisos Moutnains (CM), the CCSM4 cmip5 model, the rcp45 scenario and the 2020s time period")

# historical Reconstructions
#reconstruct_md <- read_md_file("./reconstruction-metadata.csv")
#rec_data_tables = list()
for (m in mtns) {
  d <- retrieve_reconstruction(m)
  d$time_period <- "historical"
  d$gcm <- NA
  d$scenario <- NA
  fn <-  file.path(OUT_DIR, paste(m, "historical.csv", sep="_"))
  print(fn)
  write.csv(d, fn, row.names=FALSE)
#  desc <- paste("Reconstructed historical microclimate for the ",
#                                mtns.long[[m]], ".", sep="")
  #dt <- make_dataTable(fn, reconstruct_md, desc)
  #rec_data_tables[[m]] <- dt
}

# future reconstructions
for (m in mtns) {
  for(g in gcms) {
    g.short <- unlist(strsplit(g, "\\."))[1]
    for(sc in scenarios) {
      for(tp in timeps) {
        fn <-  file.path(OUT_DIR, paste(m, g.short, sc, tp, sep="_"))
        fn <- paste(fn, ".csv", sep="")
        if(!file.exists(fn)) {
          d <- retrieve_reconstruction(m, g, sc, tp)
          d$time_period <- tp
          d$gcm <- g.short
          d$scenario <- sc
#          fn <-  file.path(OUT_DIR, paste(m, g.short, sc, tp, sep="_"))
          print(fn)
#          fn <- paste(fn, ".csv", sep="")
          write.csv(d, fn, row.names=FALSE)
        }
        ## desc <-  paste("Reconstructed future microclimate for the ",
        ##                               mtns.long[[m]], " under the ", g.short,
        ##                               " earth system model and the ", sc,
        ##                               " emission scenario for the ",
        ##                               timeps.long[["tp"]], " time period.", sep="")
        ## dt <- make_dataTable(fn, reconstruct_md, desc)
        ## rec_data_tables[[paste(m, g.short, sc, tp, sep="_")]] <- dt
      }
    }
  }
}


