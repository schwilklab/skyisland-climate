
## Sensor dataset

# move data files to export directory
# sensors
file.copy("../microclimate/sensors.csv", "./export/sensors/sensors.csv")


# daily summaries
dts <- readRDS("../results/tempdata/daily-sum.rds")
write.csv(dts, "./export/sensors/temperature-daily-summaries.csv", row.names=FALSE)




## topo vars data set
wd <- getwd()
setwd("../scripts")
source("../scripts/load_grids.R")
setwd(wd)

writeRaster(topostacks[["CM"]],"export/topo_vars/CM_topo_vars.tif","GTiff")
writeRaster(topostacks[["DM"]],"export/topo_vars/DM_topo_vars.tif","GTiff")
writeRaster(topostacks[["GM"]],"export/topo_vars/GM_topo_vars.tif","GTiff")

## CM <- raster::brick( topostacks[["CM"]] )
## writeRaster(CM,"export/topo_vars/CM_topo_vars", filename=names(CM), bylayer=TRUE, format="GTiff")
#x <- raster("export/topo_vars/CM_topo_vars.envi", format="ENVI")
#hdr(x, format="ENVI")

