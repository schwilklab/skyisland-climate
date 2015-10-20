library(raster)
source("./microclimate-topo-PCA.R")
PCAs <- loadPCAData()
tminscores <- PCAs[["DM"]]$tmin$scores # grab DM dta only for testing. TODO: fix
names(tminscores)
#refer to predicted pc surfaces. Please change random-forest-tmin.R and 
#random-forest-tmax.R code to write pc surfaces to a new folder like you
#did for topo_grids

PC_GRID_DIR <- "../results/tempdata"

# get list of grid files
pc_grids <- list.files(path=PC_GRID_DIR, pattern = "*.tif", full.names=TRUE)
pc_grids

topostack <- stack(pc_grids)
x <- predict(rasters, pca, index=1:6) # create new rasters based on PCA predictions

for(i in 1:nrow(scores)) {
  pc1s <- scores[i, "PC1"]
  pc2s <- scores[i,"PC2"]
  return(pc1s,pc2s)
}
  # map of temperature for 1 day
  tmin1 <- pc1s*predPC1_tmin + pc2s*predPC2_tmin
  plot(tmin1)
  writeRaster(predPC1, file=file.path(data_output, "predPC1_tmin.tif"), overwrite=TRUE)
  
  writeRaster(tmin1, file= .....)