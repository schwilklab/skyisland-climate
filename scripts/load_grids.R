## topographic data grids

## data exported (loaded into global namespace) as "topostack", a RasterStack
## object (from package 'raster'). requires packages raster, maptools
library(raster)
#library(maptools)

source("./data-cache.R") # for get_data() function

DATA_CACHE_DIR <- "../results/tempdata"
GIS_DATA_DIR <- "../topo_grids"

# get modification time for ascii grids to know if we need to rebuild
GRID_TIMESTAMP_DM <- ymd_hms(file.info(list.files(path=file.path(GIS_DATA_DIR, "DM"),
                                    pattern = "*.asc", full.names=TRUE)[3])$mtime)
GRID_TIMESTAMP_CM <- ymd_hms(file.info(list.files(path=file.path(GIS_DATA_DIR, "CM"),
                                                  pattern = "*.asc", full.names=TRUE)[3])$mtime)
GRID_TIMESTAMP_GM <- ymd_hms(file.info(list.files(path=file.path(GIS_DATA_DIR, "GM"),
                                    pattern = "*.asc", full.names=TRUE)[3])$mtime)

readGridFolder <- function(fpath) {
    ## get list of grid files
    ascii_grids <- list.files(path=fpath, pattern = "*.asc", full.names=TRUE)
    ## Use filenames without extensions as column names
    layers <- sapply(ascii_grids, raster)
    names(layers) <- sapply(layers, names) # get colnames for list item names
    # stack layers
    topostack <- raster::stack(layers)
    return(topostack)
}

print("loading topo grid data")

# Now get a raster::stack object for each mtn range
topostacks <- list()
topostacks[["DM"]] <- get_data(file.path(DATA_CACHE_DIR, "DM-grids.rds"), GRID_TIMESTAMP_DM,
                         readGridFolder, fpath=file.path(GIS_DATA_DIR, "DM") )
topostacks[["CM"]] <- get_data(file.path(DATA_CACHE_DIR, "CM-grids.rds"), GRID_TIMESTAMP_CM,
                         readGridFolder, fpath=file.path(GIS_DATA_DIR, "CM") )
topostacks[["GM"]] <- get_data(file.path(DATA_CACHE_DIR, "GM-grids.rds"), GRID_TIMESTAMP_GM,
                         readGridFolder, fpath=file.path(GIS_DATA_DIR, "GM") )


# now get all of these raster layers in data frame format for plotting and
# analysis
topodfs <- list()
topodfs[["CM"]] <- data.frame(rasterToPoints(topostacks[["CM"]]))
topodfs[["DM"]] <- data.frame(rasterToPoints(topostacks[["DM"]]))
topodfs[["GM"]] <- data.frame(rasterToPoints(topostacks[["GM"]]))
