## topographic data grids

## data exported (loaded into global namespace) as "topostack", a RasterStack
## object (from package 'raster'). requires packages raster, maptools

source("./data-cache.R") # for get_data() function

DATA_CACHE_DIR <- "../results/tempdata"
GIS_DATA_DIR <- "../topo_grids"
PROJ_STRING <- "+proj=longlat +ellps=WGS84 +datum=WGS84"
## grid_names <- sub("[.][^.]*$", "", basename(ascii_grids))

# get modification time for ascii grids to know if we need to rebuild
GRID_TIMESTAMP_DM <- ymd_hms(file.info(list.files(path=file.path(GIS_DATA_DIR, "DM"),
                                    pattern = "*.asc", full.names=TRUE)[1])$mtime)
GRID_TIMESTAMP_CM <- ymd_hms(file.info(list.files(path=file.path(GIS_DATA_DIR, "CM"),
                                                  pattern = "*.asc", full.names=TRUE)[1])$mtime)
GRID_TIMESTAMP_GM <- ymd_hms(file.info(list.files(path=file.path(GIS_DATA_DIR, "GM"),
                                    pattern = "*.asc", full.names=TRUE)[1])$mtime)
readGrid <- function(filename) {
    colname <- sub("[.][^.]*$", "", basename(filename))
    grid <- maptools::readAsciiGrid(filename, colname=colname,
                                    proj4string=sp::CRS(PROJ_STRING))
    return(raster::raster(grid))
}

readGridFolder <- function(fpath) {
    ## get list of grid files
    ascii_grids <- list.files(path=fpath, pattern = "*.asc", full.names=TRUE)
    ## Use filenames without extensions as column names
    layers <- sapply(ascii_grids, readGrid)
    names(layers) <- sapply(layers, names) # get colnames for list item names
    # stack layers
    topostack <- raster::stack(layers)
    return(topostack)
}

# Now get a raster::stack object for each mtn range
DM.topostack <- get_data(file.path(DATA_CACHE_DIR, "DM-grids.rds"), GRID_TIMESTAMP_DM,
                         readGridFolder, fpath=file.path(GIS_DATA_DIR, "DM") )
CM.topostack <- get_data(file.path(DATA_CACHE_DIR, "CM-grids.rds"), GRID_TIMESTAMP_CM,
                         readGridFolder, fpath=file.path(GIS_DATA_DIR, "CM") )
GM.topostack <- get_data(file.path(DATA_CACHE_DIR, "GM-grids.rds"), GRID_TIMESTAMP_GM,
                         readGridFolder, fpath=file.path(GIS_DATA_DIR, "GM") )


