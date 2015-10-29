## topographic data grids

## data exported (loaded into global namespace) as "topostack", a RasterStack
## object (from package 'raster'). requires packages raster, maptools

GIS_DATA_DIR <- "../topo_grids"
## grid_names <- sub("[.][^.]*$", "", basename(ascii_grids))

readGrid <- function(filename) {
    colname <- sub("[.][^.]*$", "", basename(filename))
    grid <- maptools::readAsciiGrid(filename, colname=colname)
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
DM.topostack <- readGridFolder(file.path(GIS_DATA_DIR, "DM"))
# TODO: save this as an r data object like with PCAs to save time in future

# TODO: need grid data for other ranges
#CM.topostack <- readGridFolder(file.path(GIS_DATA_DIR, "CM"))
#GM.topostack <- readGridFolder(file.path(GIS_DATA_DIR, "GM"))

