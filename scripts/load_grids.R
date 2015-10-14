# Load topographic data grids

# data exported (loaded into gloabl namespace) as "topostack", a RasterStack
# obect (package:raster).

GIS_DATA_DIR <- "../topo_grids"

# get list of grid files
ascii_grids <- list.files(path=GIS_DATA_DIR, pattern = "*.asc", full.names=TRUE)
# Use filenames without extensions as column names

#grid_names <- sub("[.][^.]*$", "", basename(ascii_grids))

readGrid <- function(filename) {
    colname <- sub("[.][^.]*$", "", basename(filename))
    grid <- readAsciiGrid(filename, colname=colname)
    return(raster(grid))
}

layers <- sapply(ascii_grids, readGrid)
names(layers) <- sapply(layers, names) # get colnames for list item names

# remove layer that has wrong extent. TODO FIX!
#fixed--removelayers <- layers[ names(layers) != "relelev_z"]

# stack layers
topostack <- stack(layers)


# remove intermediate files
rm(layers, ascii_grids)
