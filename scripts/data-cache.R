# data-cache.R

# provides the get_data() function that allows efficient laoding of data that
# is time-consuming to read from disk and trabsform or clean. We have several
# data sources (ascii gis grids, iButton time series, etc) which take
# considerable porcessing time to laod and transform to a useful format. But
# wwe also only want to store the raw data and code in git, not the
# intermediate useful forms. So the solution is to store the intermediate forms
# as .rds objects in a directory ignored by git. Then if those rds files do not
# exist already (and, optionally, if the source data is newer than the rds
# files), the code will rerun the reading and transforming. If the rds files
# exst and are new enough, the function simply returns the loaded R object.

library(lubridate)

# get_data()
# Lazy evaluation function.
# args:
#    dfile: path to file for rds R data object
#    time: time to check against.
#    func: Function to call if dfile modification time is older than time
#    ...: additional arguments handed to func.
get_data <- function(dfile, time, func, ...) {
    data.time <- ymd("1900-01-01") # earlier than any data in case file does
                                   # not even exist, below
    if(file.exists(dfile)) {
        data.time <- ymd_hms(file.info(dfile)$mtime)
    }

    if (time > data.time) {
        dots <- list(...) 
        res <- do.call(func, dots)
        saveRDS(res, file = dfile)
    } else {
        res <-  readRDS(dfile)
    }
    return(res)
}

