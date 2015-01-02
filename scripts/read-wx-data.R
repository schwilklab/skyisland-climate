## read-wx-data.R

## Read weather station data (historical or projected)

# data location
proj_data <- "~/science/projects/sky-island/projects/CSC-downscaled_projections"


read.proj.wx.data <- function(wfiles) {
    dflist <- list()
    
    for (i in wfiles) {
        bname <- unlist(strsplit(basename(i), ".", fixed=TRUE))
        wx       <- bname[1]
        var      <- bname[5]

        print(paste("reading wx file ", i))
        temp <- read.csv(i, sep="\t")
        temp$gcm <- paste(bname[2], bname[3], sep=".")
        temp$scenario <-  bname[4]
        if(is.null(dflist[[wx]])) {
            dflist[[wx]] <- temp
        } else {
            dflist[[wx]] <- merge(dflist[[wx]], temp)
        }
    }
    return(dflist) 
}




read.proj.wx.dir <- function(d){
    sfiles <- list.files(path=d, pattern="*.txt",full.names=TRUE)
    r <- read.proj.wx.data(sfiles)
    return(r)
}

res <- read.proj.wx.dir(file.path(proj_data,"pr"))
