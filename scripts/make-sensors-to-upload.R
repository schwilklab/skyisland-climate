#! /usr/bin/env Rscript
## script to take snesor master list and output a csv file in correct format
## for uploading to the garmin gps units

sensor.master <- read.csv("../microclimate/sensors.csv")
sensor.master$gone <- ifelse(is.na(sensor.master$gone), FALSE,TRUE)
clean.df <- subset(sensor.master, !gone)
clean.df <- data.frame(name=clean.df$sensor,
                       lat=clean.df$lat,
                       lon=clean.df$lon,
                       elev=clean.df$elev)
write.csv(clean.df, "../field-sheets/sensors-to-upload.csv", row.names=FALSE)
# then upload with
# gpsbabel -i unicsv -o garmin -f sensors-to-upload.csv -F usb:
