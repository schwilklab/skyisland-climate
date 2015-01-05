## read-historical-wx-data.R

## Read historic weather station data from dropbox. Data from NCCD in csv
## format daily summaries. Export w_data to global namespace.

library(repmis) # for reading data from dropbox

## Note: getting rJava working took some setting of LD paths see
## http://stackoverflow.com/questions/12872699/error-unable-to-load-installed-packages-just-now/25932828#25932828.
## My solution, create file /etc/ld.so.conf.d/java.conf with the following
## lines:

## /usr/lib/jvm/default-java/jre/lib/amd64
## /usr/lib/jvm/default-java/jre/lib/amd64/server

## Then run sudo ldconfig


# data location
hist_data_file <- "sky-island-historical-wx-data.csv"
hist_data_id <- "ly4s9lbv1itmh06"

wx_data <- repmis::source_DropboxData(hist_data_file, hist_data_id,
                                      sep = ",", header = TRUE,
                                      na.strings = "-9999")

