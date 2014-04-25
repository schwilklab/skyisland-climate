## decagon.r

## scripts to read and clean decagon files (soil moisture probes, soil water
## potental probes, rain guages). THis is ugly because it has a lot of repeated
## code, but there were individual downlaods that needed special processing

##TODO: clean outliers
##TODO: write code for rain guage data

library(ggplot2)
library(plyr)
library(stringr)
library(lubridate)
library(reshape2)

## raw data
RAW_DATA <- "../decagon/raw"
OUTPUT <- "../decagon/merged"

## ----------------------------------------------------------------------------
## SOIL DATA

## sites: BGN (Bridge Gap North), BGS (Bridge Gap South), MDS (Madera Dam
## South), CAT (Catclaw), LCN (Lower Canyon)

## soil probes are either soil psi (psi, kpa) or volumetric water content (VWC,
## m^3 / ,^3)

## readOneLogger <- function(name) {
##    fl <- list.files(path = ".", pattern = str_c("soil[-]", name), all.files = FALSE,
##                 full.names = FALSE, recursive = FALSE,
##                 ignore.case = FALSE, include.dirs = FALSE)
##    df <- 
##  }


## BGN
## Notes
## Port 4 missing data 130218 to 130524  CHECK.

BGN.1 <- read.csv(file.path(RAW_DATA, "soil-BGN-120524.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(BGN.1) <- c("time", "p1_psi", "p2_VWC", "p3_VWC", "p4_psi")
BGN.1$time <- mdy_hm(BGN.1$time, tz = "CST")


BGN.2 <- read.csv(file.path(RAW_DATA, "soil-BGN-120824.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(BGN.2) <- c("time", "p1_psi", "p2_VWC", "p3_VWC", "p4_psi")
BGN.2$time <- dmy_hm(BGN.2$time, tz = "CST")

BGN.3 <- read.csv(file.path(RAW_DATA, "soil-BGN-130524.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(BGN.3) <- c("time", "p1_psi", "p2_VWC", "p3_VWC", "p4_psi")
BGN.3$time <- mdy_hm(BGN.3$time, tz = "CST")

BGN.4 <- read.csv(file.path(RAW_DATA, "soil-BGN-130625.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(BGN.4) <- c("time", "p1_psi", "p2_VWC", "p3_VWC", "p4_psi")
BGN.4$time <- mdy_hm(BGN.4$time, tz = "CST")

BGN.5 <- read.csv(file.path(RAW_DATA, "soil-BGN-140405.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(BGN.5) <- c("time", "p1_psi", "p2_VWC", "p3_VWC", "p4_psi")
BGN.5$time <- mdy_hm(BGN.5$time, tz = "CST")

BGN <- rbind(BGN.1,BGN.2, BGN.3, BGN.4, BGN.5)
write.csv(BGN, file.path(OUTPUT, "soil-BGN-all.csv"), row.names=FALSE)
rm(BGN.1,BGN.2, BGN.3, BGN.4, BGN.5)


## BGS
## notes:  Port 1 v/v sensor BAD data 121123 to 130523
##         New Port 2 sensor (mps2) added 130523
BGS.1 <- read.csv(file.path(RAW_DATA, "soil-BGS-120524.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(BGS.1) <- c("time", "p1_VWC", "p2_psi", "p3_psi", "p4_VWC")
BGS.1$time <- mdy_hm(BGS.1$time, tz = "CST")

BGS.2 <- read.csv(file.path(RAW_DATA, "soil-BGS-130523.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(BGS.2) <- c("time", "p1_VWC", "p2_psi", "p3_psi", "p4_VWC")
BGS.2$time <- mdy_hm(BGS.2$time, tz = "CST")

BGS.3 <- read.csv(file.path(RAW_DATA, "soil-BGS-130625.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(BGS.3) <- c("time", "p1_VWC", "p2_psi", "p3_psi", "p4_VWC")
BGS.3$time <- mdy_hm(BGS.3$time, tz = "CST")

BGS.4 <- read.csv(file.path(RAW_DATA, "soil-BGS-130813.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(BGS.4) <- c("time", "p1_VWC", "p2_psi", "p3_psi", "p4_VWC")
BGS.4$time <- dmy_hm(BGS.4$time, tz = "CST")

BGS.5 <- read.csv(file.path(RAW_DATA, "soil-BGS-140405.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:6)]
names(BGS.5) <- c("time", "p1_VWC", "p2_psi", "p2_temp", "p3_psi", "p4_VWC")
BGS.5$time <- mdy_hm(BGS.5$time, tz = "CST")


BGS <- rbind.fill(BGS.1,BGS.2, BGS.3, BGS.4, BGS.5)
write.csv(BGS, file.path(OUTPUT, "soil-BGS-all.csv"), row.names=FALSE)
rm(BGS.1,BGS.2, BGS.3, BGS.4, BGS.5)




## CAT
CAT.1 <- read.csv(file.path(RAW_DATA, "soil-CAT-121006.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(CAT.1) <- c("time", "p1_VWC", "p2_psi", "p3_psi", "p4_VWC")
CAT.1$time <- mdy_hm(CAT.1$time, tz = "CST")

CAT.2 <- read.csv(file.path(RAW_DATA, "soil-CAT-130330.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(CAT.2) <- c("time", "p1_VWC", "p2_psi", "p3_psi", "p4_VWC")
CAT.2$time <- mdy_hm(CAT.2$time, tz = "CST")

CAT.3 <- read.csv(file.path(RAW_DATA, "soil-CAT-130626.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(CAT.3) <- c("time", "p1_VWC", "p2_psi", "p3_psi", "p4_VWC")
CAT.3$time <- mdy_hm(CAT.3$time, tz = "CST")

CAT.4 <- read.csv(file.path(RAW_DATA, "soil-CAT-130812.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(CAT.4) <- c("time", "p1_VWC", "p2_psi", "p3_psi", "p4_VWC")
CAT.4$time <- mdy_hm(CAT.4$time, tz = "CST")

CAT.5 <- read.csv(file.path(RAW_DATA, "soil-CAT-140406.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(CAT.5) <- c("time", "p1_VWC", "p2_psi", "p3_psi", "p4_VWC")
CAT.5$time <- mdy_hm(CAT.5$time, tz = "CST")


CAT <- rbind.fill(CAT.1, CAT.2, CAT.3, CAT.4, CAT.5)
write.csv(CAT, file.path(OUTPUT, "soil-CAT-all.csv"), row.names=FALSE)
rm(CAT.1, CAT.2, CAT.3, CAT.4, CAT.5)


## MDS
MDS.1 <- read.csv(file.path(RAW_DATA, "soil-MDS-121006.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(MDS.1) <- c("time", "p1_psi", "p2_VWC", "p3_psi", "p4_VWC")
MDS.1$time <- mdy_hm(MDS.1$time, tz = "CST")


MDS.2 <- read.csv(file.path(RAW_DATA, "soil-MDS-130330.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(MDS.2) <- c("time", "p1_psi", "p2_VWC", "p3_psi", "p4_VWC")
MDS.2$time <- mdy_hm(MDS.2$time, tz = "CST")

MDS.3 <- read.csv(file.path(RAW_DATA, "soil-MDS-130522.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(MDS.3) <- c("time", "p1_psi", "p2_VWC", "p3_psi", "p4_VWC")
MDS.3$time <- mdy_hm(MDS.3$time, tz = "CST")

MDS.4 <- read.csv(file.path(RAW_DATA, "soil-MDS-130624.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(MDS.4) <- c("time", "p1_psi", "p2_VWC", "p3_psi", "p4_VWC")
MDS.4$time <- mdy_hm(MDS.4$time, tz = "CST")

MDS.5 <- read.csv(file.path(RAW_DATA, "soil-MDS-130812.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(MDS.5) <- c("time", "p1_psi", "p2_VWC", "p3_psi", "p4_VWC")
MDS.5$time <- mdy_hm(MDS.5$time, tz = "CST")


MDS.6 <- read.csv(file.path(RAW_DATA, "soil-MDS-140406.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:5)]
names(MDS.6) <- c("time", "p1_psi", "p2_VWC", "p3_psi", "p4_VWC")
MDS.6$time <- mdy_hm(MDS.6$time, tz = "CST")

##
MDS <- rbind.fill(MDS.1, MDS.2, MDS.3, MDS.4, MDS.5, MDS.6)
write.csv(MDS, file.path(OUTPUT, "soil-MDS-all.csv"), row.names=FALSE)
rm(MDS.1, MDS.2, MDS.3, MDS.4, MDS.5, MDS.6)


## LCN  Note that ports 3 and 4 have bad data (mps2 without software upgrade until May 2013).

LCN.1 <- read.csv(file.path(RAW_DATA, "soil-LCN-140404.csv"),
                  stringsAsFactors = FALSE, na.strings = c("NA", "#N/A", ""))[,c(1:7)]
names(LCN.1) <- c("time", "p1_VWC", "p2_VWC", "p3_psi", "p3_temp", "p4_psi", "p4_temp")
LCN.1$time <- mdy_hm(LCN.1$time, tz = "CST")

# bind
LCN <- LCN.1
write.csv(LCN, file.path(OUTPUT, "soil-LCN-all.csv"), row.names=FALSE)
rm(LCN.1)

## ok, now for melting, casting and aggregating

reshape.soil <- function(df) {
  d <- melt(df, id.vars = "time")
  s <- colsplit(d$variable, "_",  c("probe", "var"))
  d <- cbind(d,s)
  d$variable <- NULL
  d <- dcast(d, time + probe ~ var, mean, drop=TRUE, na.rm=TRUE)
  d$none <- NULL
  d
  }
  
BGN <- reshape.soil(BGN)
BGS <- reshape.soil(BGS)
MDS <- reshape.soil(MDS)
CAT <- reshape.soil(CAT)
LCN <- reshape.soil(LCN)

## use floor_date( x, "day") to get daily summary for ddply

g1 <- ggplot(BGS, aes(time, psi, color = probe)) + geom_line()

g2 <- ggplot(BGS, aes(time, VWC, color = probe)) + geom_line()



ggplot(BGN, aes(time, psi, color = probe)) + geom_line()



#### Rain gauges.
library(lubridate)

BGS.rain <-  read.csv(file.path(RAW_DATA, "rain-BGS-121006.csv"), stringsAsFactors=FALSE)
names(BGS.rain) <- c("time","precip.mm")
BGS.rain$time <- ymd_hms(BGS.rain$time)

BGS.rain2 <-  read.csv(file.path(RAW_DATA, "rain-BGS-130524.csv"), stringsAsFactors=FALSE)[,c(1:2)]
names(BGS.rain2) <- c("time","precip.mm")
BGS.rain2$time <- mdy_hm(BGS.rain2$time)

BGS.rain3 <-  read.csv(file.path(RAW_DATA, "rain-BGS-130625.csv"), stringsAsFactors=FALSE)[,c(1:2)]
names(BGS.rain3) <- c("time","precip.mm")
BGS.rain3$time <- mdy_hm(BGS.rain3$time)

BGS.rain4 <-  read.csv(file.path(RAW_DATA, "rain-BGS-140405.csv"), stringsAsFactors=FALSE)[,c(1:2)]
names(BGS.rain4) <- c("time","precip.mm")
BGS.rain4$time <- mdy_hm(BGS.rain4$time)

BGS.rain <- rbind(BGS.rain, BGS.rain2, BGS.rain3, BGS.rain4)

write.csv(BGS.rain, file.path(OUTPUT, "rain-BGS-all.csv"), row.names=FALSE)

## TODO: make summaries

## MD.rain  ##TODO


## MD.rain <- read.csv("rain-MD-120924.csv")
## names(MD.rain) <- c("time","precip.mm")
## time1 <- dmy_hm(MD.rain$time, tz="CST")
## time1[is.na(time1)] <- mdy_hm(MD.rain$time[is.na(time1)], tz="CST")
## MD.rain$time <- time1

## write.csv(MD.rain, "rain-MD-120924.csv", row.names=FALSE)
