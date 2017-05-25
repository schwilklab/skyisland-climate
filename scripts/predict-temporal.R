# predict-temporal.R

## Author: Dylan Schwilk

## supplies historical and future PCA score.predictions list accessed by mtn
## and variable. See reconstruct-climate.R for use.

library(dplyr)
library(MuMIn)

TEMPO_RES_DIR <- "../results/tempo_mod_results/"

source("wx-data.R") # provides hist_wx_data data frame


## If running this on its own, you need to source the file below first, but in
## the main workflow, this is sourced from reconstruct-climate.R
source("./microclimate-topo-PCA.R") # provides PCAs object

PC_AXES <- c("PC1", "PC2", "PC3") #fit all three for now?

# now get for all mtns and vars. Use of MuMin means we cant wrap this in a
# function (ask me how I found that out!). So nested for loops here we go...
mtns <- c("CM", "DM", "GM")
wx_short <- filter(hist_wx_data, datet > mdy("01/01/2010"))
temporal_mods <- list()
for(mtn in mtns) {
  temporal_mods[[mtn]] <- list()
  thewx <- filter(wx_short, mtn==mtn) %>%
    filter( !(is.na(tmin) | is.na(tmax) | is.na(prcp) ))
  for( v in c("tmin", "tmax") ) {
    temporal_mods$mtn[[v]] <- list()
    train <- PCAs[[mtn]][[v]]$scores %>% inner_join(thewx)
    sink(file = file.path(TEMPO_RES_DIR, paste(mtn, "_", v, ".txt", sep="")),
           append = FALSE, split = TRUE)

    for (a in PC_AXES) {
      print(paste("Fitting model for", a))
      modstr <- paste(a,  "~ tmin + tmax + prcp + tmin:prcp + tmax:prcp")
      print(modstr)
      fullmod <- lm(modstr, data = train, na.action = "na.fail")
      allmods <- dredge(fullmod) # MuMIn function
      print(allmods)
      bmod <- get.models(allmods, subset = 1)[[1]]
      print("Best model: ")
      print(summary(bmod))
      print(anova(bmod))
      temporal_mods[[mtn]][[v]][[a]] <- bmod
      print("\n\n")
    }    
    sink(NULL)
  }
}

# clean up
rm(thewx, v, mtn, train, wx_short)

# exports temporal_mods, eg a single model is accessed:
# temporal_mods$DM$tmin$PC1


## Now get actual PCA score predictions for each mtn range, each temperature
## variable, each PCA axis and each time series (1 historical and 18 future
## projections!)

# predict PCA scores on a wx data time series (historical or projected) series
# must have columns: datet, tmin, tmax, prcp
predict_temporal_scores <- function(wx_data, mtn, var) {
  predictions <- list()
  #wx_data <-  filter(wx_data, !(is.na(tmin) | is.na(tmax) | is.na(prcp) ) )
  for (a in PC_AXES) {
    predictions[[a]] <- predict(temporal_mods[[mtn]][[var]][[a]], wx_data)
  }
  res <- data.frame(do.call(cbind, predictions))
  res$datet <- wx_data$datet
  return(res)
}



## Historical
## result stored in hist_score_predictions
hist_score_predictions <- list()
for (mtn in mtns) {
  hist_score_predictions[[mtn]] = list()
  wxd <- filter(hist_wx_data,
                mtn==mtn &  !(is.na(tmin) | is.na(tmax) | is.na(prcp) ))
  for (v in c("tmin", "tmax")) {
    print(paste(mtn, "_", v, ".txt", sep=""))
    hist_score_predictions[[mtn]][[v]] <- predict_temporal_scores(wxd, mtn, v)
  }
}

saveRDS(hist_score_predictions, file.path(TEMPO_RES_DIR, "hist_score_predictions.RDS"))


## GCM projected
## result stored in proj_score_predictions
proj_score_predictions <- list()
for(gcm in unique(proj_wx_data$gcm)) {
  proj_score_predictions[[gcm]] <- list()
  for(scenario in unique(proj_wx_data$scenario)) {
    proj_score_predictions[[gcm]][[scenario]] <- list()
    for (mtn in mtns) {
      wxd <- filter(proj_wx_data,
                    gcm == gcm & scenario == scenario &
                      mtn==mtn &  !(is.na(tmin) | is.na(tmax) | is.na(prcp) ))
      for (v in c("tmin", "tmax")) {
        print(paste(gcm, scenario, mtn, v))
        proj_score_predictions[[gcm]][[scenario]][[mtn]][[v]] <- predict_temporal_scores(wxd, mtn, v)
      }
    }
  }
}

saveRDS(proj_score_predictions, file.path(TEMPO_RES_DIR, "proj_score_predictions.RDS"))

# eg:
## > head(proj_score_predictions[["HadGEM2-CC.r1i1p1"]][["rcp45"]]$CM$tmin)
##         PC1      PC2      PC3      datet
## 1 -65.17843 3.480066 3.440567 2011-05-14
## 2 -56.37765 3.451698 3.360633 2011-05-14
## 3 -42.81894 5.325342 3.377153 2011-05-14
## 4 -61.79173 3.975548 3.446696 2011-05-14
## 5 -60.27887 3.898987 3.417984 2011-05-14
## 6 -63.69950 2.637646 3.280226 2011-05-14
