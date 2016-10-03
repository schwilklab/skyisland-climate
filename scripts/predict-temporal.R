# predict-temporal.R

## Author: Dylan Schwilk

## supplies score.predictions list accessed by mtn and variable. See
## reconstruct-climate.R for use.

library(dplyr)
library(MuMIn)

TEMPO_RES_DIR <- "../results/tempo_mod_results/"

source("wx-data.R") # provides hist_wx_data data frame


## If running this on its own, you need to source the file below first, but in
## the main workflow, this is sourced from reconstruct-climate.R

source("./microclimate-topo-PCA.R") # provides PCAs object

PC_AXES <- c("PC1", "PC2", "PC3") #fit all three for now?

score.predictions <- list()
for (mtn in c("CM", "DM", "GM")) {
  score.predictions[[mtn]] <- list()
  for (v in c("tmin", "tmax")) {
      hist_data <- filter(hist_wx_data, MTN==mtn)
      thedf <- PCAs[[mtn]][[v]]$scores %>% inner_join(hist_data)
      sink(file = file.path(TEMPO_RES_DIR, paste(mtn, "_", v, ".txt", sep="")),
           append = FALSE, split = TRUE)

      predictions <- list()
      for (a in PC_AXES) {
        print(paste(mtn, v, a))
        modstr <- paste(a,  "~ TMIN + TMAX + PRCP + TMIN:PRCP + TMAX:PRCP")
        fullmod <- lm(modstr,  data = thedf, na.action = "na.fail")
        allmods <- dredge(fullmod) # MuMIn function
        print(allmods)
        bmod <- get.models(allmods, subset = 1)[[1]]
        print(summary(bmod))
        print(anova(bmod))
        predictions[[a]] <- predict(bmod, hist_data)
      }
      score.predictions[[mtn]][[v]] <- data.frame(do.call(cbind, predictions))
      score.predictions[[mtn]][[v]]$datet <- hist_data$datet
  }
  sink(NULL)
}

saveRDS(score.predictions, file.path(TEMPO_RES_DIR, "score_predictions.RDS"))
