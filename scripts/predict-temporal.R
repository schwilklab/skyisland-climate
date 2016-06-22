

library(dplyr)
#library(MuMIn)

TEMPO_RES_DIR <- "../results/tempo_mod_results/"

source("wx-data.R") # provides hist_wx_data data frame

source("./microclimate-topo-PCA.R")  # must be run at some point


fitTemporalModelRunDiagnostics <- function(mtn, thevar) {
  # split and redirect output
  hist_data <- filter(hist_wx_data, MTN==mtn)
  df <- PCAs[[mtn]][[thevar]]$scores %>% inner_join(hist_data)
  sink(file = file.path(TEMPO_RES_DIR, paste(mtn, "_", thevar, ".txt", sep="")),
       append = FALSE, split = TRUE)

  # PC1
  mod1 <- lm(PC1 ~ TMIN + TMAX + PRCP + TMIN:PRCP + TMAX:PRCP,  data = df, na.action = "na.fail")
  print(summary(mod1))
  print(anova(mod1))
  PC1.predicted <- predict(mod1, hist_data)

  # PC2
  mod2 <- lm(PC1 ~ TMIN + TMAX + PRCP + TMIN:PRCP + TMAX:PRCP,  data = df)
  print(summary(mod2))
  print(anova(mod2))
  PC2.predicted <- predict(mod2, hist_data)
  sink(NULL)
  return(data.frame(datet=hist_data$datet, PC1=PC1.predicted, PC2=PC2.predicted))
}


score.predictions <- list()
for (mtn in c("CM", "DM", "GM")) {
  score.predictions[[mtn]] <- list()
  for (v in c("tmin", "tmax")) {
      score.predictions[[mtn]][[v]] <- fitTemporalModelRunDiagnostics(mtn, v)
  }
}

