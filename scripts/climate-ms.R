# analysis for micor climate manuscript

library(ggplot2)

source("./climate-tools.R")

DM <- build_climate_df("DM")

# check ref vs hist
DM.ref.hist <- filter(DM, period =="ref" & scenario!="rcp85") %>%
  select(x,y,march_may_min, gcm, period) %>% 
  spread(gcm, march_may_min)

ggplot(DM.ref.hist, aes(hist, CCSM4.r6i1p1)) + geom_point(alpha=0.5)
# super tight


ggplot(filter(DM, scenario!="hist"), aes(march_may_min, gswc, color=period)) +
  geom_point(alpha=0.3) + facet_grid(. ~ scenario)
ggsave("../results/plots/DM-freeze_gswc_hist_projected.jpg")

ggplot(filter(DM, scenario!="hist"), aes(x=BIO9, color=period)) +
  geom_density() + facet_grid(. ~ scenario)

ggplot(filter(DM, scenario!="hist"), aes(x=period, y=BIO9)) +
  geom_violin() + facet_grid(. ~ scenario)


# historical reconstruction raster bricks
CM <- retrieve_reconstruction_df("CM")
DM <- retrieve_reconstruction_df("DM")
GM <- retrieve_reconstruction_df("GM")

library(ggplot2)
GM.hist.df <- as.data.frame(GM)
GM.CCSM4.rcp85 <- as.data.frame(retrieve_reconstruction("GM", "CCSM4.r6i1p1", "rcp45", "2080s"))

GM.hist.df$period <- "hist"
GM.CCSM4.rcp85$period <- "2080s"
GM.all <- rbind(GM.hist.df, GM.CCSM4.rcp85)
ggplot(GM.all, aes(march_may_min, color=period)) + geom_density()

plot(CM$march_may_min)
