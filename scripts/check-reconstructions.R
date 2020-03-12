
# read in tools
source("./climate-tools.R")



library(ggplot2)

DM.hist <- retrieve_reconstruction("DM") %>% mutate(tp = "1961-2000")
DM.Had2080s <- retrieve_reconstruction("DM", "HadGEM2-CC.r1i1p1", "rcp85", "2080s") %>% mutate(tp="2071-2100")

DM.bioclim <- rbind(DM.hist, DM.Had2080s) #%>% mutate(tp = factor(tp, levels=c("1961-2000", "2071-2100")))
                                                                 



ggplot(DM.bioclim, aes(x,y, fill=march_may_min)) + geom_raster() + facet_grid(. ~ tp) +
  xlab("Longitude") +ylab("Latitude") +
  guides(fill=guide_legend(title="Spring minimum (C)")) +
  theme(legend.justification = c(0, 1), legend.position = c(0, 1))
ggsave("../results/plots/DM_hist_2080s_mmmin.png", width=10, height=5)


DM.df <- topostacks[["DM"]]
DM.df <- as.data.frame(DM.df, xy=TRUE)

DM.df <- left_join(DM.hist, DM.df)

ggplot(DM.df, aes(elev, gswc)) + geom_point()
