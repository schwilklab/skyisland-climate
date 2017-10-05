

source("./EML-tools.R")


# move data files to export directory
# sensors
file.copy("../microclimate/sensors.csv", "./export/sensors/sensors.csv")


# daily summaries
dts <- readRDS("../results/tempdata/daily-sum.rds")
write.csv(dts, "./export/sensors/temperature-daily-summaries.csv", row.names=FALSE)


sensors.eml <- make_data_set("./details.JSON")
eml_validate(sensors.eml)
# Write out our EML object to an XML file:
write_eml(sensors.eml, file=file.path(OUT_DIR, "sensors-eml.xml"))


#devtools::install_github("ropenscilabs/emldown")
library("emldown")
render_eml(file.path(OUT_DIR, "sensors-eml.xml"))

#eml_validate(file.path(OUT_DIR, "EML.xml"))


