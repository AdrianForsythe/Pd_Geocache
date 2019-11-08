library(dplyr)
library(esri2sf)
library(sp)
library(lubridate)

# url for Bat Hibernation Period (Fall-Winter-Spring)
url2 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/2"
df2 <- as.data.frame(esri2sf(url2))

url5 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/5"
df5 <- as.data.frame(esri2sf(url5))

df <- rbind(df2,df5)
df$rownumber = 1:nrow(df)

poly<-as_Spatial(df$geoms)
