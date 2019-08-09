library(dplyr)
library(esri2sf)
library(sp)
library(lubridate)


# url for Bat Hibernation Period (Fall-Winter-Spring)
url <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/2"
df <- esri2sf(url)

# sample dates
dates<-df[,c("YR_SUSPECT","YR_CONFIRM","SAMPLEDATE")]

##### Get coordinates from polygons
centroids<-st_centroid(df$geoms)

# correct centroids
corr.centroids <- st_transform(centroids, 29101) %>% 
  st_centroid() %>% 
  # this is the crs from d, which has no EPSG code:
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  # since you want the centroids in a second geometry col:
  st_geometry()

# convert to dataframe
df.centroids<-data.frame(matrix(unlist(corr.centroids), nrow=length(corr.centroids), byrow=T))

# combine
clean.df <- cbind(df,df.centroids)