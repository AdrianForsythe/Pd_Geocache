#### script for creating a spatial polygon dataframe 
#### for all counties/municipalities in US/Canada
#### where we have WNS records from

library(rgdal)
library(maps)
library(sp)
library(maptools)

# first run the output from script to match shape files to GC sites
source("scripts/coordinate-overlap.R")

# make a SpatialPolygonsDataframe, make row ids to match
rownames(df) <- names(poly)
poly.df<-SpatialPolygonsDataFrame(poly,df)

# read in list of all counties where WNS has been found
counties<-read.csv("data/all-counties.csv",header=T)
counties<-tidyr::separate(counties,1,c("county","state.province","Country"),sep="\t")
counties$county<-gsub(pattern = " / (.*)","",counties$county)

#### SHAPE FILES FOR US AND CANADA

# Canadian counties/municipalities
# read in shape file from census data
can<-readOGR("shape/lcd_000b16a_e/lcd_000b16a_e.shp")

# select only the provinces that we have data from
can<-can[can@data$PRNAME %in% c("Quebec / Québec","Ontario","Prince Edward Island / Île-du-Prince-Édouard","Nova Scotia / Nouvelle-Écosse","New Brunswick / Nouveau-Brunswick"),]

# create unique ID's
can.id<-as.character(paste(can$CDNAME,can$PRNAME,sep=", "))
can.id<-gsub(pattern = " / (.*)","",can.id)
can.id<-gsub(pattern = "Quebec","Québec",can.id)

# coerce to sf object
can.sf<-st_as_sf(can)

# add ids to sf object
can.sf$id <- can.id

# spatial transformation to proper projection 
can.sf<-st_transform(can.sf,"+proj=longlat +datum=WGS84")

# USA counties
usa <- map("county",regions = counties[counties$Country == "USA",]$state.province, fill = T)
usa.id <- paste(as.character(sapply(strsplit(usa$names, ","), function(x) x[2])),
                as.character(sapply(strsplit(usa$names, ","), function(x) x[1])),sep = ", ")
usa <- map2SpatialPolygons(usa, IDs=usa.id, proj4string=CRS("+proj=longlat +datum=WGS84"))

# loop through sp object and create a vector of county names that match ID's
usa.id<-NULL
for (i in 1:length(usa)) {
  x<-slot(usa@polygons[[i]],"ID")
  usa.id<-rbind(usa.id,x)
}

# coerce to sf object
usa.sf <- st_as_sf(usa)
usa.sf$id <- usa.id

##### Combine spatial polygon dfs from US and Canada
united.poly<-rbind(can.sf[,"id"],usa.sf[,2:1])