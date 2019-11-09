library(rgdal)
library(maps)
library(sp)
library(maptools)

# first run the output from script to match shape files to GC sites
source("scripts/coordinate-overlap.R")

# make a SpatialPolygonsDataframe, make row ids to match
rownames(df) <- names(poly)
poly.df<-SpatialPolygonsDataFrame(poly,df)

# read in list of all counties
counties<-read.csv("data/all-counties.csv",header=T)
counties<-tidyr::separate(counties,1,c("county","state.province","Country"),sep="\t")
counties$county<-gsub(pattern = " / (.*)","",counties$county)

# Canadian counties/municipalities
# read in shape file from census data
can<-readOGR("shape/lcd_000b16a_e/lcd_000b16a_e.shp")
can<-can[can@data$PRNAME %in% c("Quebec / Québec","Ontario","Prince Edward Island / Île-du-Prince-Édouard","Nova Scotia / Nouvelle-Écosse","New Brunswick / Nouveau-Brunswick"),]
can.id<-as.character(paste(can$CDNAME,can$PRNAME,sep=", "))
can.id<-gsub(pattern = " / (.*)","",can.id)
can.id<-gsub(pattern = "Quebec","Québec",can.id)

can.sf<-st_as_sf(can)
can.sf$id <- can.id
can.sf<-st_transform(can.sf,"+proj=longlat +datum=WGS84")

# USA counties
usa <- map("county",regions = counties[counties$Country == "USA",]$state.province, fill = T)
usa.id <- paste(as.character(sapply(strsplit(usa$names, ","), function(x) x[2])),
                as.character(sapply(strsplit(usa$names, ","), function(x) x[1])),sep = ", ")
usa <- map2SpatialPolygons(usa, IDs=usa.id, proj4string=CRS("+proj=longlat +datum=WGS84"))

usa.id<-NULL
for (i in 1:length(usa)) {
  x<-slot(usa@polygons[[i]],"ID")
  usa.id<-rbind(usa.id,x)
}

usa.sf <- st_as_sf(usa)
usa.sf$id <- usa.id

# combine spatial polygon dfs
united.poly<-rbind(can.sf[,"id"],usa.sf[,2:1])