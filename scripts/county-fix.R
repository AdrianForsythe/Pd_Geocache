library(rgdal)
library(maps)
library(sp)
library(maptools)

source("scripts/coordinate-overlap.R")

# make a SpatialPolygonsDataframe, make row ids to match
rownames(df) <- names(poly)
poly.df<-SpatialPolygonsDataFrame(poly,df)

# read in list of all counties
counties<-read.csv("data/all-counties.csv",header=T)
counties<-tidyr::separate(counties,1,c("county","state.province","Country"),sep="\t")
counties$county<-gsub(pattern = " / (.*)","",counties$county)

# Canada counties
# read in shape file
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
for (i in 1:1376) {
  x<-slot(usa@polygons[[i]],"ID")
  usa.id<-rbind(usa.id,x)
}

usa.sf <- st_as_sf(usa)
usa.sf$id <- usa.id

united.poly<-rbind(can.sf[,"id"],usa.sf[,2:1])

####
# combine spatial polygon dfs
# united.poly<-sf::st_union(can@polygons,usa)
# united.poly<-unionSpatialPolygons(can,usa)
# usa.id<-paste(usa.counties,usa.states,sep=", ")
# 
# can.id<-paste(can$CDNAME,can$PRNAME,sep=", ")
# 
# all.counties<-as.data.frame(c(usa.id,can.id))
# colnames(all.counties) <- "id"
# 
# geocache.presence.df$county<-tolower(geocache.presence.df$county)
# geocache.presence.df$state.prov<-tolower(geocache.presence.df$state.prov)
# geocache.presence.df$id<-paste(geocache.presence.df$county,geocache.presence.df$state.prov,sep = ", ")
# 
# not.present<-geocache.presence.df[-which(geocache.presence.df$id %in% all.counties$id),]
# not.present<-filter(not.present,year %in%  2006:2016)
# 
# all.not.present<-expand.grid(not.present$year,not.present$id)
# colnames(all.not.present) <- c("year","id")

# all.counties.merge<-merge(all.not.present,geocache.presence.df,by="id")
