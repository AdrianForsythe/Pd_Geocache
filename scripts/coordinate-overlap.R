library(geosphere)
library(sf)
library(sp)
library(dplyr)
library(lubridate)

##### Read in GC list
gc<-read.csv("data/cave-mines-not-complete.csv",header=T,fill = T,sep = ",",na.strings = "",quote = "",comment.char = "",row.names = NULL)

# somehow, I missed these sites in the previous list
m_gc<-read.csv("data/missing-cave-mines-not-complete.csv",header=T,sep=",",fill = T,na.strings = "",quote = "",comment.char = "",row.names = NULL)

# create final list
f_gc<-rbind(gc,m_gc)

# read in results (finally in the right format)
all_results<-read.table("data/cave-mines-not-complete-results.tab",header=T,fill = T,sep = "\t",na.strings = "",quote = "",comment.char = "")
all_results <- filter(all_results,status == c("Found it","Didn't find it","Owner Maintenance","Publish Listing"))

# merge with coords
all_results_merge <- merge(all_results,f_gc,by.x = "i",by.y = "url",all=T)

# fix coords
lat.dms <- do.call(rbind, strsplit(as.character(all_results_merge$lat), ":"))
lat.dec <- as.numeric(lat.dms[,1]) + (as.numeric(lat.dms[,2]) + as.numeric(lat.dms[,3])/60)/60
all_results_merge$lat <- lat.dec
lon.dms <- do.call(rbind, strsplit(as.character(all_results_merge$lon), ":"))
lon.dec <- as.numeric(lon.dms[,1]) + (as.numeric(lon.dms[,2]) + as.numeric(lon.dms[,3])/60)/60
all_results_merge$lon <- lon.dec

# and convert to DD coords
char2dms(all_results_merge,chd = ":",chm = ":",chs = ":")

# date
all_results_merge$year <- year(mdy(all_results_merge$date))

# trim down to the number of common users between geocache sites over years
geocache.locs<-all_results_merge %>% group_by(year) %>% mutate(count=length(unique(i))) %>% 
  group_by(i,lat,lat,year,count) %>% 
  summarise(total=length(!unique(users)))
  
# coords as an sp object
geocache.coords<-as_Spatial(st_as_sf(geocache.locs,coords = c("lat", "lat"),crs = 4326, agr = "constant"))

# load in WNS presence data
source("scripts/wns-presence.R")

##### find overlaps in coords to match caves with counties #####
# returns a numeric vector of length equal to the number of points
# the number is the index (number) of the polygon of y in which a point falls
# NA denotes the point does not fall in a polygon
# if a point falls in multiple polygons the last polygon is recorded.

index.df<-as.data.frame(over(geocache.coords, poly,returnList = F))
colnames(index.df) <-"index"

# do a loop for "complex" lookup operation
results<-NULL
for (i in index.df$index) {
  u<-df[i,"STATEPROV"]
  v<-df[i,"COUNTYNAME"]
  w<-df[i,"SAMPLEDATE"]
  x<-df[i,"YR_SUSPECT"]
  y<-df[i,"YR_CONFIRM"]  
  z<-df[i,"WNS_MAP_YR"]  
  results<-rbind.data.frame(results,cbind(u,v,w,x,y,z))
}
colnames(results)<-c("state.prov","county","sample.date","yr.suspect","yr.confirm","wns.map.yr")

geocache.presence.df<-cbind.data.frame(geocache.coords,results)

# merge it back together with the original dataset
all_merge<-merge(geocache.presence.df,all_results,by="i",all=T)

# fix dates
all_merge$date <- mdy(all_merge$date)
# all_merge$sample.date<-gsub(pattern = " 06/09/2018",replacement = "06/09/2018",all_merge$sample.date)

#### temporary assumptions ####
# presence dates
# all_merge$sample.date<-mdy(gsub(pattern = "/00/",replacement = "/01/",all_merge$sample.date))
all_merge$wns.map.yr<-ymd(gsub("-.+","/01/01",all_merge$wns.map.yr))
all_merge$yr.suspect<-ymd(gsub("-.+","/01/01",all_merge$yr.suspect))
all_merge$yr.confirm<-ymd(gsub("-.+","/01/01",all_merge$yr.confirm))

# records before WNS sampling
# relevant.records<-all_merge[all_merge$sample.date > all_merge$date | all_merge$sample.date > all_merge$yr.suspect | all_merge$sample.date > all_merge$yr.confirm,]
relevant.records<-all_merge[all_merge$wns.map.yr > all_merge$date,]
relevant.records<-relevant.records[!is.na(relevant.records$i),]