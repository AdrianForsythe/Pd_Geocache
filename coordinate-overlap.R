library(geosphere)
library(sf)
library(dplyr)
library(lubidate)

######
# read in geocache list
m_gc<-read.csv("cave-mines-not-complete.csv",header=T)

# read in results (finally in the right format)
all_results<-read.table("cave-mines-not-complete-results.tab",header=T,fill = T,sep = "\t",na.strings = "",quote = "",comment.char = "")

# merge with coords
all_results_merge <- merge(all_results,m_gc,by.x = "i",by.y = "url",all=T)

# fix coords
# on linux, encoding changes to "\xb0"
all_results_merge$lat <- as.numeric(gsub("\xb0 ",".",gsub("[.]","",gsub(pattern = "N ",replacement = "",all_results_merge$lat))))
all_results_merge$lon <- as.numeric(gsub("\xb0 ",".",gsub("[.]","",gsub(pattern = "W ",replacement = "-",all_results_merge$lon))))

# date
all_results_merge$year <- year(mdy(all_results_merge$date))

# trim down
geocache.locs<-all_results_merge %>% group_by(i,lon,lat) %>% summarise(total=n())

# coords as an sp object
geocache.coords<-as_Spatial(st_as_sf(geocache.coords,coords = c("lon", "lat"),crs = 4326, agr = "constant"))

# WNS presence data
source("wns-presence.R")

##### find overlaps in coords to match caves with counties #####
# returns a numeric vector of length equal to the number of points
# the number is the index (number) of the polygon of y in which a point falls
# NA denotes the point does not fall in a polygon
# if a point falls in multiple polygons the last polygon is recorded.

index.df<-as.data.frame(over(geocache.coords, poly,returnList = F, fn = mean))
colnames(index.df) <-"index"

# do a loop for "complex" lookup operation
results<-NULL
for (i in index.df$index) {
  w<-df[i,"SAMPLEDATE"]
  x<-df[i,"YR_SUSPECT"]
  y<-df[i,"YR_CONFIRM"]  
  z<-df[i,"WNS_MAP_YR"]  
  results<-rbind.data.frame(results,cbind(w,x,y,z))
}
colnames(results)<-c("sample.date","yr.suspect","yr.confirm","wns.map.yr")

geocache.presence.df<-cbind.data.frame(geocache.coords,results)

# merge it back together with the original dataset
all_merge<-merge(geocache.presence.df,all_results,by="i",all=T)

# fix dates
all_merge$date <- mdy(all_merge$date)
all_merge$sample.date<-gsub(pattern = " 06/09/2018",replacement = "06/09/2018",all_merge$sample.date)

#### temporary assumptions ####
# presence dates
all_merge$sample.date<-mdy(gsub(pattern = "/00/",replacement = "/01/",all_merge$sample.date))
all_merge$wns.map.yr<-ymd(gsub("-.+","/01/01",all_merge$wns.map.yr))
all_merge$yr.suspect<-ymd(gsub("-.+","/01/01",all_merge$yr.suspect))
all_merge$yr.confirm<-ymd(gsub("-.+","/01/01",all_merge$yr.confirm))

# records before WNS sampling
# relevant.records<-all_merge[all_merge$sample.date > all_merge$date | all_merge$sample.date > all_merge$yr.suspect | all_merge$sample.date > all_merge$yr.confirm,]
relevant.records<-all_merge[all_merge$wns.map.yr > all_merge$date,]
relevant.records<-relevant.records[!is.na(relevant.records$i),]

#### next run a model?