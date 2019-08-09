library(geosphere)
library(data.table)
library(dplyr)

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
all_results_merge<-all_results_merge[!duplicated(all_results_merge$i),]

##### find overlaps in coords to match caves with counties
clean.df <- read.csv("clean-coords.csv",header = T,na.strings = " ")
presence_coords<-as.data.table(clean.df[,c("X1","X2")])
colnames(presence_coords) <- c("lon","lat")

geocache_coords<-data.table(all_results_merge[, c("lon","lat")])

###
CartesianJoin<- function(X,Y)
  setkey(X[,c(k=1,.SD)],k)[Y[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]

LatLonWide <- CartesianJoin(presence_coords,geocache_coords)

LatLonWide$dist <- sapply(1:nrow(LatLonWide),function(i)
  distm(c(LatLonWide$lon[i],LatLonWide$lat[i]),c(LatLonWide$i.lon[i],LatLonWide$i.lat[i])))

colnames(LatLonWide) <- c("presence.lon","presence.lat","geocache.lon","geocache.lat","dist")

# clean.df[clean.df$COUNTYNAME %in% all_results_merge$county,]

# promiscuity<-all_results_merge %>% group_by(i,year) %>% summarise(total=length(unique(users)))
# hist(promiscuity$total)
# 
# most_active <- filter(promiscuity,total >1)
