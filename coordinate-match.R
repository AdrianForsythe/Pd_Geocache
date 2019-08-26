library(geosphere)
library(sf)
library(dplyr)
library(tidyr)

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
geocache.coords<-st_as_sf(geocache.locs,coords = c("lon", "lat"),crs = 4326, agr = "constant")

##### Genetic samples #####
genetic.coords<-read.csv("../Pd_MSAT/SraRunTable.csv",header=T)
genetic.coords<-genetic.coords[!is.na(genetic.coords$lat),]
genetic.coords<-st_as_sf(genetic.coords,coords = c("lon", "lat"),crs = 4326, agr = "constant")

##### find closest point to match isolate locations with geocaches
closest.matches<-as.data.frame(st_sf(st_nearest_points(geocache.coords$geometry,genetic.coords$geometry)))
closest.matches<-separate(closest.matches,1,c("gc.lon","gen.lon","gc.lat","gen.lat"),sep=c(","," "))
closest.matches$gen.lon<-as.numeric(gsub("[c(,)]", "",closest.matches$gen.lon))
closest.matches$gc.lon<-as.numeric(gsub("[c(,)]", "",closest.matches$gc.lon))
closest.matches$gen.lat<-as.numeric(gsub("[c(,)]", "",closest.matches$gen.lat))
closest.matches$gc.lat<-as.numeric(gsub("[c(,)]", "",closest.matches$gc.lat))

# distance in meters!
closest.matches$dist<-sapply(1:nrow(closest.matches),function(i)
  distm(closest.matches[i,c("gen.lon","gen.lat")],closest.matches[i,c("gc.lon","gc.lat")],fun = distHaversine))

min.closest.match <- closest.matches %>% group_by(gc.lon,gc.lat) %>% summarise(min.dist =min(dist))
