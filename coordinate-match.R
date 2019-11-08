library(vcfR)
library(adegenet)
library(geosphere)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(lubridate)

######
# read in geocache list
m_gc<-read.csv("cave-mines-not-complete.csv",header=T)

# read in results (finally in the right format)
all_results<-rbind(read.table("cave-mines-not-complete-results.tab",header=T,fill = T,sep = "\t",na.strings = "",quote = "",comment.char = ""),
                   read.table("missing-cave-mines-not-complete-results.tab",header=T,fill = T,sep = "\t",na.strings = "",quote = "",comment.char = ""))

# merge with coords
all_results_merge <- merge(all_results,m_gc,by.x = "i",by.y = "url",all=T)

# fix coords
# on linux, encoding changes to "\xb0"
lat.dms <- do.call(rbind, strsplit(as.character(all_results_merge$lat), ":"))
lat.dec <- as.numeric(lat.dms[,1]) + (as.numeric(lat.dms[,2]) + as.numeric(lat.dms[,3])/60)/60
all_results_merge$lat <- lat.dec
lon.dms <- do.call(rbind, strsplit(as.character(all_results_merge$lon), ":"))
lon.dec <- as.numeric(lon.dms[,1]) + (as.numeric(lon.dms[,2]) + as.numeric(lon.dms[,3])/60)/60
all_results_merge$lon <- lon.dec

# date
all_results_merge$year <- year(parse_date_time(all_results_merge$date,orders = "m/d/y"))

# trim down list of locations
geocache.locs<-na.omit(all_results_merge) %>%
                group_by(GC,lon,lat) %>% 
                summarise(count=n())

# coords as an sp object
geocache.coords<-st_as_sf(geocache.locs,coords = c("lon", "lat"),crs = 4326, agr = "constant")

##### Genetic samples #####
## MSAT
# coords<-read.csv("../Pd_MSAT/msat_locations.csv",header=T)
# genetic.coords<-coords[!is.na(coords$lat),]
# genetic.coords<-genetic.coords[!duplicated(genetic.coords$lat),]
# genetic.coords<-st_as_sf(genetic.coords,coords = c("lon", "lat"),crs = 4326, agr = "constant")

## SNPs
meta <- read.csv("../Pd_MSAT/SraRunTable.csv", header = T)
vcf <- read.vcfR("../Pd_MSAT/NA.bestsnp.backfill.filtered.vcf")
genind <- vcfR2genind(vcf)
strata<-meta[match(indNames(genind),meta$Run),]
include_list <- as.character(strata[!is.na(strata$lat), ]$Run)
genind <- genind[include_list]
strata <- subset(strata, Run %in% include_list)
strata <- strata[match(indNames(genind), strata$Run),]
genetic.coords <- strata[!duplicated(strata$other_location), c("lon", "lat")]
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
  distm(closest.matches[i,c("gen.lon","gen.lat")],closest.matches[i,c("gc.lon","gc.lat")],fun = distGeo))

# per year
max.visits<-na.omit(all_results_merge) %>% 
  group_by(GC,year) %>% 
  mutate(total=n()) %>% 
  group_by(GC,lat,lon,) %>% 
  summarise(max=max(total))

m1<-merge(closest.matches,strata,by.x="gen.lat",by.y="lat")
m2<-merge(m1,max.visits,by.x="gc.lat",by.y="lat")

# only use the closest matches
min.closest.match <- m2 %>% 
  group_by(gen.lon,gen.lat) %>%
  filter(dist==min(dist,na.rm = T))

write.csv(min.closest.match,"closest.matches.snp.csv")

#####
# cleaned.df<-all_results_merge %>% 
#   filter(status %in% c("Publish Listing","Found it","Didn't find it"))
# 
# s<-split(as.character(cleaned.df$users), as.character(cleaned.df$GC))
# results<-NULL
# for (i in unique(names(s))) {
#   j<-s[i]
#   c<-sapply(s,`%in%`,j)  
#   d<-sapply(c,sum)
#   results<-rbind(results,cbind(d,names(d),i))
# }
# results<-as.data.frame(results)
# rownames(results)<-NULL
# colnames(results)<-c("intersect","gc1","gc2")
# results$intersect<-as.numeric(results$intersect)
# 
# closest.matches.merge[results$gc1]
# 
# results.mat<-spread(results,key = "gc1",value = "intersect")
# rownames(results.mat)<-results.mat$gc2
# results.mat<-results.mat[,-1]
# 
# write.table(results.mat,file="gc.user.intersect.tab",quote = F,sep = "\t",col.names = T,row.names = T)
