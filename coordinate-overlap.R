library(geosphere)
library(data.table)
library(dplyr)

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

# locations of genetic samples
msat_locations<-read.csv("../Pd_MSAT/Pd_clean_data.csv",header=T,na.strings = "")
snp_locations<-read.csv("../Pd_MSAT/SraRunTable.csv",header=T)

genetic_merge<-rbind(msat_locations[,c("lat","lon","region")],snp_locations[,c("lat","lon","region")])
genetic_merge<-genetic_merge[!duplicated(c(genetic_merge$lat,genetic_merge$lon)),]

### find overlaps
genetic_coords<-data.table(genetic_merge[, c("lon","lat")])
geocache_coords<-data.table(all_results_merge[, c("lon","lat")])

CartesianJoin<- function(X,Y)
  setkey(X[,c(k=1,.SD)],k)[Y[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]

LatLonWide <- CartesianJoin(genetic_coords,geocache_coords)

LatLonWide$dist <- sapply(1:nrow(LatLonWide),function(i)
  distm(c(LatLonWide$lon[i],LatLonWide$lat[i]),c(LatLonWide$i.lon[i],LatLonWide$i.lat[i])))

colnames(LatLonWide) <- c("genetic.lon","genetic.lat","geocache.lon","geocache.lat","dist")