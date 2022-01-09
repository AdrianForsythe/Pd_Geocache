library(maps)
library(rworldmap)
# library(measurements)

dat<-read.csv("data/remaining-states_provinces-cor-gc-list.csv",header=T,row.names = NULL,fill = T,sep = ",",na.strings = "",quote = "",comment.char = "")

## a few things to clean
dat$county<-gsub(pattern = "\\(.*",replacement = "",dat$county)

# clean ws
dat<-as.data.frame(apply(dat,2,function(x)gsub('\\s+', '',x)))

# select the ones that need fixin'
need.fix<-dat[grep("°", dat$lat),]

# change the degree symbol to a space
need.fix$lat = gsub('°', ':', need.fix$lat)
need.fix$lon = gsub('°', ':', need.fix$lon)

# escape the period so that it actually gets removed
# need.fix$lat = gsub('\\.', ' ', need.fix$lat)
# need.fix$lon = gsub('\\.', ' ', need.fix$lon)

# get rid of double space
need.fix$lat = gsub('  ', ' ', need.fix$lat)
need.fix$lon = gsub('  ', ' ', need.fix$lon)

need.fix$lat = gsub('N ', '', need.fix$lat)
need.fix$lon = gsub('W ', '', need.fix$lon)

# make lon negative
need.fix$lon = paste("-",need.fix$lon,sep="")

# convert from decimal minutes to decimal degrees
# need.fix$cor.lat = as.numeric(measurements::conv_unit(need.fix$lat, from = 'deg_dec_min', to = 'dec_deg'))
# need.fix$cor.lon = as.numeric(measurements::conv_unit(need.fix$lon, from = 'deg_dec_min', to = 'dec_deg'))*-1

# from: https://gist.github.com/kgturner/72b6dc888ab585ee80de

lat<-do.call(rbind, strsplit(as.character(need.fix$lat), ":"))
lat.dec<-as.numeric(lat[,1])+
    as.numeric(lat[,2])/60
rm(lat)

#long for ddm
lon<-do.call(rbind, strsplit(as.character(need.fix$lon), ":"))
lon.dec<-as.numeric(lon[,1])+
    as.numeric(lon[,2])/60
rm(lon)
  
need.fix$lat <- lat.dec
need.fix$lon <- lon.dec

# coerce this to numeric
dat[grep("°", dat$lat,invert = T),"lat"] <- as.numeric(as.character(dat[grep("°", dat$lat,invert = T),"lat"]))
dat[grep("°", dat$lon,invert = T),"lon"] <- as.numeric(as.character(dat[grep("°", dat$lon,invert = T),"lon"]))

# replace the wrong coords with the corrected coords
dat[grep("°", dat$lat),"lat"] <- lat.dec
dat[grep("°", dat$lon),"lon"] <- lon.dec

write.csv(dat,"data/remaining-states_provinces-cor-gc-list.csv",row.names = F,quote = F)

# show it on a map
map("world")
plot(need.fix$cor.lat,need.fix$cor.lon)
