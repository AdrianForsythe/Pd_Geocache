
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

# interp of total visits
pts.grid <- interp(geocache.locs$lon,geocache.locs$lat,geocache.locs$total,duplicate = "mean")
pts.grid2 <- expand.grid(x=pts.grid$x, y=pts.grid$y)
pts.grid2$z <- as.vector(pts.grid$z)

register_google(key = "AIzaSyDw5appsfJ_gWd45-AeYe_WTT2VvI8kXhQ")
map<-get_map(location = "Maryland",source = "google",zoom = 5,maptype = "terrain-background",scale = "auto")
ggmap(map)+
  geom_tile(data=na.omit(pts.grid2),aes(x=x,y=y,fill=z),alpha=0.75)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
