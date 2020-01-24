# bat id by user
#####
library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(ggmap)
library(RColorBrewer)

##### Read in GC list
gc<-read.csv("cave-mines-not-complete.csv",header=T,fill = T,sep = ",",na.strings = "",quote = "",comment.char = "")

# somehow, I missed these sites in the previous list
m_gc<-read.csv("missing-cave-mines-not-complete.csv",header=T,fill = T,sep = ",",na.strings = "",quote = "",comment.char = "")

# create final list
f_gc<-rbind(gc,m_gc)

# read in results (finally in the right format)
all_results<-read.table("data/cave-mines-not-complete-results.tab",header=T,fill = T,sep = "\t",na.strings = "",quote = "",comment.char = "")

# search logs for bats, search urls for cave/mine
bat_mentions<-unique(all_results[grep("\\bbat[s]\\b",all_results$log),]$i)
write.table(bat_mentions,file = "data/bat_mentions_maritimes.tab",quote = F,row.names = F,col.names = T,sep = "\t")

# only take sites with bat[s]
cache_with_bats<-subset(all_results,i %in% bat_mentions)

# fix coords
cache_merge <- merge(cache_with_bats,f_gc,by.x = "i",by.y = "url")
cache_merge$lat <- as.numeric(gsub("° ",".",gsub("[.]","",gsub(pattern = "N ",replacement = "",cache_merge$lat))))
cache_merge$lon <- as.numeric(gsub("° ",".",gsub("[.]","",gsub(pattern = "W ",replacement = "-",cache_merge$lon))))

# fix date
cache_merge$date<-mdy(cache_merge$date)

# summarise visits at these caves for the time period that we care about
unique_visits_window<-cache_merge %>% filter(status == c("Publish Listing","Found it","Didn't find it"), date >= 2008) 
unique_summary <- unique_visits_window %>% group_by(i,lat,lon) %>% summarise(total=length(users))

# map for positive bat id's
ggmap(get_map(location = "Maryland",source = "google",zoom = 5,maptype = "terrain-background",scale = "auto"))+
  geom_point(data=unique_summary,aes(y=lat,x=lon,size=total,group = i))+
  # scale_colour_manual(values = brewer.pal(11,name = "Paired")) +
  geom_path(data=unique_visits_window,aes(y=lat,x=lon,group=users),color="red")+
  transition_states(year,transition_length = 2,state_length = 1)+
  enter_appear()+exit_disappear()+
  ggtitle('Year: {closest_state}')

anim_save("figures/all_summary_year.gif",animation = last_plot())
