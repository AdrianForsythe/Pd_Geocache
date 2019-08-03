###############
# mapping cave visits
library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(ggmap)
library(RColorBrewer)

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

# fix date
all_results_merge$date<-mdy(all_results_merge$date)
all_results_merge$year <- year(all_results_merge$date)
all_results_merge$month <- month(all_results_merge$date)
all_results_merge$month.year <- ymd(all_results_merge$date)
  
# summarize all data
all_visits_window<-all_results_merge %>% filter(status %in% c("Publish Listing","Found it","Didn't find it")) 
all_summary <- all_visits_window %>% group_by(i,lat,lon,year,month) %>% summarise(total=length(users))

# map for all caves
register_google(key = "AIzaSyDw5appsfJ_gWd45-AeYe_WTT2VvI8kXhQ")
map <- get_map(c(right=-57,left=-96,top=52,bottom=32),zoom = 5,source = "stamen",maptype = "terrain-background",scale = "auto")

# set color pallete for year
n <- length(unique(all_summary$year))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

# static image
ggmap(map)+
  geom_path(data=all_visits_window,aes(y=lat,x=lon,group=users),color="red",alpha=0.3)+
  geom_point(data=all_summary,aes(y=lat,x=lon,size=total,color=as.factor(year)),alpha=0.5)+
  # scale_colour_manual(values = brewer.pal(11,name = "Paired")) +
  theme(legend.position = "none")

# correlation in visits/time?
num.geocache <- all_visits_window %>% group_by(date) %>% summarise(total=length(unique(i)))

ggplot(num.geocache,aes(x=date,y=total))+
  geom_point(alpha=0.5)+
  geom_smooth(method = "lm",se=F,color="red")+
  geom_vline(xintercept = as.numeric(as.Date("2008-01-01")),linetype="dashed",color="blue")+
  labs(x= "Date",y="Number of Geocache Sites at Caves/Mines")+
  geom_text(aes(x=as.Date("2008-01-01"),y=35,label="WNS Introduced"),hjust=-0.1)+
  theme_classic()
ggsave(filename = "num-geocache-year.png",plot=last_plot())

# user activity
user.activity.date <- all_visits_window %>% group_by(date,GC) %>% summarise(total=n())
# %>% group_by(date,GC) %>% summarise(mean=mean(total),sd=sd(total))

user.activity.month <- all_visits_window %>% group_by(month,GC) %>% summarise(total=n()) %>%
  group_by(month,GC) %>% summarise(mean=mean(total),sd=sd(total))

user.activity.year <- all_visits_window %>% group_by(year,GC) %>% summarise(total=n()) %>%
  group_by(year,GC) %>% summarise(mean=mean(total),sd=sd(total))

user.activity.month.year <- all_visits_window %>% group_by(month.year) %>% summarise(total=n()) %>%
  group_by(month.year) %>% summarise(avg=mean(total),sd=sd(total))

ggplot(user.activity.date,aes(x=date,y=total))+
  stat_summary(fun.y = max,geom = "point",alpha=0.5)+
  # geom_line(data = user.activity.month.year,aes(x=month.year,y=mean,group=1))
  geom_smooth(method = "lm",na.rm = T,color="red")+
  coord_cartesian(ylim = c(0,40))+
  labs(x="Date",y="Average Number of Unique Visitors")+
  geom_vline(xintercept = as.numeric(as.Date("2008-01-01")),linetype="dashed",color="blue")+
  geom_text(aes(x=as.Date("2008-01-01"),y=25,label="WNS Introduced"),hjust=-0.1)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave(filename = "max-visits-date.png",plot=last_plot())

#### ANIMATE
# now a moving picture!
ggmap(map)+
  geom_point(data=all_visits_window,aes(y=lat,x=lon,group=users),alpha=0.5)+
  # geom_path(data=all_visits_window,aes(y=lat,x=lon,group=users))+
  transition_reveal(along = date,keep_last = F)+
  shadow_wake(wake_length = 0.1, alpha = T)+
  # scale_colour_manual(values = sample(col_vector, n,replace = T))+
  #   ease_aes("linear")+
  # ggtitle('Year: {frame_along}')
  theme(legend.position = "none")
  
anim_save("users_year.gif",animation = last_plot())

## Distance travelled by users
library(geosphere)

travellers<-all_visits_window %>% group_by(users) %>% mutate(finds = n()) %>% 
  filter(finds >1 & status %in% c("Found it","Didn't find it"))

for (i in unique(travellers$users)) {
  d<-travellers[travellers$users=="NY Kid",]
  f<-d[!duplicated(d$i),]
  m<-distm(f[,c("lat","lon")],fun = distHaversine)  
  diag(m) <- NA
  colnames(m) <- f$GC
  cbind.data.frame((m[upper.tri(m,diag = T)]),f)
  
}


###############
# search logs for bats, search urls for cave/mine
bat_mentions<-unique(all_results[grep("\\bbat[s]\\b",all_results$log),]$i)
write.table(bat_mentions,file = "bat_mentions_maritimes.tab",quote = F,row.names = F,col.names = T,sep = "\t")

# only take sites with bat[s]
cache_with_bats<-subset(all_results,i %in% bat_mentions)

# fix coords
cache_merge <- merge(cache_with_bats,m_gc,by.x = "i",by.y = "url")
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

anim_save("all_summary_year.gif",animation = last_plot())
