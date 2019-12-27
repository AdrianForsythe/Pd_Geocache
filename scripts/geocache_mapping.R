######
# mapping cave visits
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

# merge with coords
all_results_merge <- merge(all_results,f_gc,by.x = "i",by.y = "url",all=T)

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
  theme_classic()+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size = 12),
        legend.position = "none")
ggsave(filename = "figures/num-geocache.png",plot=last_plot())

##### correlation in visits/time?
num.geocache <- all_visits_window %>% group_by(date) %>% summarise(total=length(unique(i)))

ggplot(num.geocache,aes(x=date,y=total))+
  geom_point(alpha=0.5)+
  geom_smooth(method = "lm",se=F,color="red",size=2)+
  geom_vline(xintercept = as.numeric(as.Date("2008-01-01")),linetype="dashed",color="blue",size=2)+
  labs(x= "Date",y="Number of Geocache Sites at Caves/Mines")+
  geom_text(aes(x=as.Date("2008-01-01"),y=35,label="WNS Introduced"),hjust=-0.1,size=4)+
  theme_classic()+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size = 12))
ggsave(filename = "figures/num-geocache-year.png",plot=last_plot())

# user activity
user.activity.date <- all_visits_window %>% group_by(date,GC) %>% summarise(total=n())
# %>% group_by(date,GC) %>% summarise(mean=mean(total),sd=sd(total))

ggplot(user.activity.date,aes(x=date))+
  geom_histogram(color="black",fill="gray")+
  # geom_point(alpha=0.5)+
  # geom_line(data = user.activity.month.year,aes(x=month.year,y=mean,group=1))
  # geom_smooth(method = "lm",na.rm = T,color="red")+
  # coord_cartesian(ylim = c(0,40))+
  labs(x="Date",y="Number of Unique Visitors")+
  geom_vline(xintercept = as.numeric(as.Date("2008-01-01")),linetype="dashed",color="blue",size=2)+
  geom_text(aes(x=as.Date("2008-01-01"),y=2100,label="WNS Introduced"),hjust=-0.1,size=4)+
  theme_classic()+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size = 12))
ggsave(filename = "figures/max-visits-date.png",plot=last_plot())

library(lme4)
summary(lmer(total ~ date + (1|GC),data = user.activity.date))

#### ANIMATE
# now a moving picture!
p<-ggmap(map)+
  geom_point(data=all_visits_window,aes(y=lat,x=lon,group=users))+
  # geom_path(data=all_visits_window,aes(y=lat,x=lon,group=users))+
  transition_reveal(along = date,keep_last = F)+
  shadow_wake(wake_length = 0.1, alpha = T)+
  # scale_colour_manual(values = sample(col_vector, n,replace = T))+
  #   ease_aes("linear")+
  ggtitle('Year: {frame_along}')+
  theme_classic()+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size = 12),
        legend.position = "none")
anim_save("figures/users_year.gif",animation = p)

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