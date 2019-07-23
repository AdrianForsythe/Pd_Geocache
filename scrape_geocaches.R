#Loading the rvest package
library(RSelenium)
library(rvest)
library(lubridate)
library(dplyr)
library(ggplot2)
library(maps)

#start RSelenium
#connect to running server
rD <- rsDriver(port = 4445L, browser = 'firefox')
remDr <- rD$client # You dont need to use the open method 

# navigate to geocaching.com
remDr$navigate(url = "https://www.geocaching.com/account/signin?returnUrl=%2fplay")

# geocaching.com
#send username
username <- remDr$findElement(using = "id", value = "UsernameOrEmail")
username$sendKeysToElement(list("a_forsythe"))

#send password and Enter
passwd <- remDr$findElement(using = "id", value = "Password")
passwd$sendKeysToElement(list("qazwsxedc"))

SignOn <- remDr$findElement(using = "id", "SignIn")
SignOn$clickElement()

# generate list of urls
m_gc<-read.csv("cave-mines-not-complete.csv",header=T)

#navigate to your page
all_results<-NULL
for (i in m_gc$url) {
  
remDr$navigate(url = i)

#scroll down j times, waiting for the page to load at each time
for(j in 1:m_gc[m_gc$url==i,]$numpage){      
  remDr$executeScript(paste("scroll(0,",j*10000,");"))
  Sys.sleep(1.5)    
}

#get the page html
page_source<-remDr$getPageSource()

#parse it
users<-html(page_source[[1]]) %>% html_nodes(".h5") %>%
  html_text()

status<-html(page_source[[1]]) %>% html_nodes(".LogType .log-meta") %>%
  html_text(trim = T)

date<-html(page_source[[1]]) %>% html_nodes(".LogDate") %>%
  html_text()

log<-html(page_source[[1]]) %>% html_nodes(".LogText") %>%
  html_text(trim = T)

if (length(users)==0) {
  users<-""
}

if (length(status)==0) {
  status<-""
}

if (length(date)==0) {
  date<-""
}

if (length(log)==0) {
  log<-""
}


page_results <- cbind.data.frame(users,status,date,log,i)
all_results<-rbind(all_results,page_results)
}

# save results
write.table(all_results,file = "cave-mines-not-complete-results.tab",row.names = F,col.names = T,quote = F,sep = "\t")

###############
# search logs for bats, search urls for cave/mine
bat_mentions<-unique(all_results[grep("\\bbat[s]\\b",all_results$log),]$i)

# only take sites with bat[s]
cache_with_bats<-subset(all_results,i %in% bat_mentions)

# fix coords
cache_merge <- merge(cache_with_bats,m_gc,by.x = "i",by.y = "url")
cache_merge$lat <- as.numeric(gsub("° ",".",gsub("[.]","",gsub(pattern = "N ",replacement = "",cache_merge$lat))))
cache_merge$lon <- as.numeric(gsub("° ",".",gsub("[.]","",gsub(pattern = "W ",replacement = "-",cache_merge$lon))))

unique_visits_window<-cache_merge %>% filter(status == c("Publish Listing","Found it","Didn't find it"), year >= 2008) %>% group_by(i,lat,lon) %>% summarise(total=length(users))

# map
ggplot()+geom_polygon(data=map_data("North America"),aes(x=long,y=lat,group=group))+coord_fixed(1.3)

ggmap(get_map(location = "New York",source = "google",zoom = 4))+
  geom_point(data=unique_visits_window,aes(y=lat,x=lon,size=total),alpha=0.5)

#write.table(bat_mentions,file = "bat_mentions_maritimes.tab",quote = F,row.names = F,col.names = T,sep = "\t")

# fix date
cache_with_bats$date <- mdy(cache_with_bats$date)
cache_with_bats$year <- year(cache_with_bats$date)
cache_with_bats$month <- month(cache_with_bats$date)
cache_with_bats$day <- day(cache_with_bats$date)

relevant_dat<-subset(cache_with_bats, date < mdy("1-1-2014") & date > mdy("1-1-2009"))

month_activity<- relevant_dat %>% group_by(month,i) %>% summarize(total=length(date))

ggplot(month_activity,aes(x=month,y=total,color=i))+
  geom_point()+geom_line()+
  labs(y="Total Visits (2008-2013)")+
  scale_x_discrete(limits=c(1:12),labels=month.name)+
  theme(axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12,angle = -90),
        axis.title = element_text(size=12,face="bold"),
        legend.position="none")

ggsave("cache_visits_month.png",month_p,dpi=300)

ggplot(relevant_dat,aes(x=year,fill=i))+
  geom_histogram(binwidth = 1,color="black")+
  scale_x_discrete(limits=c(seq(2008,2013,1)),labels=c(seq(2008,2013,1)))+
  # facet_grid(i~.)+
  labs(y="Number of Unique Visits")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text = element_text(size=12),
        axis.title = element_text(size=12,face="bold"),
        strip.text = element_text(size=12))

ggsave(filename = "cache_visits_year.png",plot = years_p,dpi = 300,height = 12,units = "in")

binary_records<- relevant_dat %>% group_by(i,users,status) %>% summarize(total=length(date))
binary_spread<-spread(binary_records,key = i,value = total)
binary_spread<-binary_spread %>% replace(., is.na(.), 0) %>% replace(., .==2, 1)

# proportion of users that visit more than one cache
sum(rowSums(binary_spread[,3:7]) > 1) / nrow(binary_spread)

# number of users that visited more that one cave
between.visits<-ddply(ddply(relevant_dat,.(user,i,date),summarise,sum.cave=length(date)),.(user),summarise,
                      betweenvisits=(diff(date)))

sum(between.visits$betweenvisits < 14)

# do users visit more than one site?
png("cache_visits.png",res = 300,width = 1200,height = 1200)
upset(data = binary_spread,nsets = 6,order.by = "degree",decreasing = F,mainbar.y.label = "Number of Unique Visits at Site(s)",
      sets.bar.color = rev(c("#A3A528","#E675F3","#EC746C","#43B0F6","#63C07E")))
dev.off()

####
dat<-read.csv("~/Fragment_analyses/comb_binned.csv",header=T)

final_results<-NULL
for(i in c("NB","ON","PE","US","NS","QU")){
  s<-dat[grep(i,dat$isolate),]
  s$region<-(i)
  final_results<-rbind(final_results,s)
}

final_results$region<-gsub("NB","New Brunswick", final_results$region)
final_results$region<-gsub("NS","Nova Scotia", final_results$region)
final_results$region<-gsub("ON","Ontario", final_results$region)
final_results$region<-gsub("QU","Quebec", final_results$region)
final_results$region<-gsub("US","United States", final_results$region)
final_results$region<-gsub("PE","Prince Edward Island", final_results$region)

# Do you want just the maritimes, or all of our isolates?
final_results<-subset(final_results,region == "New Brunswick" | region == "Nova Scotia" | region == "Prince Edward Island")

row.names(final_results)<-final_results[,"isolate"]
final_results[,-c(1,11,12)]<-lapply(final_results[,-c(1,11,12)],as.integer)

pop<-df2genind(final_results[,-c(1,11,12)],ploidy = 1)
pop<-missingno(pop,type = "ignore")

# match cave data
location_dat<-read.csv("Pd_clean_data.csv",header = T)
pop@pop<-location_dat[location_dat$Isolate %in% final_results$isolate,]$cave
diversity_dat<-poppr(pop)

# back to the cache data
# if we set this to the number of total unique users, results are not significant.
# can we consider a users second or third visit to the same cave unique? even if they visit a different cache?
cave_counts<-ddply(relevant_dat,.(i),summarize,total=length(unique(user)))
counts_diversity<-merge(cave_counts,diversity_dat,by.x = "i",by.y = "Pop")

# number of shared genotypes?
mlg.crosspop(pop)
counts_diversity$shared.genotypes<-c(2,6,3,1,6)

m <- lm(total ~ (N-shared.genotypes)/N, counts_diversity,na.action = na.omit)
summary(m)

counts_diversity_p<-ggplot(subset(counts_diversity,N>1),aes(x=total,y=Hexp))+
  scale_y_reverse()+
  # scale_y_log10(limits=c(10,200),breaks=c(10,100),labels=c(10,100))+
  # scale_color_manual(values=c("#FF6164","#C7A000","#00C500","#00CC93","#00B0EF","#B45FFF","#FF00D7"),breaks=counts_diversity$gc,labels=counts_diversity$i,name="Site")+
  # scale_size_area(breaks = c(1,5,10),name="Number of Isolates")+
  geom_smooth(method = "lm",se = F,fullrange=T,color="black")+
  geom_point(aes(color=i),size=5)+
  # geom_text(aes(x=0.25,y=80),label=('r^2= 0.2, p = 0.31'),size=4)+
  labs(x="Total Visits",y="Allelic Diversity")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=12,face="bold"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12,face="bold"),
        legend.position = "none")

ggsave(filename = "geocache_diversity_unique.png",counts_diversity_p,dpi = 300,units = "in")

library(cowplot)
upset_p<-ggdraw()+draw_image("cache_visits.png")
legend<-get_legend(month_p_legend)

plot_grid(month_p,upset_p,counts_diversity_p,legend,scale = c(0.8,1,0.8,1),labels = c("A","B","C",""),nrow = 2,rel_widths = c(1,0.8,1,0.3))
ggsave(filename = "cache_visits_panel.png",plot = last_plot(),dpi = 300,width = 11,height = 8.5,units = "in")