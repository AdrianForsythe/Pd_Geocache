##### WNS Presence shape file
library(esri2sf)
library(tidyverse)
library(sf)

url2 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/2"
df2 <- as.data.frame(esri2sf(url2))
url5 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/5"
df5 <- as.data.frame(esri2sf(url5))
presence.df <- rbind(df2, df5)
presence.df <- presence.df %>%
  filter(!is.na(YR_CONFIRM) & YR_CONFIRM != " ") %>% 
  mutate(county = paste0(trimws(gsub(pattern = "County.*$",replacement = "",COUNTYNAME)),"-",STATEPROV),
         date=lubridate::dmy(paste0("01-01",gsub(pattern = "-.*$",replacement = "",YR_CONFIRM))),
         year=lubridate::year(date),
         rownumber = 1:nrow(.))
presence.poly <- as_Spatial(presence.df$geoms)

# pull out a unique list of county polys
uniq.df<-presence.df %>% distinct(county,.keep_all_T)

# convert coords from county poly to centroid points for each county. Then reproject.
united.xy <- uniq.df$geoms %>% st_centroid() %>% 
  st_transform(., "+proj=longlat +datum=WGS84")

wh<- st_sfc(st_point(c(41.86408,-74.081798)),crs = "+proj=longlat +datum=WGS84")
dist.origin<-st_distance(united.xy,united.xy)

dist.df<-as.data.frame(cbind(dist.origin[lower.tri(dist.origin)],
                             dist(lubridate::year(uniq.df$date))))
colnames(dist.df)<-c("geo","year")

summary(lm(geo~year,dist.df))
dist.df %>% ggplot(aes(y=geo,x=year)) + 
  geom_boxplot(aes(group=year)) + 
  geom_smooth(method = "lm",color="blue",se = T,na.rm = T)
  # scale_y_log10()

full.yc<-expand.grid(uniq.df[,c("year","county")])
full.yc$yc<-paste0(full.yc$year,"-",full.yc$county)
uniq.df$yc<-paste0(uniq.df$year,"-",uniq.df$county)

full.yc$incidence<-ifelse(full.yc$yc %in% uniq.df$yc,1,0)

write.csv(x = full.yc,file = "data/full-county-year-incidence.csv")

length(st_distance(united.xy))

uniq.df %>% 


dist_dist<-as.matrix(st_distance(united.xy))
year_dist<-matrix(full.yc$incidence,nrow = 551,ncol = 551)
plot(dist_dist,year_dist)

full.yc %>% mutate(incidence=incidence/n_distinct(county)) %>% ggplot(aes(x=year,y=incidence))+geom_bar(stat = "identity")
