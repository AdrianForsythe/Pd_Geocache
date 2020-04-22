# before we start, make counties actually unique by concatenating county + state
# wouldn't have this problem if US was more original with their naming...
# and fix year in presence.df (I think I do this somewhere early but I can't find it)
presence.df <- presence.df %>% 
  mutate(county = paste0(COUNTYNAME,"-",STATEPROV),
         year=gsub(pattern = "-.*$",replacement = "",WNS_MAP_YR))

# summary of GC finds
# site coords are included here in order to grab the centroid later
# There are subtle differences in xy coords! 
# Not sure how this happened.... at some point maybe too different methods for finding centroid (or similar) were concatenated?
# going to take the median between the two sets for coords for now...
county_visits <- relevant.records %>%
  filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
  group_by(county,Year) %>% 
  mutate(lat=median(lat),
         lon=median(lon)) %>% 
  ungroup() %>%
  group_by(county,Year,lat,lon) %>% 
  distinct(User) %>%
  summarise(total = length(User))

county_visits.p <- as_Spatial(st_as_sf(county_visits,
                                       coords = c("lat","lon"), 
                                       crs = 4326, agr = "constant"))

# at the spatial locations of object x retrieves the indexes or attributes from spatial object y
# index for sites that match up with polys
# these are the county polygons that match up with sites
num<-sp::over(county_visits.p, as_Spatial(presence.df$geoms),fn=NULL)

# pull out a unique list of county polys
# not from Cali or Wash for now!
uniq.df<-presence.df[na.omit(unique(num)),] %>% filter(STATEPROV != c("California","Washington"))

# convert coords from county poly to centroid points for each county. Then reproject.
united.xy <- uniq.df$geoms %>% st_centroid() %>% 
  st_transform(., "+proj=longlat +datum=WGS84")

# need the number of visits at county level that match up with county centroids
# match centroid back to county
# then grab total vists

county.shared.users <- relevant.records %>%
  # getting rid of visits that don't matter
  filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing") &
                       county.state != c("California","Washington")) %>%
  # getting rid of users that don't visit sites in other counties
  group_by(User) %>%
  mutate(total = length(unique(county))) %>% filter(total > 1)

# get the number of shared users
shared.users<-NULL
for (year in unique(as.character(county.shared.users$wns.map.yr))) {
  s<-subset(county.shared.users,wns.map.yr==year)
  for (county1 in unique(s$county)) {
    for (county2 in rev(unique(s$county))) {
      num.shared<-length(intersect(s[which(s$county == county1),]$User,
                          s[which(s$county == county2),]$User))
      shared.users<-as.data.frame(rbind(shared.users,cbind(year,county1,county2,num.shared)))
    }
  }
}

shared.users<-shared.users[shared.users$county1!=shared.users$county2,]

# create list of all combinations
all.years <- shared.users %>% expand(year,county1,county2)

# merge with the data we have, filling in gaps
all.shared.users<-merge(shared.users,all.years,all=TRUE)

# create binary incidence value
all.shared.users$incidence <- ifelse(is.na(all.shared.users$num.shared),0,1)

# fix year
all.shared.users$date <- ymd(all.shared.users$year)
all.shared.users$year <- year(all.shared.users$year)

# fix number of users
# put in NA's where there was no traffic between caves
all.shared.users$num.shared <-  as.numeric(replace_na(all.shared.users$num.shared,0))

county_rate<-all.shared.users %>% 
  arrange(date) %>%
  group_by(date) %>%
  summarise(county.inf.count = sum(incidence>0),
            uninf.counties = sum(incidence==0)) %>%
  mutate(inf.counties = cumsum(county.inf.count))

# cumulative number of infected and uninfected counties
all.shared.users$inf.counties<-county_rate[match(all.shared.users$date,county_rate$date),]$inf.counties
all.shared.users$uninf.counties<-county_rate[match(all.shared.users$date,county_rate$date),]$uninf.counties

# which counties are touching?
touching<-st_intersects(uniq.df$geoms,sparse = F)

touching.m <- as.matrix(touching)
rownames(touching.m)<-colnames(touching.m)<-uniq.df$county
touching.m2 <- reshape2::melt(touching.m)[reshape2::melt(upper.tri(touching.m))$value,]
names(touching.m2) <- c("county1","county2","touching")

# merge gc weights with adjacency score: 1 = touching, 0 = not touching
both.weights<-merge(all.shared.users,touching.m2,by=c("county1","county2"))
both.weights$touching<-if_else(both.weights$touching == TRUE,1,0)

# save
write.csv(both.weights,"data/gc-shared-users.csv")

ggplot(uniq.df$geoms)+
  borders("world") +
  borders("state") +
  geom_sf(aes(fill=uniq.df$WNS_MAP_YR))+
  coord_sf(xlim = c(-100, -60), ylim = c(32, 50))+
  # coord_sf(xlim = c(-100, -57.5), ylim = c(35, 50))+
  theme_bw()
ggsave("figures/counties-shared-users.png",plot=last_plot(),dpi=300)
