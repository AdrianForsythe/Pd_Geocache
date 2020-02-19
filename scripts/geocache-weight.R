# summary of GC finds
# site coords are included here in order to grab the centroid later
county_visits <- relevant.records %>%
  filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
  group_by(GC,Year,coords.x1,coords.x2) %>%
  summarise(total = length(User))

county_visits.p <- as_Spatial(st_as_sf(county_visits,
                                       coords = c("coords.x1","coords.x2"), 
                                       crs = 4326, agr = "constant"))

# index for sites that match up with polys
num<-sp::over(county_visits.p, as_Spatial(presence.df$geoms),fn=NULL)

# pull out only the polys that we care about
uniq.poly<-presence.poly[na.omit(unique(num)),]

# convert coords from poly to centroid points of counties. Then reproject.
united.xy <- st_as_sf(uniq.poly) %>% st_centroid() %>% 
  st_transform(., "+proj=longlat +datum=WGS84")

# construct neighbour list from county centroids
county.n <- poly2nb(as_Spatial(st_as_sf(uniq.poly)), row.names = names(uniq.poly))

# spatial weights of neighbourlist
county.w <- nb2listw(county.n,zero.policy = TRUE)

# add names so that we can merge to the df later
names(county.w$weights) <- names(uniq.poly)

# turn NULL's into 0's
county.w$weights[sapply(county.w$weights, is.null)] <- 0

# then unlist
weights.long<-unlist(county.w$weights,use.names = T)

updated<-presence.df[na.omit(unique(num)),]
updated$FID<-names(uniq.poly)

# fix the incidence year for WNS
updated$WNS_MAP_YR <- ymd(gsub("-.+", "/01/01", updated$WNS_MAP_YR))

# updated$YR_SUSPECT <- ymd(gsub("-.+", "/01/01", updated$YR_SUSPECT))
# updated$YR_CONFIRM <- ymd(gsub("-.+", "/01/01", updated$YR_CONFIRM))

# create binary incidence value
updated$incidence <- ifelse(is.na(updated$WNS_MAP_YR),0,1)

updated.weights<-merge(weights.long,updated)
updated.weights$x[is.na(updated.weights$x)]<-0
colnames(updated.weights)[1]<-"gc.weight"

write.csv(updated.weights,"data/gc-weights.csv")

