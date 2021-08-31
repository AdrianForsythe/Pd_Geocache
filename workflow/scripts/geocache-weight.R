# summary of GC finds - GC
# polygons are included here in order to grab the centroid later

spatial_weight_matrix<-function(relevant.records,presence.df,weights,shared.users){

  require(tidyverse)
  require(sf)
  require(lwgeom)
  require(maps)
  
  # for testing
  relevant_records<-read.csv("workflow/data/relevant-records.csv",header=T)
  presence_df<-readRDS("workflow/data/presence.df.rds")
  weights<-"workflow/data/gc.weights.csv"
  
  relevant_records<-read.csv(relevant.records)
  # presence_df<-readRDS(presence.df)

  # pull out a unique list of county polys
  # exclude west coast right now
  uniq_df<-presence_df %>% 
    filter(!STATEPROV %in% c("California","Washington") ) %>% 
    distinct(county,.keep_all = T)
  
  # convert coords from county poly to centroid points for each county. Then reproject.
  
  # sf with spherical geometry?!?
  # see https://github.com/r-spatial/sf/issues/1762#issuecomment-900571711
  sf_use_s2(FALSE)
  
  united_xy <- uniq_df$geoms %>% st_centroid() %>%
    st_transform(., "+proj=longlat +datum=WGS84")

  county_visits <- relevant_records %>%
    filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
    group_by(county,year,lon,lat) %>%
    distinct(User) %>%
    summarise(total = length(User))

  ## Number of intersecting sites within a given radius (10km)
  site_visits<-relevant_records %>%
    filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
    group_by(GC,year,lon,lat) %>%
    distinct(User) %>%
    summarise(total = length(User)) %>%
    st_as_sf(coords=c("lat","lon"),crs = 4326)

  # n_local_neighbors <- lengths(sf::st_is_within_distance(site_visits, dist = 100000))

  # need the number of visits at county level that match up with county centroids
  # match centroid back to county
  # then grab total vists

  just_gc <- relevant_records %>% filter(!is.na(GC))

  # get the number of shared users
  shared_users<-NULL
  for (i_year in unique(just_gc$year)) {
    s<-filter(just_gc,year == i_year)
    for (county1 in unique(s$county)) {
      for (county2 in rev(unique(s$county))) {
          num_shared<-length(intersect(as.character(s[which(s$county == county1),]$User),
                                       as.character(s[which(s$county == county2),]$User)))
          shared_users<-as.data.frame(rbind(shared_users,cbind(i_year,county1,county2,num_shared)))
        }
      }
    }

  shared_users<-expand(shared_users,county1,county2,i_year) %>% left_join(shared_users,by=c("county1","county2","i_year"))
  shared_users<-shared_users[shared_users$county1!=shared_users$county2,]
  shared_users$i_year<-as.numeric(as.character(shared_users$i_year))
  shared_users$county1<-as.character(shared_users$county1)
  shared_users$county2<-as.character(shared_users$county2)

  # merge back to the original data
  all_shared_users <- presence_df %>%
    left_join(shared_users,by=c("year"="i_year","county"="county1")) %>%
     select(-geoms)

  # fix number of users
  # put in NA's where there was no traffic between caves
  all_shared_users$num_shared <- as.numeric(replace_na(all_shared_users$num_shared,0))

  # create binary incidence value
  all_shared_users$incidence <- ifelse(all_shared_users$YR_CONFIRM == " ",0,1)

  county_rate<-all_shared_users %>%
    arrange(date) %>%
    group_by(date) %>%
    summarise(county_inf_count = sum(incidence>0),
              uninf_counties = sum(incidence==0)) %>%
    mutate(inf_counties = cumsum(county_inf_count))

  # cumulative number of infected and uninfected counties
  all_shared_users$inf_counties<-county_rate[match(all_shared_users$date,county_rate$date),]$inf_counties
  all_shared_users$uninf_counties<-county_rate[match(all_shared_users$date,county_rate$date),]$uninf_counties

  # which counties are touching?
  touching<-st_intersects(uniq_df$geoms,sparse = F)

  touching_m <- as.matrix(touching)
  rownames(touching_m)<-colnames(touching_m)<-uniq_df$county
  touching_m2 <- reshape2::melt(touching_m)[reshape2::melt(upper.tri(touching_m))$value,]
  names(touching_m2) <- c("county","county2","touching")

  # merge gc weights with adjacency score: 1 = touching, 0 = not touching
  both_weights<-left_join(all_shared_users,touching_m2,by=c("county","county2"))
  both_weights$touching<-if_else(is.na(both_weights$touching) | isFALSE(both_weights$touching),0,1)

  # save
  write.csv(x = both_weights,file = weights,quote = F)

  su.p<-uniq_df %>% 
    ggplot()+
    borders("world",fill = "white") +
    borders("state",fill = "white") +
    coord_sf(xlim = c(-125, -57.5), ylim = c(27.5, 55))+
    geom_sf(aes(fill=uniq_df$WNS_MAP_YR))+
    geom_path()
    # coord_sf(xlim = c(-100, -57.5), ylim = c(35, 50))+
    theme_bw()
  ggsave(shared.users,plot=su.p,dpi=300)
  # spatial weight matricies from neighbour list
  # county_m <- nb2mat(county_n, style = "B", zero.policy = T)
}
spatial_weight_matrix(snakemake@input[[1]],snakemake@input[[2]],snakemake@output[[1]],snakemake@output[[2]])
