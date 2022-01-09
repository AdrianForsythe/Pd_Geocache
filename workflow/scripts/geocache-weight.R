# summary of GC finds - GC
# polygons are included here in order to grab the centroid later

spatial_weight_matrix<-function(relevant.records,presence.df,weights,shared.users){
  
  require(sf)
  require(sp)
  require(tidyverse)
  require(lwgeom)
  require(maps)
  
  # sf with spherical geometry?!?
  # see https://github.com/r-spatial/sf/issues/1762#issuecomment-900571711
  sf_use_s2(FALSE)
  
  # for testing
  relevant_records<-read.csv("workflow/data/relevant-records.csv",header=T)
  presence_df<-readRDS("workflow/data/presence.df.rds")
  weights<-"workflow/data/gc.weights.csv"
  figure<-"workflow/figures/touching-inf-counties.png"
  
  # pull out a unique list of county polys
  # exclude west coast right now
  uniq_df<-presence_df %>% 
    filter(!STATEPROV %in% c("California","Washington")) %>% 
    distinct(county,geoms)

  # which counties are touching?
  touching<-st_intersects(uniq_df$geoms,sparse = T)
  
  # generate matrix
  touching_m <- as.matrix(touching)
  
  # names as counties
  rownames(touching_m)<-colnames(touching_m)<-uniq_df$county
  
  # melt to long format, rename vars, and remove diagonals of matrix
  touching_m2 <- touching_m %>% as.data.frame() %>% 
    rownames_to_column("county") %>%
    pivot_longer(cols = -"county",names_to = "county2",values_to = "touching") %>% 
    # distinct() %>% 
    filter(county!=county2)
  
  # distance based metrics
  # grab centroids
  p<-uniq_df$geoms %>% st_point_on_surface()
  
  # calculate distance matrix
  p.dist<-st_distance(p,p) %>% as.matrix()
  # names as counties
  rownames(p.dist)<-colnames(p.dist)<-uniq_df$county
    
  # generate a full list of absent/present/suspected records WNS for all counties
  full_df<-presence_df %>% 
    filter(!STATEPROV %in% c("California","Washington") ) %>% 
    expand(county,WNS_MAP_YR) %>% 
    mutate(WNS_STATUS="Absent") %>% 
    anti_join(presence_df,by=c("county","WNS_MAP_YR")) %>% 
    bind_rows(presence_df %>% select(county,WNS_MAP_YR,WNS_STATUS))

  # save for later
  write.csv(full_df,"workflow/data/full-occurrance-df.csv",row.names = F,quote = F)
  
  # important that the number of counties touching is consistent...
  total_touching<-full_df %>% 
    left_join(touching_m2,by=c("county"="county2")) %>% 
    # full_join(uniq_df,by=c("county.y"="county")) %>% 
    group_by(county.y,WNS_MAP_YR) %>% 
    summarise(n.touching=sum(touching,na.rm=T),
              n.inf=sum(touching[WNS_STATUS=="Confirmed"],na.rm = T),
              n.sus=sum(touching[WNS_STATUS=="Suspect"],na.rm = T),
              n.abs=sum(touching[!WNS_STATUS %in% c("Confirmed","Suspect")],na.rm = T),
              prop=n.inf/n_distinct(touching_m2$county))

  plot<-total_touching %>% 
    group_by(WNS_MAP_YR) %>% 
    summarise(n.inf=sum(n.inf,na.rm = T),
              n.sus=sum(n.sus,na.rm = T),
              prop=n.inf/n_distinct(touching_m2$county)) %>% 
    arrange(WNS_MAP_YR) %>% 
    mutate(cumulative=cumsum(n.inf)) %>% 
    ggplot(aes(x=WNS_MAP_YR,y=cumulative))+
    geom_vline(xintercept = "2007-08",size=2,colour="red",alpha=0.5)+
    geom_bar(aes(x=WNS_MAP_YR,y=prop*1000),stat = "identity",color="black",fill="gray") +
    geom_point(size=2)+
    geom_path(aes(group=1))+
    scale_y_continuous(sec.axis = sec_axis(~ . / 1000,name = "Proportion of total counties"))+
    # scale_x_date(breaks = 2008:2020)+
    labs(y="Cumulative number of adjacent counties with WNS confirmed",x="Year WNS confirmed in county")+
    theme_classic()
  ggsave(filename = figure,plot = plot,dpi = 300,width = 6,height = 6,units = "in")
  
  write.csv(x = total_touching,file="workflow/data/total-touching-infected.csv",quote = F,row.names = F)
  write.csv(x = touching_m2,file="workflow/data/total-touching.csv",quote = F,row.names = F)
  
  require(gganimate)
  require(transformr)
  
  uniq_df %>%
    left_join(total_touching,by=c("county","year")) %>% #%>% mutate(year=as.character(year))
    arrange(year) %>% 
    mutate(cumulative=cumsum(n.inf)) %>% 
  ggplot()+
    borders("world",fill = "white") +
    borders("state",fill = "white") +
    geom_sf(aes(fill=n.inf,geometry=geoms))+
    scale_fill_viridis_c()+
    coord_sf(xlim = c(-125, -57.5), ylim = c(27.5, 55))
  
  # convert coords from county poly to centroid points for each county. Then reproject.
  united_xy <- uniq_df$geoms %>% st_centroid() %>%
    st_transform(., "+proj=longlat +datum=WGS84")
  
  # county_visits <- relevant_records %>%
  #   filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
  #   group_by(county,year,lon,lat) %>%
  #   distinct(User) %>%
  #   summarise(total = length(User))
  
  ## Number of intersecting sites within a given radius (10km)
  # site_visits<-relevant_records %>%
  #   filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
  #   group_by(GC,year,lon,lat) %>%
  #   distinct(User) %>%
  #   summarise(total = length(User)) %>%
  #   st_as_sf(coords=c("lat","lon"),crs = 4326)
  
  # n_local_neighbors <- lengths(sf::st_is_within_distance(site_visits, dist = 100000))
  
  # need the number of visits at county level that match up with county centroids
  # match centroid back to county
  # then grab total vists
  
  just_gc <- relevant_records %>% filter(!is.na(GC))
  
  # get the number of shared users
  shared_users<-NULL
  for (i_year in unique(just_gc$presence.year)) {
    s<-filter(just_gc,presence.year == i_year)
    for (county1 in unique(s$county)) {
      for (county2 in rev(unique(s$county))) {
        num_shared<-length(intersect(as.character(s[which(s$county == county1),]$User),
                                     as.character(s[which(s$county == county2),]$User)))
        shared_users<-as.data.frame(rbind(shared_users,cbind(i_year,county1,county2,num_shared)))
      }
    }
  }
  
  all_combs<-shared_users %>% 
    expand(county1,county2,i_year)
  
  num_shared_users<-shared_users %>% 
    left_join(all_combs,by=c("county1","county2","i_year")) %>% 
    filter(county1!=county2) %>% 
    mutate(i_year=as.numeric(as.character(i_year)),
           county1=as.character(county1),
           county2=as.character(county2),
           # fix number of users, put in NA's where there was no traffic between caves
           num_shared = as.numeric(replace_na(num_shared,0)))
  
  # merge back to the original data
  # merge gc weights with adjacency score: TRUE = touching, FALSE/NA = not touching
  both.weights <- full_df %>% 
    left_join(touching_m2,by=c("county"="county2")) %>% 
    mutate(county.incidence = ifelse(WNS_STATUS == "Confirmed",0,1), # create binary incidence value
           year=lubridate::year(gsub("-.+","/01/01",WNS_MAP_YR))) %>% 
    right_join(num_shared_users,by=c("year"="i_year","county"="county1","county.y"="county2"))
  # arrange(WNS_MAP_YR) %>%
  # group_by(WNS_MAP_YR) %>%
  #mutate(sum(incidence))
  #mutate(inf_counties = n_distinct(county[incidence==1])) # cumulative number of infected and uninfected counties
  
  # save
  write.csv(x = both.weights,file = weights,quote = F)
  
  su.p<-uniq_df %>% 
    left_join(presence_shared_users) %>% 
    ggplot()+
    borders("world",fill = "white") +
    borders("state",fill = "white") +
    geom_sf(aes(fill=WNS_MAP_YR,geometry=geoms))+
    coord_sf(xlim = c(-125, -57.5), ylim = c(27.5, 55))+
    scale_fill_viridis_d()+
  theme_bw()
  ggsave(shared.users,plot=su.p,dpi=300)
  # spatial weight matricies from neighbour list
  # county_m <- nb2mat(county_n, style = "B", zero.policy = T)
}
spatial_weight_matrix(snakemake@input[[1]],snakemake@input[[2]],snakemake@output[[1]],snakemake@output[[2]])
