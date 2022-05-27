##### Create maps of Geocache records
## Not necessary at the moment
## have to switch to a non-Google API maps solution
gc_mapping <- function(eu_visits,na_visits,num.geocache,num.geocache.year,max.visits.date,travellers.date) {

  require(ggmap)
  require(gganimate)
  require(tidyverse)
  require(RColorBrewer)
  require(gifski)
  require(maps)
  require(maptools)
  
  eu_visits<-"workflow/data/gc-eu-scrape.csv"
  na_visits<-"workflow/data/gc-scrape.csv"
  num.geocache<-"workflow/figures/eu-num-geocache.png"
  num.geocache.year<-"workflow/figures/eu-num-geocache-year.png"
  max.visits.date<-"workflow/figures/eu-max-visits-date.png"
  travellers.date<-"workflow/figures/eu-travellers.gif"
  
  eu_visits_window<-read.csv(eu_visits,header=TRUE) %>% 
    mutate(Year = lubridate::year(lubridate::ymd(Date))) %>%
    filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing") &
           Year != 2021)

  all_summary <- eu_visits_window %>%
                  # mutate(month = month(date)) %>%
                  group_by(GC,Year,lat,lon) %>%
                  summarise(total = n_distinct(User))

  ggmap::register_google(key = API_KEY)
  map <- get_stamenmap(c(left=-11.04,bottom=35.2,right=41.44,top=60.08),zoom = 5, source = "stamen", maptype = "terrain-background", scale = "auto")
  n <- length(unique(all_summary$Year))
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == "qual", ]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

  n_gc<-ggplot()+
    borders("world",fill="gray") +
    # geom_sf(data=presence_df,aes(geometry=geoms))+
    geom_path(data = eu_visits_window, aes(y = lon,x = lat, group = User), color = "red", alpha = 0.3) +
    geom_point(data = all_summary, aes(y = lon, x = lat),color="black") +
    coord_sf(xlim = c(-11.04, 41.44), ylim = c(35.2, 60.08))+
    theme_classic() +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          legend.position = "none",
          panel.background = element_rect(fill = 'skyblue'))
  ggsave(filename = num.geocache, plot = n_gc,dpi=300)

  n_gc_y <- eu_visits_window %>% group_by(Year) %>%
    summarise(total = n_distinct(GC)) %>% ungroup() %>%
    ggplot() +
    geom_bar(aes(x = Year,y=total),color = "black", fill = "gray",stat="identity") +
    # geom_smooth(method = "lm", se = F, color = "red", size = 2) +
    geom_vline(xintercept = 2008,
               linetype = "dashed", color = "blue", size = 2) +
    labs(x = "Year", y = "Number of Geocache Sites at Caves/Mines") +
    geom_text(aes(x = 2008, y = 25, label = "WNS Introduced"),
              hjust = 1.5, size = 4) + theme_classic() + theme(axis.text = element_text(size = 10),
                                                               axis.title = element_text(size = 12))
  
    theme_classic() +
    theme(axis.text = element_text(size = 10),axis.title = element_text(size = 12))
  ggsave(filename = num.geocache.year, plot = n_gc_y,dpi=300)

  m_v_d <- eu_visits_window %>%
    group_by(Year) %>%
    summarise(total = n_distinct(User)) %>% ungroup() %>%
    ggplot() +
    geom_bar(aes(x = Year,y=total),color = "black", fill = "gray",stat="identity") +
    labs(x = "Year", y = "Number of Unique Visitors") +
    geom_vline(xintercept = 2008,linetype = "dashed", color = "blue", size = 2) +
    geom_text(aes(x = 2008, y = 200, label = "WNS Introduced"),
              hjust = 1.5, size = 4) + theme_classic() + theme(axis.text = element_text(size = 10),
                                                                axis.title = element_text(size = 12))
  ggsave(filename = max.visits.date, plot = m_v_d,dpi=300)

  #####
  na_visits_window<-read.csv(na_visits,header=TRUE) %>% 
    mutate(Year = lubridate::year(lubridate::ymd(Date))) %>%
    filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing") &
             Year != 2021)
  
  all_visits_window<-na_visits_window %>% 
    left_join(eu_visits_window,by="User",suffix=c(".na",".eu"))
  
  all_visits_window %>% filter(!is.na(GC.na) & !is.na(GC.eu)) %>% 
    summarise(n=n_distinct(User))
  
  distinct.eu.visits<-all_visits_window %>% filter(!is.na(GC.na) & !is.na(GC.eu)) %>% 
    distinct(GC.na,GC.eu,.keep_all=T) %>% # make sure these are unique trips to europe!
    group_by(User,Year.na,Year.eu) %>% 
    summarise(time=difftime(Date.eu,Date.na,units = "days")) %>% 
    mutate(viability.window=ifelse(abs(time)<=30,1,0),
           direction=ifelse(time<0,"to-europe","to-na"))

  # most visits are separated by long periods of time.
  distinct.eu.visits %>% 
    ggplot(aes(abs(time)))+
    geom_histogram(fill="gray",color="black")+
    geom_vline(xintercept = 30,color="red",linetype="dashed",size=2)+
    labs(x="Time (days)",y="Count")+
    theme_classic()
  
  # few users made visits within the viability window...
  distinct.eu.visits %>% 
    filter(viability.window==1 & direction == "to-europe") %>% 
    summarise(n=n_distinct(User))

  distinct.eu.visits %>% 
    filter(viability.window==1 & direction == "to-na") %>% 
    summarise(n=n_distinct(User))
  
  
  travellers<-all_visits_window %>% 
    sample_n(size = 100) %>%
    ggplot(group=seq_along(User))+
    borders("world",fill="gray") +
    coord_sf(xlim = c(-125, 41.44), ylim = c(27.5, 60.08))+
    # geom_segment(aes(x=lat.na,y=lon.na,xend=lat.eu,yend=lon.eu,group=User),alpha=0.25)+
    ggtitle("Year: {frame_along}") +
    transition_states(as.Date(Date.na))+
    shadow_wake(wake_length = 0.1, alpha = T) +
    geom_point(data=all_visits_window,aes(y=lon.na, x = lat.na,group=1),color="blue",size=2)+
    geom_point(data=all_visits_window,aes(y=lon.eu, x = lat.eu,group=1),color="red",size=2)+
    # shadow_wake(wake_length = 0.1, alpha = T) +
    # ease_aes('linear')
    theme_classic() +
    theme(axis.text = element_text(size = 10),
          panel.background = element_rect(fill = 'skyblue'))
  save_animation(travellers,filename=travellers.date)
    # anim_save(animate(travellers,renderer = gifski_renderer(),height=800,width=1600),filename=travellers.date)
  
  list(time = Sys.time(), tempfile = tempfile())
}
gc_mapping(snakemake@input[[1]],snakemake@input[[2]],snakemake@output[[1]],snakemake@output[[2]],snakemake@output[[3]],snakemake@output[[4]])
