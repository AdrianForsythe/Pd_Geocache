##### Create maps of Geocache records
## Not necessary at the moment
## have to switch to a non-Google API maps solution

gc_mapping <- function(presence.df,relevant.records,num.geocache,num.geocache.year,max.visits.date,users.year.gif) {

  require(ggmap)
  require(gganimate)
  require(tidyverse)
  require(RColorBrewer)
  require(gifski)
  require(maps)
  require(maptools)
  
  # for testing
  presence.df<-"workflow/data/presence.df.rds"
  relevant.records<-"workflow/data/relevant-records.csv"
  num.geocache<-"workflow/figures/num-geocache.png"
  num.geocache.year<-"workflow/figures/num-geocache-year.png"
  max.visits.date<-"workflow/figures/max-visits-date.png"
  users.year.gif<-"workflow/figures/users_year.gif"
  
  presence_df<-readRDS(presence.df)

  all_visits_window <- read.csv(relevant.records) %>%
                  filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing"))

  # average distance travelled by users
  # all_visits_window %>% group_by(GC) %>% 
    # pivot_wider(id_cols = "User",names_from = "GC",values_from = c("coords.x1","coords.x2"))
    # group_by(User) %>% summarise()
  
  all_summary <- all_visits_window %>%
                  # mutate(month = month(date)) %>%
                  group_by(GC,gc.year,coords.x1,coords.x2) %>%
                  summarise(total = length(User))

  ggmap::register_google(key = "AIzaSyDw5appsfJ_gWd45-AeYe_WTT2VvI8kXhQ")
  map <- get_stamenmap(c(right = -57, left = -125, top = 52, bottom = 32),
                 zoom = 5, source = "stamen", maptype = "terrain-background", scale = "auto")
  n <- length(unique(all_summary$year))
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == "qual", ]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))


  n_gc<-ggplot()+
    borders("world",fill = "gray") +
    borders("state",fill="gray") +
    # maps::map("world",fill = TRUE,col='white',boundary = 'black',xlim = c(-125,-57),ylim = c(32,52))+
    # maps::map("lakes", add=TRUE, fill=TRUE, col='lightblue', boundary='black',xlim = c(-125,-57),ylim = c(32,52))
    # geom_sf(data=presence_df,aes(geometry=geoms,fill=WNS_MAP_YR))+
    geom_path(data = all_visits_window, aes(y = coords.x2,x = coords.x1, group = User), color = "red", alpha = 0.3) +
    geom_point(data = all_summary, aes(y = coords.x2, x = coords.x1),color="black") +
    coord_sf(xlim = c(-125, -57.5), ylim = c(27.5, 55))+
    scale_fill_viridis_d()+
    # coord_sf(xlim = c(-96, -57), ylim = c(32, 52))+
    theme_classic() +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          legend.position = "none",
          panel.background = element_rect(fill = 'skyblue'))
  ggsave(filename = num.geocache, plot = n_gc,dpi=300)

  n_gc_y <- all_visits_window %>% group_by(GC.Date) %>%
    summarise(total = n_distinct(GC)) %>% ungroup() %>%
    ggplot(aes(x = as.Date(GC.Date))) +
    geom_histogram(color = "black", fill = "gray") +
    # geom_bar(color = "black", fill = "gray",stat="identity") +
    # geom_smooth(method = "lm", se = F, color = "red", size = 2) +
    geom_vline(xintercept = as.Date("2008-01-01"),
               linetype = "dashed", color = "blue", size = 2) +
    labs(x = "Date", y = "Number of Geocache Sites at Caves/Mines") +
    # geom_text(aes(x = as.Date("2008-01-01"), y = 35, label = "WNS Introduced"),hjust = -0.1, size = 4) +
    theme_classic() +
    theme(axis.text = element_text(size = 10),axis.title = element_text(size = 12))
  ggsave(filename = num.geocache.year, plot = n_gc_y,dpi=300)

  m_v_d <- all_visits_window %>%
    group_by(GC,GC.Date) %>%
    summarise(total = n_distinct(User)) %>% ungroup() %>%
    ggplot(aes(x = as.Date(GC.Date))) +
    geom_histogram(color = "black", fill = "gray") +
    labs(x = "Date", y = "Number of Unique Visitors") +
    geom_vline(xintercept = as.Date("2008-01-01"),
               linetype = "dashed", color = "blue", size = 2) +
    annotate(geom = "text",y=750,x=as.Date("2002-01-01"), label = "WNS Introduced",
              hjust = -0.1, size = 4) + 
    theme_classic() + theme(axis.text = element_text(size = 10),
                                                                axis.title = element_text(size = 12))
  ggsave(filename = max.visits.date, plot = m_v_d,dpi=300)

  all_visits_window<-all_visits_window %>% mutate(GC.Date=as.Date(GC.Date))

  p<-ggplot()+
    borders("world",fill="gray") +
    geom_sf(data=presence_df,aes(geometry=geoms))+
    coord_sf(xlim = c(-125, -57.5), ylim = c(27.5, 55))+
    geom_point(data=all_visits_window,aes(y=lon, x = lat,group=User),color="blue",size=3)+
    ggtitle("Year: {frame_along}") +
    transition_states(GC.Date) +
    shadow_wake(wake_length = 0.1, alpha = T) +
    ease_aes('linear')
    # theme_classic() +
    # theme(axis.text = element_text(size = 10),
    # axis.title = element_text(size = 12), legend.position = "none")
    animate(p, renderer = gifski_renderer(),overwrite=TRUE)
    anim_save(filename=users.year.gif,animation=p)

  travellers <- all_visits_window %>%
  group_by(User) %>%
  mutate(finds = n()) %>%
    filter(finds > 1 & Type %in% c("Type.found_it", "Type.didnt_find_it"))

  for (i in unique(travellers$User)) {
    d <- travellers[travellers$User == i, ]
    f <- d[!duplicated(d$GC), ]
    m <- distm(f[, c("coords.x1", "coords.x2")], fun = distGeo)
    diag(m) <- NA
    colnames(m) <- f$GC
    cbind(m[lower.tri(m, diag = T)], f)
  }
  list(time = Sys.time(), tempfile = tempfile())
}
gc_mapping(snakemake@input[[1]],snakemake@input[[2]],snakemake@output[[1]],snakemake@output[[2]],snakemake@output[[3]])
