##### Create maps of Geocache records
## Not necessary at the moment
## have to switch to a non-Google API maps solution
mapping <- function (...) {
  
  all_visits_window <- relevant.records %>% 
                  filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing"))
  all_summary <- all_visits_window %>%
                  # mutate(month = month(date)) %>%
                  group_by(GC,Year,coords.x1,coords.x2) %>% 
                  summarise(total = length(User))
  
  register_google(key = "AIzaSyDw5appsfJ_gWd45-AeYe_WTT2VvI8kXhQ")
  map <- get_stamenmap(c(right = -57, left = -96, top = 52, bottom = 32), 
                 zoom = 5, source = "stamen", maptype = "terrain-background", scale = "auto")
  n <- length(unique(all_summary$Year))
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == "qual", ]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
  ggplot()+
    borders("world") +
    geom_sf(aes(geometry=presence.df$geoms))+
    geom_path(data = all_visits_window, aes(y = coords.x2,x = coords.x1, group = User), color = "red", alpha = 0.3) +
    geom_point(data = all_summary, aes(y = coords.x2, x = coords.x1),color="black") +
    # coord_sf(xlim = c(-125, -57.5), ylim = c(27.5, 55))+
    coord_sf(xlim = c(-96, -57), ylim = c(32, 52))+
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
          axis.title = element_text(size = 12),
          legend.position = "none")
  ggsave(filename = "figures/num-geocache.png", plot = last_plot())
  
  num.geocache <- all_visits_window %>% group_by(Date) %>%
    summarise(total = length(unique(GC)))
  
  ggplot(num.geocache, aes(x = Date)) + 
    geom_histogram(color = "black", fill = "gray") + 
    # geom_smooth(method = "lm", se = F, color = "red", size = 2) + 
    geom_vline(xintercept = as.numeric(as.Date("2008-01-01")), 
               linetype = "dashed", color = "blue", size = 2) + 
    labs(x = "Date", y = "Number of Geocache Sites at Caves/Mines") + 
    # geom_text(aes(x = as.Date("2008-01-01"), y = 35, label = "WNS Introduced"),hjust = -0.1, size = 4) + 
    theme_classic() + 
    theme(axis.text = element_text(size = 10),axis.title = element_text(size = 12))
  ggsave(filename = "figures/num-geocache-year.png", plot = last_plot())
  
  user.activity.date <- all_visits_window %>% group_by(Date, GC) %>% summarise(total = n())
  ggplot(user.activity.date, aes(x = Date)) + geom_histogram(color = "black", fill = "gray") + labs(x = "Date", y = "Number of Unique Visitors") + 
    geom_vline(xintercept = as.numeric(as.Date("2008-01-01")), 
               linetype = "dashed", color = "blue", size = 2) + 
    geom_text(aes(x = as.Date("2004-01-01"), y = 200, label = "WNS Introduced"), 
              hjust = -0.1, size = 4) + theme_classic() + theme(axis.text = element_text(size = 10), 
                                                                axis.title = element_text(size = 12))
  ggsave(filename = "figures/max-visits-date.png", plot = last_plot())
  
  # summary(lmer(total ~ Date + (1 | GC), data = user.activity.date))
  
  p <- ggmap(map) + 
    geom_point(data = all_visits_window[which(all_visits_window$Year<2008),], aes(y = coords.x2, x = coords.x1, group = User),color="blue")+
    geom_point(data = all_visits_window[which(all_visits_window$Year>2008),], aes(y = coords.x2, x = coords.x1, group = User),color="red")+ 
    transition_reveal(along = Date, keep_last = F) + shadow_wake(wake_length = 0.1, alpha = T) + 
    ggtitle("Year: {frame_along}") + 
    theme_classic() + 
    theme(axis.text = element_text(size = 10), 
    axis.title = element_text(size = 12), legend.position = "none")
  anim_save("figures/users_year.gif", animation = p)
  
  travellers <- all_visits_window %>% group_by(User) %>% mutate(finds = n()) %>% 
    filter(finds > 1 & Type %in% c("Type.found_it", "Type.didnt_find_it"))
  # for (i in unique(travellers$User)) {
  #   d <- travellers[travellers$User == i, ]
  #   f <- d[!duplicated(d$GC), ]
  #   m <- distm(f[, c("coords.x1", "coords.x2")], fun = distGeo)
  #   diag(m) <- NA
  #   colnames(m) <- f$GC
  #   cbind(m[lower.tri(m, diag = T)], f)
  # }
  list(time = Sys.time(), tempfile = tempfile())
}