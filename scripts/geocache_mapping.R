##### Create maps of Geocache records
## Not necessary at the moment
## have to switch to a non-Google API maps solution
mapping <- function (gc_dat,scrape_dat) {
  all_results_merge <- merge(scrape_dat, gc_dat, by.x = "i", 
                             by.y = "url", all = T)
  all_results_merge$lat <- as.numeric(gsub("\xb0 ", ".", gsub("[.]", 
                                                              "", gsub(pattern = "N ", replacement = "", all_results_merge$lat))))
  all_results_merge$lon <- as.numeric(gsub("\xb0 ", ".", gsub("[.]", 
                                                              "", gsub(pattern = "W ", replacement = "-", all_results_merge$lon))))
  all_results_merge$date <- mdy(all_results_merge$date)
  all_results_merge$year <- year(all_results_merge$date)
  all_results_merge$month <- month(all_results_merge$date)
  all_results_merge$month.year <- ymd(all_results_merge$date)
  all_visits_window <- all_results_merge %>% filter(status %in% 
                                                      c("Publish Listing", "Found it", "Didn't find it"))
  all_summary <- all_visits_window %>% group_by(i, lat, lon, 
                                                year, month) %>% summarise(total = length(users))
  register_google(key = "AIzaSyDw5appsfJ_gWd45-AeYe_WTT2VvI8kXhQ")
  map <- get_map(c(right = -57, left = -96, top = 52, bottom = 32), 
                 zoom = 5, source = "stamen", maptype = "terrain-background", 
                 scale = "auto")
  n <- length(unique(all_summary$year))
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 
                                    "qual", ]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, 
                             rownames(qual_col_pals)))
  ggmap(map) + geom_path(data = all_visits_window, aes(y = lat, 
                                                       x = lon, group = users), color = "red", alpha = 0.3) + 
    geom_point(data = all_summary, aes(y = lat, x = lon, 
                                       size = total, color = as.factor(year)), alpha = 0.5) + 
    theme_classic() + theme(axis.text = element_text(size = 10), 
                            axis.title = element_text(size = 12), legend.position = "none")
  ggsave(filename = "figures/num-geocache.png", plot = last_plot())
  num.geocache <- all_visits_window %>% group_by(date) %>% 
    summarise(total = length(unique(i)))
  ggplot(num.geocache, aes(x = date, y = total)) + geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", se = F, color = "red", size = 2) + 
    geom_vline(xintercept = as.numeric(as.Date("2008-01-01")), 
               linetype = "dashed", color = "blue", size = 2) + 
    labs(x = "Date", y = "Number of Geocache Sites at Caves/Mines") + 
    geom_text(aes(x = as.Date("2008-01-01"), y = 35, label = "WNS Introduced"), 
              hjust = -0.1, size = 4) + theme_classic() + theme(axis.text = element_text(size = 10), 
                                                                axis.title = element_text(size = 12))
  ggsave(filename = "figures/num-geocache-year.png", plot = last_plot())
  user.activity.date <- all_visits_window %>% group_by(date, 
                                                       GC) %>% summarise(total = n())
  ggplot(user.activity.date, aes(x = date)) + geom_histogram(color = "black", 
                                                             fill = "gray") + labs(x = "Date", y = "Number of Unique Visitors") + 
    geom_vline(xintercept = as.numeric(as.Date("2008-01-01")), 
               linetype = "dashed", color = "blue", size = 2) + 
    geom_text(aes(x = as.Date("2008-01-01"), y = 2100, label = "WNS Introduced"), 
              hjust = -0.1, size = 4) + theme_classic() + theme(axis.text = element_text(size = 10), 
                                                                axis.title = element_text(size = 12))
  ggsave(filename = "figures/max-visits-date.png", plot = last_plot())
  summary(lmer(total ~ date + (1 | GC), data = user.activity.date))
  p <- ggmap(map) + geom_point(data = all_visits_window, aes(y = lat, 
                                                             x = lon, group = users)) + transition_reveal(along = date, 
                                                                                                          keep_last = F) + shadow_wake(wake_length = 0.1, alpha = T) + 
    ggtitle("Year: {frame_along}") + theme_classic() + theme(axis.text = element_text(size = 10), 
                                                             axis.title = element_text(size = 12), legend.position = "none")
  anim_save("figures/users_year.gif", animation = p)
  travellers <- all_visits_window %>% group_by(users) %>% mutate(finds = n()) %>% 
    filter(finds > 1 & status %in% c("Found it", "Didn't find it"))
  for (i in unique(travellers$users)) {
    d <- travellers[travellers$users == i, ]
    f <- d[!duplicated(d$i), ]
    m <- distm(f[, c("lat", "lon")], fun = distHaversine)
    diag(m) <- NA
    colnames(m) <- f$GC
    cbind.data.frame((m[upper.tri(m, diag = T)]), f)
  }
  list(time = Sys.time(), tempfile = tempfile())
}