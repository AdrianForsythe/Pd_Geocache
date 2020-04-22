  all_results_merge <- scraped %>%
    filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
    left_join(gc_filtered_dat, by = "GC") %>%
    mutate(county.state = paste0(county,"-",province.state),
    Year = year(ymd(Date)))
  
  # count users at each site per year  
  # we do this again at a later step, but this is useful to tell if there are any sites that don't get visited in a year
  geocache.locs <- all_results_merge %>% 
    group_by(GC, Year,lat.x,lon.x) %>%  
    distinct(User) %>% 
    tally(name = "total")
  
  # order of coords matters!
  geocache.coords <- as_Spatial(st_as_sf(geocache.locs, coords = c("lat.x","lon.x"), crs = 4326, agr = "constant"))
  
  # create an index of matching polygons from presence data
  index.df <- as.data.frame(sp::over(geocache.coords, presence.poly, returnList = F))
  
  # iterate through index and pull out columns
  colnames(index.df) <- "index"
  results <- NULL
  for (i in index.df$index) {
    u <- presence.df[i, "STATEPROV"]
    v <- presence.df[i, "county.state"]
    w <- presence.df[i, "SAMPLEDATE"]
    x <- presence.df[i, "YR_SUSPECT"]
    y <- presence.df[i, "YR_CONFIRM"]
    z <- presence.df[i, "WNS_MAP_YR"]
    results <- rbind.data.frame(results, cbind(u, v, w, x, y, z))
  }
  colnames(results) <- c("county.state", "county", "sample.date", "yr.suspect", "yr.confirm", "wns.map.yr")
  
  # bind results with coordinates of geocache sites (they are in the same order as the oringal index)
  geocache.presence.df <- cbind.data.frame(geocache.coords, results)
  
  # merge with our scraped dataset of geocache records
  # we want all, because there are a few sites that did not get visited some years
  presence.scrape <- merge(geocache.presence.df, scraped, by = "GC",all=TRUE)
  
  # recode sites without visits to 0
  is.na(presence.scrape$total) <- 0
  
  # fix date format
  presence.scrape <- presence.scrape %>%
    mutate(date = ymd(Date),
    wns.map.yr = ymd(gsub("-.+", "/01/01", wns.map.yr)),
    r.suspect = ymd(gsub("-.+", "/01/01", yr.suspect)),
    yr.confirm = ymd(gsub("-.+", "/01/01", yr.confirm)))
  
  # filter out geocache logs that were not made before WNS was "suspected"
  # we don't was to discard visits that would have preceeded introdution of infections!
  relevant.records <- presence.scrape %>% 
    filter(wns.map.yr >= Year)
  
  write.csv(relevant.records,"data/relevant-records.csv")
  
  # relevant.records.wpoly<-left_join(relevant.records,presence.df,by=c("county"="COUNTYNAME"))
  # saveRDS(relevant.records.wpoly,file = "data/relevant-records-withPoly.RDS")
  
  list(time = Sys.time(), tempfile = tempfile())