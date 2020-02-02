find_overlap <- function (...) {
  all_results_clean <- filter(scraped, status == c("Found it", "Didn't find it", "Owner Maintenance", "Publish Listing"))
  all_results_merge <- merge(all_results_clean, filtered_gc_dat, by.x = "i", by.y = "url", all = T)
  
  lat.dms <- do.call(rbind, strsplit(as.character(all_results_merge$lat),":"))
  lat.dec <- as.numeric(lat.dms[, 1]) + (as.numeric(lat.dms[,2]) + as.numeric(lat.dms[, 3])/60)/60
  all_results_merge$lat <- lat.dec
  
  lon.dms <- do.call(rbind, strsplit(as.character(all_results_merge$lon),":"))
  lon.dec <- as.numeric(lon.dms[, 1]) + (as.numeric(lon.dms[,2]) + as.numeric(lon.dms[, 3])/60)/60
  all_results_merge$lon <- lon.dec
  
  # char2dms(all_results_merge, chd = ":", chm = ":", chs = ":")
  
  all_results_merge$year <- year(mdy(all_results_merge$date))
  geocache.locs <- all_results_merge %>% 
    group_by(year) %>% 
    mutate(count = length(unique(i))) %>% 
    group_by(i, lat,lon, year, count) %>% 
    summarise(total = length(duplicated(users)))
  
  # order of coords matters!
  geocache.coords <- as_Spatial(st_as_sf(geocache.locs, coords = c("lon","lat"), crs = 4326, agr = "constant"))
  
  index.df <- as.data.frame(sp::over(geocache.coords, poly, returnList = F))
  
  colnames(index.df) <- "index"
  results <- NULL
  for (i in index.df$index) {
    u <- presence.df[i, "STATEPROV"]
    v <- presence.df[i, "COUNTYNAME"]
    w <- presence.df[i, "SAMPLEDATE"]
    x <- presence.df[i, "YR_SUSPECT"]
    y <- presence.df[i, "YR_CONFIRM"]
    z <- presence.df[i, "WNS_MAP_YR"]
    results <- rbind.data.frame(results, cbind(u, v, w, x, y, z))
  }
  colnames(results) <- c("state.prov", "county", "sample.date", "yr.suspect", "yr.confirm", "wns.map.yr")
  
  geocache.presence.df <- cbind.data.frame(geocache.coords, results)
  
  all_merge <- merge(geocache.presence.df, scraped, by = "i",all=TRUE)
  
  all_merge$date <- mdy(all_merge$date)
  all_merge$wns.map.yr <- ymd(gsub("-.+", "/01/01", all_merge$wns.map.yr))
  all_merge$yr.suspect <- ymd(gsub("-.+", "/01/01", all_merge$yr.suspect))
  all_merge$yr.confirm <- ymd(gsub("-.+", "/01/01", all_merge$yr.confirm))
  relevant.records <- all_merge %>% filter(wns.map.yr > date)
  relevant.records <- relevant.records[!is.na(relevant.records$i),]
  write.csv(relevant.records,"data/relevant-records.csv")
  list(time = Sys.time(), tempfile = tempfile())
}