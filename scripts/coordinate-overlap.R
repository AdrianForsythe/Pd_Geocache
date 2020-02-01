find_overlap <- function (gc_dat,scrape_dat,poly) {
  all_results_clean <- filter(scrape_dat, status == c("Found it", "Didn't find it", "Owner Maintenance", "Publish Listing"))
  all_results_merge <- merge(all_results_clean, gc_dat, by.x = "i", 
                             by.y = "url", all = T)
  lat.dms <- do.call(rbind, strsplit(as.character(all_results_merge$lat), 
                                     ":"))
  lat.dec <- as.numeric(lat.dms[, 1]) + (as.numeric(lat.dms[, 
                                                            2]) + as.numeric(lat.dms[, 3])/60)/60
  all_results_merge$lat <- lat.dec
  lon.dms <- do.call(rbind, strsplit(as.character(all_results_merge$lon), 
                                     ":"))
  lon.dec <- as.numeric(lon.dms[, 1]) + (as.numeric(lon.dms[, 
                                                            2]) + as.numeric(lon.dms[, 3])/60)/60
  all_results_merge$lon <- lon.dec
  char2dms(all_results_merge, chd = ":", chm = ":", chs = ":")
  all_results_merge$year <- year(mdy(all_results_merge$date))
  geocache.locs <- all_results_merge %>% group_by(year) %>% 
    mutate(count = length(unique(i))) %>% group_by(i, lat,lat, year, count) %>% summarise(total = length(!unique(users)))
  geocache.coords <- as_Spatial(st_as_sf(geocache.locs, coords = c("lat","lat"), crs = 4326, agr = "constant"))
  index.df <- as.data.frame(over(geocache.coords, poly, returnList = F))
  colnames(index.df) <- "index"
  results <- NULL
  for (i in index.df$index) {
    u <- df[i, "STATEPROV"]
    v <- df[i, "COUNTYNAME"]
    w <- df[i, "SAMPLEDATE"]
    x <- df[i, "YR_SUSPECT"]
    y <- df[i, "YR_CONFIRM"]
    z <- df[i, "WNS_MAP_YR"]
    results <- rbind.data.frame(results, cbind(u, v, w, x, 
                                               y, z))
  }
  colnames(results) <- c("state.prov", "county", "sample.date", 
                         "yr.suspect", "yr.confirm", "wns.map.yr")
  geocache.presence.df <- cbind.data.frame(geocache.coords, 
                                           results)
  all_merge <- merge(geocache.presence.df, all_results, by = "i", 
                     all = T)
  all_merge$date <- mdy(all_merge$date)
  all_merge$wns.map.yr <- ymd(gsub("-.+", "/01/01", all_merge$wns.map.yr))
  all_merge$yr.suspect <- ymd(gsub("-.+", "/01/01", all_merge$yr.suspect))
  all_merge$yr.confirm <- ymd(gsub("-.+", "/01/01", all_merge$yr.confirm))
  relevant.records <- all_merge[all_merge$wns.map.yr > all_merge$date, 
                                ]
  relevant.records <- relevant.records[!is.na(relevant.records$i), 
                                       ]
  list(time = Sys.time(), tempfile = tempfile())
}