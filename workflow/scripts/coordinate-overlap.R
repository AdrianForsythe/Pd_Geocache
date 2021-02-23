require(ggmap)
require(tidyverse)
require(sf)
require(sp)
require(lubridate)

GetOverlaps<-function(scraped,presence.df,presence.poly,relevant_records){
  
  gc.records<-read.csv(scraped,header=TRUE) %>% 
    mutate(Year = lubridate::year(lubridate::ymd(Date)))

  presence_df<-readRDS(presence.df) %>% rowid_to_column("poly.index")
  presence_poly<-readRDS(file = presence.poly)
  
  all_results_merge <-gc.records %>%
    filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing"))

  # count users at each site per year
  # we do this again at a later step, but this is useful to tell if there are any sites that don't get visited in a year
  geocache.locs <- all_results_merge %>%
    group_by(GC, Year,lat,lon) %>%
    distinct(User) %>%
    tally(name = "total")

  # order of coords matters!
  geocache.coords <- as_Spatial(st_as_sf(all_results_merge, coords = c("lat","lon"), crs = 4326, agr = "constant"))

  # create an index of matching polygons from presence data
  geocache.coords$poly.index <- over(geocache.coords, presence_poly, returnList = F)
  
  # bind results with coordinates of geocache sites (they are in the same order as the oringal index)
  geocache.presence.df <- geocache.coords[!is.na(geocache.coords$poly.index),] %>%  
    merge(.,presence_df,by="poly.index")

  # merge with our scraped dataset of geocache records
  # we want all, because there are a few sites that did not get visited some years
  presence.scrape <-merge(gc.records,geocache.presence.df,by = c("GC","User","Title","Log","Date","Type")) %>% 
    rename(GC.Date=Date) %>% 
    mutate(wns.map.yr = lubridate::ymd(gsub("-.+", "/01/01", WNS_MAP_YR)),
           r.suspect = lubridate::ymd(gsub("-.+", "/01/01", YR_SUSPECT)),
           yr.confirm = lubridate::ymd(gsub("-.+", "/01/01", YR_CONFIRM)))

  # we don't was to discard visits that would have preceeded introdution of infections!
  # filter out geocache logs that were not made before WNS was "suspected"
  write.csv(dplyr::select(presence.scrape,-"geoms"),relevant_records)

  # relevant.records.wpoly<-left_join(relevant.records,presence.df,by=c("county"="COUNTYNAME"))
  # saveRDS(relevant.records.wpoly,file = "data/relevant-records-withPoly.RDS")

}

GetOverlaps(snakemake@input[[1]],snakemake@input[[2]],snakemake@input[[3]],snakemake@output[[1]])
