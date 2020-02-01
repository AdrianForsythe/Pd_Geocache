#####
## Not necessary at this point
match_locations <- function (gc_dat,scrape_dat) {
  lat.dms <- do.call(rbind, strsplit(as.character(all_results_merge$lat), 
                                     ":"))
  lat.dec <- as.numeric(lat.dms[, 1]) + (as.numeric(lat.dms[,2]) + as.numeric(lat.dms[, 3])/60)/60
  all_results_merge$lat <- lat.dec
  lon.dms <- do.call(rbind, strsplit(as.character(all_results_merge$lon),":"))
  lon.dec <- as.numeric(lon.dms[, 1]) + (as.numeric(lon.dms[,2]) + as.numeric(lon.dms[, 3])/60)/60
  all_results_merge$lon <- lon.dec
  all_results_merge$year <- year(parse_date_time(all_results_merge$date, orders = "m/d/y"))
  geocache.locs <- na.omit(all_results_merge) %>% group_by(GC,lon, lat) %>% summarise(count = n())
  geocache.coords <- st_as_sf(geocache.locs, coords = c("lon","lat"), crs = 4326, agr = "constant")
  dat <- read.csv("data/comb_binned.csv", header = T)
  row.names(dat) <- dat[, "isolate"]
  keep_cols <- colnames(dat[, -c(1, 11)])
  drees_dat <- read.csv("data/Drees_microsats_binned.csv", 
                        header = T)
  drees_dat <- drees_dat[, c(2, 26:47)]
  colnames(drees_dat) <- tolower(colnames(drees_dat))
  colnames(drees_dat) <- gsub("_bins", "", colnames(drees_dat))
  rownames(drees_dat) <- drees_dat[, 1]
  all_dat <- rbind(dat[, keep_cols], drees_dat[, keep_cols])
  pop <- df2genind(all_dat, ploidy = 1)
  pop <- missingno(pop, type = "mean")
  location_dat <- na.omit(read.csv("data/msat_locations.csv", 
                                   header = T))
  strata <- location_dat[match(indNames(pop), location_dat$StrainID), 
                         ]
  strata$id <- as.factor(paste(strata$cave, strata$Province, 
                               sep = "-"))
  pop@strata <- strata
  pop@pop <- pop@strata$cave
  na.strata <- na.omit(strata[strata$Region == "North America", 
                              ])
  pop <- pop[na.strata$StrainID]
  pop <- poppr::clonecorrect(pop)
  coords <- na.strata[!duplicated(na.strata$cave), c("adj.lon", 
                                                     "adj.lat")]
  genetic.coords <- st_as_sf(coords, coords = c("adj.lon", 
                                                "adj.lat"), crs = 4326, agr = "constant")
  closest.matches <- as.data.frame(st_sf(st_nearest_points(geocache.coords$geometry, 
                                                           genetic.coords$geometry)))
  closest.matches <- separate(closest.matches, 1, c("gc.lon", 
                                                    "gen.lon", "gc.lat", "gen.lat"), sep = ", ")
  closest.matches$gen.lon <- as.numeric(gsub("[c(,)]", "", 
                                             closest.matches$gen.lon))
  closest.matches$gc.lon <- as.numeric(gsub("[c(,)]", "", closest.matches$gc.lon))
  closest.matches$gen.lat <- as.numeric(gsub("[c(,)]", "", 
                                             closest.matches$gen.lat))
  closest.matches$gc.lat <- as.numeric(gsub("[c(,)]", "", closest.matches$gc.lat))
  closest.matches$dist <- sapply(1:nrow(closest.matches), function(i) distm(closest.matches[i, 
                                                                                            c("gen.lon", "gen.lat")], closest.matches[i, c("gc.lon", 
                                                                                                                                           "gc.lat")], fun = distGeo))/1000
  max.visits <- na.omit(all_results_merge) %>% group_by(GC, 
                                                        year) %>% mutate(total = n()) %>% group_by(GC, lat, lon, 
                                                        ) %>% summarise(max = max(total))
  m1 <- merge(closest.matches, coords, by.x = "gen.lat", by.y = "adj.lat")
  m2 <- merge(m1, max.visits, by.x = "gc.lat", by.y = "lat")
  min.closest.match <- m2 %>% group_by(gen.lon, gen.lat) %>% 
    filter(dist == min(dist, na.rm = T))
  write.csv(min.closest.match, "data/closest.matches.msat.csv")
  cleaned.df <- all_results_merge %>% filter(status %in% c("Publish Listing", 
                                                           "Found it", "Didn't find it") & GC %in% min.closest.match$GC)
  s <- split(as.character(cleaned.df$users), as.character(cleaned.df$GC))
  cave.intersects <- NULL
  for (i in unique(names(s))) {
    j <- s[i]
    c <- sapply(s, `%in%`, j)
    d <- sapply(c, sum)
    cave.intersects <- rbind(cave.intersects, cbind(d, names(d), 
                                                    i))
  }
  cave.intersects <- as.data.frame(cave.intersects)
  rownames(cave.intersects) <- NULL
  colnames(cave.intersects) <- c("intersect", "gc1", "gc2")
  cave.intersects$intersect <- as.numeric(cave.intersects$intersect) - 
    1
  cave.intersects.mat <- spread(cave.intersects, key = "gc1", 
                                value = "intersect")
  rownames(cave.intersects.mat) <- cave.intersects.mat$gc2
  cave.intersects.mat <- cave.intersects.mat[, -1]
  write.csv(cave.intersects.mat, file = "data/gc.user.intersect.msat.csv", 
            row.names = T)
  meta <- read.csv("../Pd_MSAT/SraRunTable.csv", header = T)
  vcf <- read.vcfR("../Pd_MSAT/NA.bestsnp.backfill.filtered.vcf")
  genind <- vcfR2genind(vcf)
  strata <- meta[match(indNames(genind), meta$Run), ]
  include_list <- as.character(strata[!is.na(strata$lat), ]$Run)
  genind <- genind[include_list]
  strata <- subset(strata, Run %in% include_list)
  strata <- strata[match(indNames(genind), strata$Run), ]
  genetic.coords <- strata[!duplicated(strata$other_location), 
                           c("lon", "lat")]
  genetic.coords <- st_as_sf(genetic.coords, coords = c("lon", 
                                                        "lat"), crs = 4326, agr = "constant")
  closest.matches <- as.data.frame(st_sf(st_nearest_points(geocache.coords$geometry, 
                                                           genetic.coords$geometry)))
  closest.matches <- separate(closest.matches, 1, c("gc.lon", 
                                                    "gen.lon", "gc.lat", "gen.lat"), sep = ", ")
  closest.matches$gen.lon <- as.numeric(gsub("[c(,)]", "", 
                                             closest.matches$gen.lon))
  closest.matches$gc.lon <- as.numeric(gsub("[c(,)]", "", closest.matches$gc.lon))
  closest.matches$gen.lat <- as.numeric(gsub("[c(,)]", "", 
                                             closest.matches$gen.lat))
  closest.matches$gc.lat <- as.numeric(gsub("[c(,)]", "", closest.matches$gc.lat))
  closest.matches$dist <- sapply(1:nrow(closest.matches), function(i) distm(closest.matches[i, 
                                                                                            c("gen.lon", "gen.lat")], closest.matches[i, c("gc.lon", 
                                                                                                                                           "gc.lat")], fun = distGeo))/1000
  max.visits <- na.omit(all_results_merge) %>% group_by(GC, 
                                                        year) %>% mutate(total = n()) %>% group_by(GC, lat, lon, 
                                                        ) %>% summarise(max = max(total))
  m1 <- merge(closest.matches, coords, by.x = "gen.lat", by.y = "lat")
  m2 <- merge(m1, max.visits, by.x = "gc.lat", by.y = "lat")
  min.closest.match <- m2 %>% group_by(gen.lon, gen.lat) %>% 
    filter(dist == min(dist, na.rm = T))
  write.csv(min.closest.match, "data/closest.matches.snp.csv")
  list(time = Sys.time(), tempfile = tempfile())
}