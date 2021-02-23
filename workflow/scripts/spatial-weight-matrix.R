#### create a spatial weight matrix
# crude method, adjacency measured using centroids of counties
require(tidyverse)
spatial_weight_matrix<-function(relevant.records,presence.df,weights.long){

  presence_df<-readRDS(presence.df)

  site_visits <- read.csv(relevant.records) %>%
    filter(Type %in% c("Type.found_it", "Type.didnt_find_it", "Type.owner_maintenance", "Type.publish_listing")) %>%
    group_by(county,Year,coords.x1,coords.x2) %>%
    summarise(total = length(User))

  site_visits.p <- as_Spatial(st_as_sf(site_visits,
                                       coords = c("coords.x1","coords.x2"),
                                       crs = 4326, agr = "constant"))

    # at the spatial locations of object x retrieves the indexes or attributes from spatial object y
    # index for sites that match up with polys
    # these are the county polygons that match up with sites
    num<-sp::over(site_visits.p, as_Spatial(presence_df$geoms),fn=NULL)

    # pull out a unique list of county polys
    uniq.df<-presence_df[na.omit(unique(num)),]

    # convert coords from county poly to centroid points for each county. Then reproject.
    united.xy <- uniq.df$geoms %>% st_centroid() %>%
      st_transform(., "+proj=longlat +datum=WGS84")

    # construct neighbour list from county centroids
    county.n <- poly2nb(as_Spatial(presence_df$geoms))

    # spatial weights of neighbourlist
    county.w <- nb2listw(county.n,zero.policy = TRUE)

    county.w$neighbours

    # add names so that we can match these with the df later
    names(county.w$weights) <- presence_df$rownumber

    # replace NULL with 0
    county.w$weights[sapply(county.w$weights, is.null)] <- 0
    weights_long<-unlist(county.w$weights,use.names = T)
    saveRDS(weights_long,weights.long)
    # spatial weight matricies from neighbour list
    # county.m <- nb2mat(county.n, style = "B", zero.policy = T)
  }
  spatial_weight_matrix(snakemake@input[[1]],snakemake@input[[2]],snakemake@output[[1]])
