#### create a spatial weight matrix
# crude method, adjacency measured using centroids of counties


# load in spatial polygons df for US and Canada
source("county-fix.R")

# xy to use for CONNECTION NETWORK
united.xy<-st_transform(united.poly, 29101) %>% 
    # grab the centroids of each 
    st_centroid() %>% 
    # this is the crs from d, which has no EPSG code:
    st_transform(., '+proj=longlat +datum=WGS84')

# use the polygon coordinates in `$geometry`
united.w <- poly2nb(as_Spatial(united.poly$geometry), row.names=united.poly$id,)

# style = C ; globally standardised (sums over all links to n)
united.listw <- nb2listw(united.w, style = "C")

# adacency matrix
# style = B ; basic binary format
united.m <- nb2mat(united.w, style='B',zero.policy = T)

# if you want to visualize it
# plot(united.poly, col='gray', border='blue', lwd=2)
# plot(united.w, united.xy, col='red', lwd=2, add=TRUE)