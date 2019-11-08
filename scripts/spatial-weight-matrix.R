library(raster)
library(spdep)

source("county-fix.R")

# xy to use for CONNECTION NETWORK
united.xy<-st_transform(united.poly, 29101) %>% 
    st_centroid() %>% 
    # this is the crs from d, which has no EPSG code:
    st_transform(., '+proj=longlat +datum=WGS84')

united.w <- poly2nb(as_Spatial(united.poly$geometry), row.names=united.poly$id,)
united.listw <- nb2listw(united.w, style = "C")
united.m <- nb2mat(united.w, style='B',zero.policy = T)

# plot(united.poly, col='gray', border='blue', lwd=2)
# plot(united.w, united.xy, col='red', lwd=2, add=TRUE)

