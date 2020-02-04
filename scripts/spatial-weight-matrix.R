#### create a spatial weight matrix
# crude method, adjacency measured using centroids of counties

make_adacency<-function (...) {
  united.xy <- st_transform(united.poly, 29101) %>% st_centroid() %>% 
    st_transform(., "+proj=longlat +datum=WGS84")
  united.w <- poly2nb(as_Spatial(united.poly$geometry), row.names = united.poly$id)
  united.listw <- nb2listw(united.w, style = "C")
  united.m <- nb2mat(united.w, style = "B", zero.policy = T)
  list(time = Sys.time(), tempfile = tempfile())

  list(time = Sys.time(), tempfile = tempfile())
  
}