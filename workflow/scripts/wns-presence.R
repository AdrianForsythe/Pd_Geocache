##### WNS Presence shape file
# devtools::install_github("yonghah/esri2sf")

require(tidyverse)
require(lubridate)
require(esri2sf)
require(sf)

presence_shape<-function(presence_df,presence_poly){
  
  # generating the RDS
  url2 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/2"
  df2 <- esri2sf(url2)
  url5 <- "https://www.sciencebase.gov/arcgis/rest/services/Catalog/59d45504e4b05fe04cc3d3e0/MapServer/5"
  df5 <- as.data.frame(esri2sf(url5))
  presence.df <- bind_rows(df2, df5) %>% 
    mutate(county = paste0(trimws(gsub(pattern = "County.*$",replacement = "",COUNTYNAME)),"-",STATEPROV),
           date=dmy(paste0("01-01",gsub(pattern = "-.*$",replacement = "",WNS_MAP_YR))),
           year=year(date))
  presence.poly <- as_Spatial(presence.df$geoms)
  saveRDS(presence.df, file = presence_df)
  saveRDS(presence.poly, file = presence_poly)
}

presence_shape(snakemake@output[[1]],snakemake@output[[2]])
