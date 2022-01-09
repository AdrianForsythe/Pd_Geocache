# summary of GC finds - GC
# polygons are included here in order to grab the centroid later

incidence<-function(presence.df,output){
  require(tidyverse)
  require(sf)
  
  # for testing
  # presence.df<-"workflow/data/presence.df.rds"
  # output<-"data/full-county-year-incidence.csv"
    
  # pull out a unique list of county polys
  uniq.df<-readRDS(presence.df) %>% distinct(county,.keep_all = T)
  
  full.yc<-expand.grid(uniq.df[,c("year","county")])
  full.yc$yc<-paste0(full.yc$year,"-",full.yc$county)
  uniq.df$yc<-paste0(uniq.df$year,"-",uniq.df$county)
  
  full.yc$incidence<-ifelse(full.yc$yc %in% uniq.df$yc,1,0)
  
  write.csv(x = full.yc,file = output)
}

incidence(snakemake@input[[1]],snakemake@output[[1]])
