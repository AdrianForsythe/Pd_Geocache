filter.description<-function(data_path,out_path,keyword_plot){
  #####
  require(tidyverse)
  require(UpSetR)
  #####
  keywords <- c("flashlight","cave","mine","bat","ore","mineral")

  gc_filtered_dat = na.omit(read.csv(data_path,header=T,strip.white = T))

  full_set<-NULL

  for (i in as.character(gc_filtered_dat$GC)) {
    column_matches<-NULL
    for (j in keywords) {
      s<-filter(gc_filtered_dat,GC==i)
      if(length(grep(paste(" ",j,sep=""),s$Description,ignore.case = TRUE,)>0)){
        matches<-grep(paste(" ",j,sep=""),s$Description,ignore.case = TRUE)
      } else {
        matches <- 0
      }
      column_matches<-cbind(column_matches,matches)
    }
    full_set<-rbind(full_set,cbind(column_matches,i))
    colnames(full_set) <- c(keywords,"GC")
  }

  long_full_set <- full_set %>% as.data.frame(full_set) %>%
    pivot_longer(cols = -GC,names_to = "keyword") %>%
    mutate(value=as.integer(levels(value))[value],
           presence = ifelse(value > 0 ,1,0))

  wide_full_set<-long_full_set %>%
    pivot_wider(names_from = keyword,values_from = presence,id_cols = GC,values_fn = list(presence = min)) %>%
    # mutate_all(as.integer()) %>%
    as.data.frame()

  png(keyword_plot,res = 300,height = 800,width = 1000,units = "px")
  wide_full_set %>% as.data.frame() %>%  UpSetR::upset(.,nintersects = NA,nsets = length(keywords),order.by = "freq")
  dev.off()

  ##### Filtering
  filtered_set<-subset(wide_full_set,  wide_full_set$flashlight > 0 & wide_full_set$cave > 0 |
                         wide_full_set$ore > 0 & wide_full_set$mine > 0 |
                         wide_full_set$mineral > 0 & wide_full_set$mine > 0)

  filtered_dat<-na.omit(gc_filtered_dat[match(filtered_set$GC,gc_filtered_dat$GC),])

  write.csv(filtered_dat,out_path)
}

filter.description(snakemake@input[[1]],snakemake@output[[1]],snakemake@output[[2]])
