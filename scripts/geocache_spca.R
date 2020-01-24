

x<-read.table("cave-mines-not-complete-results.tab",sep="\t",fill=T,header = T)
m_gc<-read.csv("cave-mines-not-complete.csv",header=T)

cache_merge <- merge(x,m_gc,by.x = "i",by.y = "url")
cache_merge$lat <- as.numeric(gsub("° ",".",gsub("[.]","",gsub(pattern = "N ",replacement = "",cache_merge$lat))))
cache_merge$lon <- as.numeric(gsub("° ",".",gsub("[.]","",gsub(pattern = "W ",replacement = "-",cache_merge$lon))))

cache_summ<-cache_merge %>% group_by(i,lat,lon) %>% summarise(weight = n())

pop.graph <- chooseCN(cbind(cache_summ$lon,cache_summ$lat),type=7,plot=FALSE,ask = F,a=1,dmin=1, res="listw")
plot(pop.graph, cbind(cache_summ$lat,cache_summ$lon),col="gray")

