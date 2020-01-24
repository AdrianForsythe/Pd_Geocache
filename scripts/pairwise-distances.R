library(geosphere)
library(dplyr)

# read in geocache list
m_gc<-read.csv("cave-mines-not-complete.csv",header=T)

# fix coords
# on linux, encoding changes to "\xb0"
m_gc$lat <- as.numeric(gsub("\xb0 ",".",gsub("[.]","",gsub(pattern = "N ",replacement = "",m_gc$lat))))
m_gc$lon <- as.numeric(gsub("\xb0 ",".",gsub("[.]","",gsub(pattern = "W ",replacement = "-",m_gc$lon))))

# creat the matrix
pairwise.dist<-distm(m_gc[,c("lon","lat")],m_gc[,c("lon","lat")],fun=distHaversine)
colnames(pairwise.dist)<-m_gc$url
rownames(pairwise.dist)<-m_gc$url

# write.csv(x=pairwise.dist,file="pairwise.dist.csv",col.names=T,row.names=T)

# how to lookup the combinations for each user?
all_results<-read.table("cave-mines-not-complete-results.tab",header=T,fill = T,sep = "\t",na.strings = "",quote = "",comment.char = "")

#clean things up first
clean_dat<-all_results[!duplicated(c(all_results$users,all_results$i)),c("users","i")]
clean_dat$i<-gsub(pattern = "_(.*)","",clean_dat$i)
clean_dat$i<-gsub(pattern = "https://www.geocaching.com/geocache/","",clean_dat$i)

# spread data frame so that each visit to a unique site gets its own column
sites<-unique(clean_dat$i)
combs<-expand.grid(sites,sites)
users<-unique(clean_dat$users)

result<-NULL
full_results<-NULL
for (j in 1:nrow(combs)) {
  # print(combs[j,])
  v1<-combs[j,1]
  v2<-combs[j,2]
  result<-clean_dat %>% group_by(users) %>% filter(i == v1 | i == v2) %>% filter(n() > 1) %>% count()
  # %>% summarise_at("count")
  full_results<-rbind(full_results,cbind(result,combs[j,]))
}