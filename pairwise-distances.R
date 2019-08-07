library(geosphere)
library(dplyr)

# read in geocache list
m_gc<-read.csv("cave-mines-not-complete.csv",header=T)

# fix coords
# on linux, encoding changes to "\xb0"
m_gc$lat <- as.numeric(gsub("\xb0 ",".",gsub("[.]","",gsub(pattern = "N ",replacement = "",m_gc$lat))))
m_gc$lon <- as.numeric(gsub("\xb0 ",".",gsub("[.]","",gsub(pattern = "W ",replacement = "-",m_gc$lon))))

pairwise.dist<-distm(m_gc[,c("lon","lat")],m_gc[,c("lon","lat")],fun=distHaversine)

write.csv(x=pairwise.dist,file="pairwise.dist.csv",col.names=T,row.names=T)
