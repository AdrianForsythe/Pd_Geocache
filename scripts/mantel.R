library(ade4)

# first run the output from script to match shape files to GC sites
source("scripts/coordinate-overlap.R")

# all combination of counties and years
x<-expand.grid(unique(all_merge$year),
               unique(all_merge$county))

# matix of total number of users in counties at years
visits<-NULL
for (i in 1:nrow(x)) {
  r<-all_merge[all_merge$year %in% x[i,]$Var1 & all_merge$county %in% x[i,]$Var2,]
  visits<-rbind(visits,cbind.data.frame(x[i,],nrow(r)))
}
# clean results
visits<-na.omit(visits)
colnames(visits) <- c("year","county","n")

# add colum and row names in pairwise fasion
id<-paste(visits$year,visits$county,sep=".")
m<-as.matrix(dist(visits$n))
colnames(m) <- id
rownames(m) <- id

# matrix of yearly incidence at county level 
y<-expand.grid(unique(all_merge$wns.map.yr),unique(all_merge$county))
y$Var1<-(gsub("-.+","",y$Var1))

incidence<-NULL
for (i in 1:nrow(y)) {
  r<-all_merge[all_merge$year %in% y[i,]$Var1 & all_merge$county %in% y[i,]$Var2,]
  r2<-nrow(r[!is.na(r$wns.map.yr),])
  incidence<-rbind(incidence,cbind.data.frame(y[i,],r2))
}

incidence<-na.omit(incidence)
colnames(incidence) <- c("wns.map.yr","county","n")
id<-paste(incidence$wns.map.yr,incidence$county,sep=".")
n<-as.matrix(dist(incidence$n))
colnames(n) <- id
rownames(n) <- id

m2<-m[rownames(m) %in% rownames(n),colnames(m) %in% colnames(n)]

# mantel
mantel.rtest(as.dist(n),as.dist(m2),nrepet = 999)
