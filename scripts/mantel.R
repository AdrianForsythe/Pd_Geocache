source("coordinate-overlap.R")

  # all combination of counties and years
x<-expand.grid(unique(all_merge$year),unique(all_merge$county))

# matix of total number of users in counties at years

result<-NULL
for (i in 1:nrow(x)) {
  r<-all_merge[all_merge$year %in% x[i,]$Var1 & all_merge$county %in% x[i,]$Var2,]
  result<-rbind(result,cbind.data.frame(x[i,],nrow(r)))
}

result<-na.omit(result)
colnames(result) <- c("year","county","n")
id<-paste(result$year,result$county,sep=".")
m<-as.matrix(dist(result$n))
colnames(m) <- id
rownames(m) <- id

y<-expand.grid(unique(all_merge$wns.map.yr),unique(all_merge$county))
y$Var1<-(gsub("-.+","",y$Var1))

result<-NULL
for (i in 1:nrow(y)) {
  r<-all_merge[all_merge$year %in% y[i,]$Var1 & all_merge$county %in% y[i,]$Var2,]
  r2<-nrow(r[!is.na(r$wns.map.yr),])
  result<-rbind(result,cbind.data.frame(y[i,],r2))
}

result<-na.omit(result)
colnames(result) <- c("wns.map.yr","county","n")
id<-paste(result$wns.map.yr,result$county,sep=".")
n<-as.matrix(dist(result$n))
colnames(n) <- id
rownames(n) <- id

m2<-m[rownames(m) %in% rownames(n),colnames(m) %in% colnames(n)]

# mantel
library(ade4)
mantel.rtest(as.dist(n),as.dist(m2),nrepet = 999)
