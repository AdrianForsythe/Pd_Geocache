
# construct neighbour list from county centroids
county.n <- poly2nb(uniq.poly, row.names = names(uniq.poly))

# spatial weights of neighbourlist
county.w <- nb2listw(county.n,style = "C",zero.policy = TRUE)
lapply(county.w$weights, function(x) ifelse(x == "NULL",0,x))

library(reshape)
df <- data.frame(matrix(unlist(county.w$weights), nrow=length(county.w$weights), byrow=T))
lapply(county.w$weights,function(x) ifelse(!is.numeric(x),0,melt(x)))
melt<-melt.list(county.w$weights[1:65])

# spatial weight matricies from neighbour list
# county.m <- nb2mat(county.n, style = "B", zero.policy = T)

updated<-presence.df[na.omit(unique(num)),]
updated$WNS_MAP_YR <- ymd(gsub("-.+", "/01/01", updated$WNS_MAP_YR))
updated$YR_SUSPECT <- ymd(gsub("-.+", "/01/01", updated$YR_SUSPECT))
updated$YR_CONFIRM <- ymd(gsub("-.+", "/01/01", updated$YR_CONFIRM))
updated$incidence <- ifelse(is.na(updated$WNS_MAP_YR),0,updated$WNS_MAP_YR)

updated$activity<-as.data.frame(z[match(updated$COUNTYNAME,z$county),"total"])$total

####

lag = lagsarlm(incidence ~ activity + COUNTYNAME, data=updated, listw = county.w,tol.solve=1.0e-30, zero.policy=TRUE)

summary(lag)
