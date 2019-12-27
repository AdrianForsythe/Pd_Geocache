library(dplyr)
library(lubridate)
library(ggplot2)
library(rgdal)
library(spdep)
library(leaflet)
library(CARBayesST)

source("coordinate-overlap.R")
source("spatial-weight-matrix.R")

united.poly$id.p<-tolower(united.poly$id)

# only need a few columns here
geocache.presence.df<-geocache.presence.df[,c("state.prov","county","year","total","count","wns.map.yr")]
geocache.presence.df$id.gc <- tolower(paste(geocache.presence.df$county,geocache.presence.df$state.prov,sep=", "))

# y<-geocache.presence.df[geocache.presence.df$id %in% united.poly$id,]$wns.map.y

ult.merge<-merge(geocache.presence.df,united.poly,by.x="id.gc",by.y="id.p")

# mean values
geocache.av <- ult.merge %>% group_by(county) %>% mutate(geocache.total = sum(total))

# make map
colours <- colorNumeric(palette = "BuPu", domain = geocache.av$geocache.total)
map1 <- leaflet(data=geocache.av) %>% addTiles() %>% 
              addPolygons(fillColor = ~colours(geocache.total), color="red", weight=1, fillOpacity = 0.7) %>% 
              addLegend(pal = colours, values = geocache.total, opacity = 1,
                        title="Total geocache visits \nper county",position="bottomright") %>% 
              addScaleBar(position="bottomleft")
map1

# list of rows
with.neighbours<-rownames(united.m[rowSums(united.m)>0,rowSums(united.m)>0])

# remove ones with no neighbours
united.w2 <- poly2nb(as_Spatial(united.poly[united.poly$id %in% with.neighbours,]$geometry), row.names=united.poly[united.poly$id %in% with.neighbours,]$id)
united.listw2 <- nb2listw(united.w2, style = "B",zero.policy = T)
united.m2 <- nb2mat(united.w2, style='B',zero.policy = T)

ult.merge2 <- ult.merge[ult.merge$id %in% with.neighbours,]
geocache.presence.wn2 <-geocache.presence.wn[geocache.presence.wn$county %in% poly.df.wn2$COUNTYNAME,]

# mean again
geocache.av2 <- geocache.presence.wn2 %>% group_by(county) %>% summarise(geocache.total = sum(total))
poly.df.wn2@data$COUNTYNAME <- gsub(" County","",poly.df.wn2@data$COUNTYNAME)
poly.df.wn2@data$gc.total <- geocache.av[geocache.av$county %in% poly.df.wn2@data$COUNTYNAME,]$geocache.total

W.nb2<- poly2nb(poly.df.wn2, row.names = 1:nrow(geocache.av2))
W.list2 <- nb2listw(W.nb2, style = "B",zero.policy = T)
W2 <- nb2mat(W.nb2, style = "B",zero.policy = T)

# construct model
formula <- total ~ offset(log(count))
model1 <- glm(formula = formula,family = "poisson",data = ult.merge)
resid.glm <- residuals(model1)
summary(model1)

moran.mc(x = resid.glm, listw = united.listw, nsim = 10000)

formula2 <- total ~ offset(log(count))
model2<-ST.CARsepspatial(formula = formula, family = "poisson",data = ult.merge, W = united.m2[rownames(united.m2) %in% ult.merge$id, rownames(united.m2) %in% ult.merge$id],
                         burnin = 20000, n.sample = 220000,thin=10)
