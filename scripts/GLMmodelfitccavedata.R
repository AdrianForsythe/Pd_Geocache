##cave dataset
glm_model<-function(...){
  
# fit glm model to data

# model<-glm(count ~ wns.map.yr - 1 , data= relevant.records, family=poisson(link="identity")) 
model <- glm(incidence ~ gc.weight + COUNTYNAME + WNS_MAP_YR ,data = updated.weights, family = binomial(link = "cloglog"))
#Is p value worth keeping in here?

#intrinsic growth rate of infection
lambda=as.numeric(model$coefficients)
lwr.lambda<-confint(model)[1]
upr.lambda<-confint(model)[2]

gamma=1/3; #infectious period
R0=(lambda/gamma)+1 #basic reproduction number

# taking the total number of sites infected per year and the cumulative sum
rate_data<-relevant.records %>% arrange(wns.map.yr) %>% group_by(wns.map.yr) %>%
  summarise(cave.count = length(unique(GC))) %>% mutate(inf.caves = cumsum(cave.count))

png("figures/exp-growth-rate.png")
plot(rate_data$inf.caves,rate_data$cave.count,
     xlab="cumulative number of infected caves",
     ylab="number of new caves",
     pch=19, lwd=2, col=colors()[89], bty='l', las=1)
abline(a=1, b=lambda, col='brown',lwd=2)
title(substitute(paste("Exponential growth rate", ~lambda," = ",lambdaval), list(lambdaval=round(lambda,2))))
dev.off()
#Stll missing some parts
#
}