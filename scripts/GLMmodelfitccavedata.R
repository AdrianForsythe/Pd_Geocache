##cave dataset
glm_model<-function(...){
  
#fit glm model to data
model<-glm(count ~ wns.map.yr - 1 , data= relevant.records, family=poisson(link="identity")) 
#Is p value worth keeping in here?

#intrinsic growth rate of infection
lambda=as.numeric(model$coefficients)
lwr.lambda<-confint(model)[1]
upr.lambda<-confint(model)[2]

gamma=1/3; #infectious period
R0=(lambda/gamma)+1 #basic reproduction number

data<-data.frame(Y=relevant.records$count,Z=relevant.records$wns.map.yr);
png("figures/exp-growth-rate.png")
plot(data$Y,data$Z,xlab="cumulative number of infected caves",ylab="number of new caves", pch=19, lwd=2, col=colors()[89], bty='l', las=1)
abline(a=1, b=lambda, col='brown',lwd=2)
title(substitute(paste("Exponential growth rate", ~lambda," = ",lambdaval), list(lambdaval=round(lambda,2))))
dev.off()
#Stll missing some parts
#
}