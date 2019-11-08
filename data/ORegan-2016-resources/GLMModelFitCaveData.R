##cave dataset
nypa_data<-read.csv("NYPA_caves_infections.csv",header=TRUE)

##cave dataset from which cumulative number of cases is calculated:
nypa_data[which(apply(nypa_data[,2:7]>0,1,sum)>1),]

#cumulative number of cases in previous year (predictor)
Y<-c(1,2,5,5,1,2,3,1,1,1,5,1,4,6,13,13,1,1,1,1,10,12,1,1,2);
#number of new cases (response)
incidence<-c(2,3,0,0,2,1,1,1,2,5,1,4,2,7,0,1,1,0,2,10,2,0,1,1,0);

#fit glm model to data
model<-glm(Z ~ Y - 1 ,data=data,family=poisson(link="identity")) #p-value <0.001
#intrinsic growth rate of infection
lambda=as.numeric(model$coefficients)
lwr.lambda<-confint(model)[1]
upr.lambda<-confint(model)[2]

gamma=1/3; #infectious period
R0=(lambda/gamma)+1 #basic reproduction number


x11(height=4, width=4)
data<-data.frame(Y=Y,Z=incidence);
plot(data$Y,data$Z,xlab="cumulative number of infected caves",ylab="number of new caves", pch=19, lwd=2, col=colors()[89], bty='l', las=1)
#T=seq(0,length(Y),by=.1)
#lines(T,1+lambda*T, lwd=2,col='brown' )
abline(a=1, b=lambda, col='brown',lwd=2)
title(substitute(paste("Exponential growth rate", ~lambda," = ",lambdaval), list(lambdaval=round(lambda,2))))

#now look at the outbreak data 
x11(height=4, width=4)
infectious<-c(2,5,5,5,2,3,4,1,3,5,6,4,6,13,13,14,1,1,3,10,12,12,1,2,2);
t<-c(1,2,3,4,1,2,3,1,2,1,2,1,2,3,4,5,1,2,3,1,2,3,1,2,3);
cumulativedata<-data.frame(Y=infectious,t=t);
plot(cumulativedata$t,cumulativedata$Y,xlab="Time since initial infection",ylab="Cumulative number of infected caves",pch=19, lwd=2, col=colors()[89], bty='l', las=1)
years=seq(1,5,by=.1)
lines(years,exp(lambda*years), lwd=2, col='brown')
#title(substitute(paste("Basic reproduction number ", R0," = ",R0val), list(R0val=R0)) )
