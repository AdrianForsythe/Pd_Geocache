#Load Dependencies
source('WNS-GT-Duration.R')
source('GLMModelFitCaveData.R')

#Read data: US County data (uc)
uc=read.csv('us_data_dur.csv', header=TRUE)
#Subset county data to only Counties with Caves (cc)
cc=uc[which(uc$caves>0),]
#Get coordinates of county centroids
c=rbind(cc$x,cc$y)/1000	#divide by 1000 to convert to km
#Calculate distance matrix
dist=makedist(c)

#Find MLE Beta values
beta=getbeta(dist,cc$caves,cc$tau,cc$WNS,cc$dur,start=c(10.30425552, -0.03080282, 0.03506665, 0.26193472))
#Calculate simultaneous confidence intervals
delta=getdelta(beta$par,dist,cc$caves,cc$tau,cc$WNS,cc$dur)

#Create a matrix with all rows equal to MLE Beta vlaues
b=matrix(beta$par,ncol=4,nrow=10000,byrow=TRUE)

#Simulate foreward from Cave Zero with a sir cave model
dur_sir_sims1_0=makesims(cc,dist,b,nsims=10000,seed=521)
#Simulate foreward from year 5 (all known infection data) with a sir cave model
dur_sir_sims1_5=makesims(cc,dist,b,first=5,nsims=10000,seed=521)

#change durations to set up a si cave model
load('si_dur.Rdata')
cc$dur <- si_dur[which(uc$caves>0),]

#Simulate foreward from Cave Zero with a si cave model
dur_si_sims1_0=makesims(cc,dist,b,nsims=10000,seed=521)
#Simulate foreward from year 5 (all known infection data) with a si cave model
dur_si_sims1_5=makesims(cc,dist,b,first=5,nsims=10000,seed=521)
