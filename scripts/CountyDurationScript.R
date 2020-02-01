#County durant script

#Script that calculates the duration of the epidemic in each county in order
#to find number of susceptible caves per county per year using SSII
#Script creates 2 csv outputs us_data_dur.csv and us_data_dur_S_sequence.csv
#Both csv files must be sorted from lowest to highest FIPS number for simulations to work.


#Now we are altering this script to include the human interaction from Smith et al

county_duration<-function(...){
  
SSIIWNS<- function(initVars, gamma, R0, populationSize, maxEvents, startTime=0, endTime) {
  #Purpose: Perform numerical integration of SSII system of ODEs using lsoda() ODE solver
  #          to simulate a deterministic SSII model run
  # Returns: a data frame with the following variables:
  #          - time: times at which events occurred
  #          - SB: time series of the number/proportion of individual BATS SUSCEPTIBLES
  #          - SH: time series of the number/proportion of individuals HUMANS SUSCEPTIBLES
  #          - IB: time series of the number/proportion of individuals BATS INFECTED
  #          - IH: time series of the number/proportion of individuals HUMANS INFECTED
  #          
  # Parameters:
  #          -initVars: column vector of the initial integers of the population in each class
  #                     [Example (integer-valued): c(S = 1000, I = 2, R = 0) 
  #          -R0: basic reproductive rate, which will be used 
  #                  to calculate beta 
  #          - gamma: reciprocal of infectious period
  #          -maxEvents: number of ODE integration intervals 
  #          -startTime: time point at which to begin integrating [default: 0]
  #          -endTime: time point at which integration will end [default: 100]
  
  # Package
  require (deSolve)
  #similar to the other script that has gamma dist duration
  timeSteps  <- seq(startTime, endTime, length = maxEvents)
  intervalWidth <- round(((endTime - startTime) / maxEvents),4) # Round to 4 decimal places
  betaB <-gamma*R0/populationSize #density dependent transmission; beta is a function of the initially 
  betaH <-gamma*R0/populationSize #Needs a betaH estimates bc this wont fly
  #fully susceptible population size
  parameters  <- c(gamma,betaB,betaH,theta)
  
  #Obviously this model is significantly smaller than the other ones and only has 1 infected state
  
  SSIIRModel <- function(t, x, parms) {
    with(as.list(c(parms, x)), {
      dSB <- (-betaB*SB*IB) 		#Bat susceptibles
      dSH <- (-betaH*SH*IH)        #Human susceptibles
      dIH <- betaH*SH*IH -(gamma)*IB	#infected           	  		
      dIB <- gamma*IB + betaB*SB*IB - theta*IB
      dRB <- theta*IB
      res <- c(dSB, dSH, dIH, dIB, dRB)
      list(res)
    })
  }
  
  out <- as.data.frame(lsoda(initVars, timeSteps, SSIIRModel, parameters))
  
  
  
} 

caves<-read.csv("examples/ORegan-2016-resources/us_data.csv", header=TRUE) #WNS county dataset

#Now to calculate the duration of the epidemic in each county

unique_caves<-data.frame(unique_cave_numbers,unique_duration);

new_caves<-merge(caves,unique_caves,by.x="caves",by.y="unique_cave_numbers",all.x=TRUE)
write.csv(file="data/us_data_dur",new_caves) 
# csv file must be sorted by FIPS number to generate simulations correctly

#script to calculate number of susceptible caves per county per year over course of epidemic 
#using SSII model parameterized by cave data 

#these are the simulations

unique_S_sequence<-unlist(lapply(unique_cave_numbers,function (x) {
  results<-SSIIRWNS(c(SH=x-1, SB=x-1, IB=1, IH=1, RB=0), 1/3, 2.56, x, 100*10^2, startTime=0, 100);
  results$S[seq(0,10000,100)+1]
}))
unique_S_sequence<-matrix(t(unique_S_sequence),ncol=length(unique_cave_numbers),nrow=101);
unique_S_sequence<-floor(unique_S_sequence[-101,]);


unique_caves<-data.frame(unique_cave_numbers,unique_duration,t(unique_S_sequence));

new_caves<-merge(caves,unique_caves,by.x="caves",by.y="unique_cave_numbers",all.x=TRUE)
write.csv(file="data/us_data_dur_S_sequence.csv",new_caves) 
# csv file must be sorted by FIPS number to generate simulations correctly
}
  
  
