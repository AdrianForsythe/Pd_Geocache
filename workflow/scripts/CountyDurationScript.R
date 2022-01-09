#County durant script

# Script that calculates the duration of the epidemic in each county in order
# to find number of susceptible sites per county per year using SSII
# Script creates 2 csv outputs us_data_dur.csv and us_data_dur_S_sequence.csv
# Both csv files must be sorted from lowest to highest FIPS number for simulations to work.

#Now we are altering this script to include the human interaction from Smith et al

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

county_duration<-function(...){
  require(tidyverse)
  

SIRWNS<- function(initVars, gamma, R0, populationSize, maxEvents, startTime=0, endTime) {
  
  # load Package
  require (deSolve)

  # for testing
  both_weights<-read.csv("workflow/data/gc.weights.csv",header=T) %>% filter(FID %in% unique_site_numbers$FID[1])
    
  # similar to the other script that has gamma dist duration...
  timeSteps  <- seq(startTime, endTime, length = maxEvents)
  intervalWidth <- round(((endTime - startTime) / maxEvents),4) # Round to 4 decimal places
  beta_glm <- glm(incidence ~ touching , data = both_weights , family = binomial(link = "cloglog")) # density dependent transmission; beta is a function of the initially 
  
  # is this what we want to do?
  beta_glm$fitted.values
  
  # Needs a beta estimates to determine a more accurate model
  # fully susceptible population size
  parameters  <- c(gamma,beta)
  
  #Obviously this model is significantly smaller than the other ones and only has 1 infected state

  SIRModel <- function(t, x, parms) {
    with(as.list(c(parms, x)), {
      dS <- (-beta*S*I) 		#Bat susceptibles
           #Human susceptibles
      dI <- beta*S*I -(gamma)*I	#infected           	  		
      dR <- gamma*I
      res <- c(dS,dI,dR)
      list(res)
    })
  }
  
  out <- as.data.frame(lsoda(initVars, timeSteps, SIRModel, parameters))
  
} 

# number of sites in county
sites<-read.csv("workflow/data/relevant-records.csv") %>% distinct(GC,county,FIPS)

counties<-readRDS("workflow/data/presence.df.rds") %>% select(-geoms) %>% mutate(FIPS=as.integer(FIPS))

unique_site_numbers<-sites %>% left_join(counties) %>% group_by(FID,FIPS) %>% summarise(n=n_distinct(GC))

unique_duration<-unlist(lapply(unique_site_numbers,function (x) {
  
  results<-SIRWNS(c(S=unique_site_numbers[1,3]-1, I=1, R=0), 1/3, 2.56, unique_site_numbers[1,3], 100*10^2, startTime=0, 100)
  ceiling(results[which(results$I<1)[1],1])
  
}))  

# Now to calculate the duration of the epidemic in each county
unique_sites<-data.frame(unique_site_numbers,unique_duration);

# new_sites<-merge(sites,unique_sites,by.x="sites",by.y="unique_site_numbers",all.x=TRUE)
# write.csv(file="data/us_data_dur",new_sites) 
# csv file must be sorted by FIPS number to generate simulations correctly

new_sites<-read.csv("examples/ORegan-2016-resources/us_data_dur.csv",header=T) %>% arrange(FIPS)

# script to calculate number of susceptible sites per county per year over course of epidemic 
# using SSII model parameterized by site data 
# these are the simulations

unique_S_sequence<-unlist(lapply(unique_site_numbers,function (x) {
  results<-SIRWNS(c(S=x-1, I=1, R=0), 1/3, 2.56, x, 100*10^2, startTime=0, 100);
  results$S[seq(0,10000,100)+1]
}))
unique_S_sequence<-matrix(t(unique_S_sequence),ncol=length(unique_site_numbers),nrow=101);
unique_S_sequence<-floor(unique_S_sequence[-101,]);


unique_sites<-data.frame(unique_site_numbers,unique_duration,t(unique_S_sequence));

new_sites<-merge(sites,unique_sites,by.x="sites",by.y="unique_site_numbers",all.x=TRUE)
write.csv(file="data/us_data_dur_S_sequence.csv",new_sites) 
# csv file must be sorted by FIPS number to generate simulations correctly
}
  
  
