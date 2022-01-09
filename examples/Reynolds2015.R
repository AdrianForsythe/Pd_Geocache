## SapronoseDF a function written by Tom Ingersoll on December 26, 2012
## Modification by Hannah Reynolds on September 25, 2012 
## Simulates an invasion of a cave and bat population by Pseudogymnoascus destructans


## DON'T DO THIS.  Read Jenny Bryan's discussion of why this is a 'code smell'
rm(list=ls(all=TRUE)) 

library(ggplot2)


library(extrafont)
loadfonts()

## The function
SapronoseDF<-function(	
  R, # bat birth/migration rate 
  r, # bat recovery rate
  K, #bat carrying capacity
  KZ, # fungal carrying capacity
  Betaxb, # bat-bat infection rate
  Mub, # background bat mortality
  taum, # delay in mortality
  Rhob, # bat WNS mortality rate
  Betabr, # bat to environment colonization (spore shed rate)
  Betaxr, # fungal growth rate
  Muz, # fungal death rate
  h, # length of hibernation season
  t, # time in years
  X0, #starting susceptible Bat
  Y0, # starting infected bats
  Z0,	# P. destructans spores 
  V,	# cave volume
  theta,	# infectious dose
  tau # incubation period (days)
)	
{
  X<-matrix(NA,t*365+1,1) 		# uninfected bats
  Y<-matrix(NA,t*365+1,1)			# infected bats
  Z<-matrix(NA,t*365+1,1)		    # P. destructans CFUs
  Time<-matrix(NA,t*365+1,1) 		# Days
  
  X[1]<-X0
  Y[1]<-Y0
  Z[1]<-Z0
  Time[1]<-0
  
  for(T in 1:(t*365)){
    
    # the day of the year
    day = T-365*as.integer(T/365) 
    
    # the female bats give birth after hibernating
    birth = ifelse(day/250!=1,0,R*0.5*(X[T]+Y[T])*(1- (X[T]+Y[T])/K) ) 
    
    # WNS infection restricted to hibernation season
    Rhob_s = ifelse(h<day && day<=364, 0,Rhob) 
    Betaxb_s = ifelse(h<day && day<=364, 0,Betaxb)
    Betabr_s = ifelse(h<day && day<=364, 0,Betabr)
    
    # keeps track of what infectious bats have died	
    deadinf = ifelse(day<(tau+taum+1), 0, Y[T-tau-taum]*Rhob_s) 								
    
    # the infectious bats
    infY = ifelse(day<tau+1, 0, Y[T-tau])	
    
    #susceptible bats can get infected from infectious bats and reservoir
    maxinf = ifelse(h<day && day<=364 || X[T]<1,0, as.integer(X[T]*Z[T]/V/theta)+(Betaxb_s*X[T]/(X[T]+Y[T])*(infY-deadinf))) 	 
    
    # all of the susceptibles become infected if there are fewer susceptibles than the max new infections
    newinf = ifelse(X[T]>maxinf, maxinf, X[T])							
    
    #susceptible bats are born, get infected, and die at the background mortality rate Mub
    dXdt =  	birth - newinf - as.integer(Mub*X[T]) 						
    X[T+1]=(ifelse(X[T]<0 || (X[T]+dXdt)<1, 0, X[T]+dXdt))		
    
    #infected bats increase when X is infected and die with WNS mortality Rhob_s after mortality period taum
    dYdt = (ifelse(X[T]+Y[T]<1, 0, newinf))  
    newdeath = ifelse(day<taum+1, 0, Y[T-taum]*Rhob_s) 			
    Y[T+1]= (ifelse(Y[T]+dYdt-newdeath-as.integer(Mub*Y[T])<1, 0, Y[T]+dYdt - newdeath-as.integer(Mub*Y[T])))				
    
    #spores are shed from infectious bats and grow in the environment
    dZdt_shed <- ifelse(infY <0, 0, Betabr_s*(infY)) 
    dZdt_growth <- ifelse(Z[T]>KZ, 0, (1 - Z[T]/KZ)*Z[T]*(10^Betaxr) - Z[T]) 
    dZdt_death <- Z[T] - Z[T]/(10^Muz)
    dZdt <- ifelse(dZdt_growth > 0, dZdt_shed + dZdt_growth - dZdt_death, dZdt_shed - dZdt_death)
    
    Z[T+1] <- (ifelse(Z[T] + dZdt < 1, 0, Z[T] + dZdt))	 
    
    # Y bats recover out of hibernation season depending on r
    X[T+1] = ifelse(h<day && day <=364, X[T+1]+r*Y[T+1], X[T+1])
    Y[T+1] = ifelse(h<day && day <=364, Y[T+1]-r*Y[T+1], Y[T+1])
    
    Time[T+1]<-T
    
    #	record values
    ExtTime<- max(Time[(X+Y)>0])+1 # in days
    ExtFungus <- max(Time[Z>0])+1 # in days
    ExtTimeVec<-c(ExtTime,Betaxb,Betabr, Betaxr, Y[T+1], X[T+1], Z[T+1])
    LiveB <- Y[T]+X[T]
    WNSB <- Y[T]
    SporeLoad <- Z[T]
    
  }
  
  ## BMB: NOT GOOD.  Always try to avoid global assignment <<-
  ##  (not always possible, but you should try)
  X<<-X
  Y<<-Y
  Z<<-Z
  Time<<-Time	
  V<<-V
  ExtTime<<-ExtTime
  ExtFungus<<-ExtFungus
  ExtTimeVec<<-ExtTimeVec	
  LiveB <<- LiveB
  WNSB <<- WNSB
  SporeLoad <<- SporeLoad
}	

## example track within seasons with several settings for sediment
years = 5
DATA<- matrix(NA, (365*years+1)*6, 5) 
SedID <-c("no growth", "Stream sand", "Stream silt/gravel", "Entrance sand", "Mud crack clay", "Leafy debris")
ValsSed <-c(0.0, 0.046, 0.202, 0.215, 0.307, 0.421)
KZSed <- c(3.01*10^4, 3.01*10^4, 5.22*10^4, 1.17*10^5, 3.43*10^5, 1.53*10^6 )
MSed <- c(10^6, 10^6, 10^6, 10^6, 10^6, 10^4)
ValsMuz <- c(0.004, 0.006, 0.007, 0.0006, 0.01, 0.004)

for(i in 1:6){
  SapronoseDF(
    R = 0.95, #birth rate (80% for the WNS females; only have 1 reproduction period a year, after the hibernation season)
    r = 0, # summer recovery rate for bats 0 or 0.1
    K = 10000, #the bat carrying capacity 
    KZ = 10^6*KZSed[i], # total sediment carrying capacity for the fungus; sediment mass 10^6 times max spore load from 1 gram
    Betaxb = 1, # bat to bat  infection (daily); each bat encounters 1 other bat every 10 days 
    Mub = 0.00032, #daily death rate calculated from avg. male & female longevity (Keen & Hitchcock)
    Rhob = 1/102, # experimental data from Lorch et al; positive controls all die
    taum = 120, # delay in mortality; days between infection and death
    Betabr = 100, # daily spores shed per WNS-infected bat 0 1 10 100
    Betaxr = ValsSed[i], #fungal growth rate 0 0.092 0.206 0.174 0.054 0.007
    Muz = ValsMuz[i], #452.38, # average fungal death rate between D56 and D238 (for the sediments in which fungus had stopped increasing)
    h = 200, # days in a hibernation season 125 150 175 200
    t = years, # years 10 20 100
    X0 = 9000, #starting susceptible bat population #Yr1 9000 #Yr2 0 #Yr3
    Y0 = 1, # infected bats #Yr1 1 #Yr2 8805 #Yr3 1 #Yr4 #Yr5 Y
    Z0 = 0, # starting number of spores Yr2 247168807 Yr3 6107127761 
    V= 1000, # hibernaculum volume 280 1000 m3
    theta = 10^6/(0.0435^3*4/3*pi), # threshold density for infection from environment (spores/m3) based on 10^6 spores to infect 1 bat of 0.04 m radius
    tau = 50 # days it takes to go from infected to infectious
  )
  
  DATA[((i-1)*(365*years+1)+1):((i-1)*(365*years+1)+365*years+1),1]=Time
  DATA[((i-1)*(365*years+1)+1):((i-1)*(365*years+1)+365*years+1),2]=ValsSed[i]
  DATA[((i-1)*(365*years+1)+1):((i-1)*(365*years+1)+365*years+1),3]=X
  DATA[((i-1)*(365*years+1)+1):((i-1)*(365*years+1)+365*years+1),4]=Y
  DATA[((i-1)*(365*years+1)+1):((i-1)*(365*years+1)+365*years+1),5]=Z
}

write.table(DATA, file = "5Yrs.txt", sep = " ")
Data <- read.table(file = "5Yrs.txt", header=TRUE)

## attach(Data)  ## BMB: CODE SMELL, don't use attach() if you can help it
Data <- as.data.frame(Data) ## we've managed to lose all of the variable names
                                        #  so variables below are 'V1', 'V2', which isn't very clear

sp <- ggplot(Data, aes(x=V1/365, y=V4/1000))
sp <- sp + geom_line(color="red")
sp <- sp + geom_line(data=Data, color="light blue", aes(x=V1/365, y=V3/1000))
sp <- sp + geom_line(data=Data, color="black", aes(x=V1/365, y=log10(V5+1)))
sp <- sp + facet_grid(V2 ~.)
sp <- sp + theme_bw()
sp <- sp + scale_x_continuous("Year")
sp <- sp + scale_y_continuous("Bat Population (1000s) / log (Pd CFUs) ")
sp <- sp + labs(title="Hibernation 150 Days")
sp <- sp + theme(text=element_text(family="Arial"))

print(sp)

ggsave("5Yrs.pdf",width=2.5, height=3.5)


## example Wrapped function to run through multiple variables



DATA<-matrix(NA,84,7) # for examining bat populations
SedID <-c("No growth", "Stream sand", "Stream silt/gravel", "Entrance sand", "Mud crack clay", "Leafy debris")
ValsSed <-c(0.0, 0.046, 0.202, 0.215, 0.307, 0.421)
KZSed <- c(3.01*10^4, 3.01*10^4, 5.22*10^4, 1.17*10^5, 3.43*10^5, 1.53*10^6 )
MSed <- c(10^6, 10^6, 10^6, 10^6, 10^6, 10^4) 
ValsMuz <- c(0.004, 0.006, 0.007, 0.0006, 0.01, 0.004)
rVals <- c(0, 0.1)

Vals <- c(10^-6, 10^-5, 10^-4, 10^-3, 10^-2, 10^-1, 1) #  growth suppression from competition or temperature

for (f in 1:2){
  for(i in 1:6){
    for(j in 1:7){
      
      SapronoseDF(
        R = 0.95, #birth rate (80% for the WNS females; only have 1 reproduction period a year, after the hibernation season)
        r = rVals[f], # summer recovery rate for bats 0 or 0.1
        K = 10000, #the bat carrying capacity 
        KZ = 10^6*KZSed[i], # carrying capacity for the fungus; sediment mass times max spore load from 1 gram
        Betaxb = 1, # bat to bat infection (daily); each bat encounters 1 other bat every 10 days 
        Mub = 0.00032, #daily death rate calculated from avg. male & female longevity (Keen & Hitchcock)
        Rhob = 1/102, # experimental data from Lorch et al; positive controls all die
        taum = 120, # delay in mortality; days between infection and death
        Betabr = 100, # daily spores shed per WNS-infected bat 0 1 10 100
        Betaxr = Vals[j]*ValsSed[i], #fungal growth rate 
        Muz = ValsMuz[i], #452.38, # average fungal death rate between D56 and D238 (for the sediments in which fungus had stopped increasing)
        h = 200, # days in a hibernation season 125 150 175 200
        t = 100, # years 10 20 100
        X0 = 9000, #starting susceptible bat population 
        Y0 = 1, # starting number of infected bats 
        Z0 = 0, # starting number of CFUS 
        V= 1000, # hibernaculum volumes  280 1000 m3
        theta = 10^6/(0.0435^3*4/3*pi), # threshold density for infection from environment (spores/m3) based on 10^6 spores to infect 1 bat of 0.04 m radius
        tau = 50 # days to go from infected to infectious
      )
      
      
      DATA[(f-1)*42 + (((i-1)*7)+j),1]<-ExtTime
      DATA[(f-1)*42 +(((i-1)*7)+j),2]<-ValsSed[i]
      DATA[(f-1)*42 +(((i-1)*7)+j),3]<-Vals[j]
      DATA[(f-1)*42 +(((i-1)*7)+j),4]<-LiveB
      DATA[(f-1)*42 +(((i-1)*7)+j),5]<-SporeLoad
      DATA[(f-1)*42 +(((i-1)*7)+j),6]<-rVals[f]
      DATA[(f-1)*42 + (((i-1)*7)+j), 7] <-ExtFungus
      
      
    }
  }
}


write.table(DATA, file = "100Yr.txt", sep = " ")
