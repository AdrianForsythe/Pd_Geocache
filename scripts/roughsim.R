## make sure to run through coordinate-overlap.R and geocache-weight.R first!
## that will generate a file "gc-shared-users.csv" 
## that will have the pairwise number of shared users and adjacency index per county!

both.weights<-read.csv("data/gc-shared-users.csv",header=T)

# there are no records from 2016? wierd.

# just loop 'em!
for (i in 2008:2019) {
  x<-both.weights %>%
  filter(year==i) %>%
  select(county1,county2,touching) %>%
  pivot_wider(names_from = county2,values_from = touching) %>%
  replace(is.na(.), 0)
  row.names(x)<-x$county1
  x<-x[,-1]
  assign(paste0("y",i),x)
}

#Lets try just the first step with ivec
initialvec <- (both.weights$incidence)

#If we go through relevant records, we can create the proper weight matrix which we want
#y2008matrix is the matrix in question
#Alternatively county.m is the constant weight matrix of all the counties from the beginning 

#Errors occuring in matrix multiplication because I think WNS_STATUS is a character not numeric


#Now we are going to a for loop process

#pseudo code is:
#After a initial matrix has been defined for starting values
#For every year i in the list of years (which contains 2008-2019)
#uninf <- (initialvec) == 0
#movement <- (y2008matrix)%*%as.numeric(initialvec)
#beta <- -0.19
#FOI <- beta * movement
#hazard <- 1 - exp(FOI)
#I think the size is right. Right?
#initialvec[uninf] <- rbinom(sum(uninf), size = 727, prob = hazard)
#return(initialvec)
#Ok so this works but its doesn't feel like its intuitively right to me with what is meant by year changing matrix

# again, not sure why 2016 is missing.... I'll have to look back at the original records...
year <- c(2008:2015,2017:2019)
currentyear <- 2008
countymatrix <- list(y2008,y2009,y2010,y2011,y2012,y2013,y2014,y2015,y2017,y2018,y2019)

while(currentyear < 2020){
  uninf <- (initialvec) == 0
  #Currently using a time averaged matrix until can think a little more about iterating through the list of matricies
  movement <- (county.m)%*%as.numeric(initialvec)
  beta <- -0.19
  FOI <- beta * movement
  hazard <- 1 - exp(FOI)
  #I think the size is right. Right?
  initialvec[uninf] <- rbinom(sum(uninf), size = length(unique(both.weights$county1)), prob = hazard)
  return(initialvec)
  currentyear<- currentyear + 1
}
