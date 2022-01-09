## make sure to run through coordinate-overlap.R and geocache-weight.R first!
## that will generate a file "gc-shared-users.csv" 
## that will have the pairwise number of shared users and adjacency index per county!

require(tidyverse)

## num shared users
both.weights<-read.csv("workflow/data/gc.weights.csv",header=T) %>% 
  mutate(touching.infected=replace_na(ifelse(county1.incidence==TRUE & touching==1,1,0),0))
## incidence
full_df<-read.csv("workflow/data/full-occurrance-df.csv") %>% 
  mutate(incidence=ifelse(WNS_STATUS=="Confirmed",1,0),
         year=lubridate::year(gsub("-.+","/01/01",WNS_MAP_YR)))
## touching infected county
total_touching<-read.csv("workflow/data/total-touching-infected.csv")
## gc records
relevant.records<-read.csv("workflow/data/relevant-records.csv")

touch.df<-total_touching %>% rename("county"="county.y") %>% 
  left_join(full_df)

# create invidual vectors for each year
# just loop 'em!
for (i in 2008:2019) {
  x<-both.weights %>%
  filter(year==i) %>%
  dplyr::select(county,county.y,touching) %>%
  mutate(touching=as.numeric(touching)) %>% 
  pivot_wider(id_cols = county,names_from = county.y,values_from = touching,values_fn = unique) %>%
  replace(is.na(.), 0) %>% column_to_rownames("county")
  x<-x %>% relocate(rownames(x))
  assign(paste0("t",i),x)
}

for (i in 2008:2019) {
  x<-both.weights %>%
    filter(year==i) %>%
    dplyr::select(county,county.y,num_shared) %>%
    pivot_wider(id_cols = county,names_from = county.y,values_from = num_shared,values_fn = unique) %>%
    replace(is.na(.), 0) %>% column_to_rownames("county")
  x<-x %>% relocate(rownames(x))
  assign(paste0("s",i),x)
}

# Lets try just the first step with ivec
initialvec.df <- both_weights %>% #distinct(.keep_all = T) %>% 
  mutate(incidence=ifelse(WNS_STATUS=="Confirmed",1,0),
         year=lubridate::year(gsub("-.+","/01/01",WNS_MAP_YR)))

# If we go through relevant records, we can create the proper weight matrix which we want
# y2008 is the matrix in question
# Alternatively county.m is the constant weight matrix of all the counties from the beginning 

# Errors occuring in matrix multiplication because I think WNS_STATUS is a character not numeric

# Now we are going to a for loop process

# pseudo code is:
# After a initial matrix has been defined for starting values
# For every year i in the list of years (which contains 2008-2019)
# uninf <- (initialvec) == 0
# movement <- (y2008matrix)%*%as.numeric(initialvec)
# beta <- -0.19
# FOI <- beta * movement
# hazard <- 1 - exp(FOI)
# I think the size is right. Right?
# initialvec[uninf] <- rbinom(sum(uninf), size = 727, prob = hazard)
# return(initialvec)
# Ok so this works but its doesn't feel like its intuitively right to me with what is meant by year changing matrix

year <- c(2008:2019)
currentyear <- 2008
countymatrix <- list(paste0("y",2008:2019))

while(currentyear < 2020){
  initialvec<-initialvec.df %>% filter(year==currentyear & county %in% rownames(t2008)) %>% 
    pivot_wider(id_cols = "county",names_from = "county.y",values_from = "incidence",values_fn = function(x) mean(x,na.rm=T)) %>% 
    column_to_rownames("county") %>% 
    mutate_all(as.numeric) 
  initialvec<-initialvec %>% arrange(rownames(t2008)) %>%  relocate(rownames(t2008))
  
  uninf <- (initialvec) == 0
  # Currently using a time averaged matrix until can think a little more about iterating through the list of matricies
  movement <- (t2008)%*%as.numeric(initialvec)
  beta <- -0.19
  FOI <- beta * movement
  hazard <- 1 - exp(FOI)
  # I think the size is right. Right?
  initialvec[uninf] <- rbinom(sum(uninf), size = length(unique(c(both.weights$county,both.weights$county.y))), prob = hazard)
  return(initialvec)
  currentyear<- currentyear + 1
}
