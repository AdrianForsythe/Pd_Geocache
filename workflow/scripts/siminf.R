library(SimInf)
library(tidyverse)
library(lubridate)
relevant.records<-read.csv("workflow/data/relevant-records.csv",header = T) %>% filter(ymd(GC.Date) >= "2008-01-01")
# days of the pandemic
relevant.records$pand.days<-((year(relevant.records$GC.Date)-min(year(relevant.records$GC.Date)))*365)+yday(ymd(relevant.records$GC.Date))

# need a list of to-and-from movements for each user
fnt.mvm<-relevant.records %>% group_by(User) %>% mutate(Entries=n()) %>% filter(Entries > 1) %>% select(GC,User,GC.Date,pand.days) %>% 
  summarise(movements=list(GC),dates=list(pand.days)) %>% 
  separate_rows(sep = ",",convert = T)

caches<-fnt.mvm %>% 
  group_by(User) %>% mutate(num=row_number()) %>% 
  arrange(User,-dates) %>% 
  select(-dates) %>% 
  spread(value = movements,key=num) %>% mutate_all(as.character)

dates<-fnt.mvm %>% 
  group_by(User) %>% mutate(num=row_number()) %>% 
  arrange(User,-dates) %>% 
  select(-movements) %>% 
  spread(value = dates,key=num)

# I don't have a better way to do this yet
tnf.lists<-function(x){
  odd<-rev(seq(3, ncol(x), 2))
  from.results<-NULL
  to.results<-NULL
  for (i in 1:nrow(x)) {
    for (j in odd) {
      from<-x[i,j]
      to<-x[i,j-1]
    } 
    from.results<-na.omit(c(from.results,from))
    to.results<-na.omit(c(to.results,to))
  }
  return(data.frame(cbind(from.results,to.results)))
}
tnf.cache<-tnf.lists(x=caches)
tnf.date<-tnf.lists(x=dates)

num_users<-NULL
for (i in 1:nrow(tnf.cache)) {
  r<-fnt.mvm %>% 
    filter(movements %in% c(tnf.cache[i,1],tnf.cache[i,2])) %>% 
    group_by(dates) %>% 
    summarise(num_users=n()) %>% cbind(tnf.cache[i,1],tnf.cache[i,2])
  num_users<-rbind(num_users,r)
  }
colnames(num_users)<-c("dates","num_users","from","to")

#####
# reps
n <- 1000
nnodes<-n_distinct(fnt.mvm$movements)

# closed population
transitions <- c("S -> b*S*I/(S+I) -> I")
compartments <- c("S", "I")

u0 <- data.frame(S = rep(0, nnodes), I = rep(0, nnodes))
add <- data.frame(event = "enter", time = rep(1:10, each = 5),
                  node = 1:5, dest = 0, n = 1:5, proportion = 0, select = 1, shift = 0)
infect <- data.frame(event = "enter", time = 25, node = 5,
                     dest = 0, n = 1, proportion = 0, select = 2, shift = 0)

move <- data.frame(event = "extTrans", time = num_users$dates,
                   node = as.numeric(num_users$from), dest = as.numeric(num_users$to),
                   n = num_users$num_users, proportion = 0, select = 4, shift = 0)
remove <- data.frame(event = "exit", time = c(70, 110),
                     node = rep(1:5, each = 2), dest = 0, n = 0, proportion = 0.2,
                     select = 4, shift = 0)
events<-rbind(move)
model <- SISe(u0 = u0, tspan = 1:max(num_users$dates), events = events, 
              phi = 0, upsilon = 0.012,epsilon = 0.1, gamma = 0.1, alpha = 1, beta_t1 = 0.10,
              beta_t2 = 0.12, beta_t3 = 0.12, beta_t4 = 0.10, end_t1 = 91,
              end_t2 = 182, end_t3 = 273, end_t4 = 365)
# model <- mparse(transitions = transitions, compartments = compartments,events = events,
#                    gdata = c(b = 0.16, g = 0.077), u0 = u0, tspan = 1:(10*365))
result <- run(model = model)

plot(result)

mean(replicate(n = 1000, {
  nI <- trajectory(run(model = model), index = 1:4)$I
  sum(nI) > 0
}))
