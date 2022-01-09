# developing a discrete-event simulator:
# a simulator in continuous time for which the infection of each townships occurs at a unique point in time.
# Each time one township becomes infected, the probability of infection in nearby townships increases.

# The algorithm determines which township is infected, and when it becomes infected. The algorithm has six steps:
# 1. For each township, sum the infection rates to compute the Township Rate.
# - A township may become infected through local transmission if adjacent townships are infected. Local transmission rates may be higher or lower depending on the population density of the neighbor and whether or not two townships are separated by a river.
# - A township may become infected through long-distance translocation of rabid raccoons.
# - For each township, the total rate is the sum of the local rate divided by the number of neighbors plus the long-distance translocation rate.
# 2. Compute the Total Rate of change, the sum of all the Township Rates .
# 3. Compute the elapsed time, the waiting time to the next event. The waiting time is exponentially distributed with expected time 1/Total Rate. In the simulator, we draw a random variate from this distrubution.
# 4. In the simulator, we "force" the edges on the western border of Connecticut. The time to infection is not simulated, but given by the data. After computing the elapsed time, we see whether the date when rabies was observed in one of the forced townships preceded the simulated event. If so, the borders are forced, and no event is simulated.
# 5. If no event is forced, we simulate infection. Only one township becomes infected at a time, so the algorith simply selects one. The probability for any particular township is its Township Rate / Total Rate. We draw a multinomial random variate to determine which township is infected.
# 6. Finally, once a township has been infected, we update the infection status and repeate the algorithm until every township is infected.

tt<-read.csv("workflow/data/total-touching.csv")
ncounties<-n_distinct(tt$county)
ttinf<-read.csv("workflow/data/total-touching-infected.csv")

l.rate<-tt %>% 
  group_by(county) %>% 
  summarise(nt=sum(touching,na.rm = T)) %>% ungroup() %>% 
  left_join(ttinf,by=c("county"="county.y")) %>% 
  group_by(county,WNS_MAP_YR) %>% 
  summarise(local.rate=n.inf/n.touching)

t.rate <- l.rate %>% group_by(county) %>% summarise(county.rate=sum(local.rate,na.rm = T))

# remotes::install_github("r-simmer/simmer")
# remotes::install_github("r-simmer/simmer.plot")
library(simmer)
library(simmer.plot)
library(parallel)
set.seed(42)

# Arrival rate
lambda <- 3/20
# infection rate 
mu <- c(1/8, 1/3)
# Probability of bat
p <- 0.75

# Theoretical resolution
A <- matrix(c(1,   mu[1],            0,
              1, -lambda, (1-p)*lambda,
              1,   mu[2],       -mu[2]), byrow=T, ncol=3)
B <- c(1, 0, 0)
P <- solve(t(A), B)
N_average_theor <- sum(P * c(1, 0, 1)) ; N_average_theor


visitor <- trajectory("wns") %>%
  ## add an intake activity 
  seize("county", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("county", 1)

envs <- mclapply(1:100, function(i) {
  simmer("WNSsim") %>%
    add_resource("county", 1) %>%
    add_resource("battransimission", 1) %>%
    add_generator("visitor", visitor, function() rnorm(1, 10, 2)) %>%
    run(80) %>%
    wrap()
})

plot(get_mon_resources(envs), "visitor", "pump", items="system") +
  geom_hline(yintercept=N_average_theor)
