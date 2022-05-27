library(tidyverse)
library(lubridate)
library(parallel)

#'''
#' 1) The simplest model (Null) did not include any heterogeneity; spatial spread 
#' was homogeneous.
#' 2) In slightly more complicated models, the rate of spread was correlated 
#' linearly with the log of human population density (Human) or was lower 
#' when two townships were separated by a river and higher when they were not 
#' (River). 
#' 3) In the most complicated heterogeneous model, the rates of local spread 
#' were linear functions of the log of human population density with different 
#' slopes and intercepts for pairs of adjacent townships depending on whether 
#' they were separated by a river (RivHum1). 

# for parallel processing
cores=4

#' Function which prints a message using shell echo; useful for printing messages from inside mclapply when running in Rstudio
message_parallel <- function(...){
  system(sprintf('echo "\n%s\n"', paste0(..., collapse="")))
}



# read in presence data, used for times of county infections
presence.df<-readRDS("workflow/data/presence.df.rds") %>% 
  select(-geoms) %>% 
  as.data.frame() %>% 
  filter(STATEPROV=="New York")

# range of dates for the pandemic, in cumulative number of days
presence.df$pand.years<-((year(presence.df$date)-min(year(presence.df$date))))

# null expectations for translocations
mu<-3*10^(-4)

# limit of number of days before all are infected
days.max<-max(presence.df$pand.years)

# dataframe with info on adjacency
total.touching<-read.csv(file = "workflow/data/total-touching.csv",header = T) %>% filter(county %in% presence.df$county & county2 %in% presence.df$county)

# adjacency matrix is kinda messy, should only keep upper.tri
m<-total.touching %>%
  pivot_wider(names_from = county2,values_from = touching) %>% 
  column_to_rownames("county") %>% 
  as.matrix()
m[upper.tri(m)]<-NA
total.touching<-m %>% as.data.frame() %>% rownames_to_column("county") %>% pivot_longer(cols = -county,names_to = "county2",values_to = "touching") %>% filter(!is.na(touching))

init.counties<-presence.df %>% filter(pand.years==1) %>% pull(county)

results<-NULL
perm<-1
while (perm <= 100) {

  # starting with X = 1 (no infections) for all counties
  n.infected<-1
  
  # we start at day 0
  current.time<-0
  
  all.counties<-unique(c(total.touching$county,total.touching$county2)) %>% 
    data.frame() %>% 
    mutate(X=1)
  
  colnames(all.counties)<-c("county","X")
  n.counties<-nrow(all.counties)
  
  all.counties[all.counties$county %in% init.counties,]$X<-0
  
  # repeat until each county becomes infected.   
  while (n.infected<n.counties) {
    # Step 1
    # the total rate of infection in the jth county, ρj, where:
    # ρj = μjXj + ∑i λi,j Xj(1 − Xi)
    
    # i = infected county
    # j = uninfected county
    # μ = translocations
    # λ = local rate of spread
    # Xj = 1 if the jth county is uninfected, and Xj = 0 otherwise.
    i.list<-all.counties %>% distinct() %>% pull(county) 
    local.rates<-mclapply(i.list,mc.cores = cores,function(i){
      # message_parallel("Current county:",i)
      # compute infection probs for each
      # if the ith and jth townships are not adjacent, then λi,j = 0
      Xi<-all.counties %>% filter(county==i) %>% pull(X)
      
      # compute lambda for all adjacent counties
      r<-sum(unlist(lapply(all.counties %>% pull(county),function(j) {
        lambda<-total.touching %>% filter(county%in%c(i,j) & county2%in%c(i,j)) %>% 
          mutate(ij=ifelse(touching==TRUE,0.47,0)) %>% pull(ij)
        Xj<-all.counties %>% filter(county==j) %>% pull(X)
        lambda*Xj*(1-Xi)
      })))
      
      # all uninfected counties
      # muj and Xj
      # null assumptions?
      j.mu<-mu
      Xj<-all.counties %>% filter(county==i) %>% pull(X)
      j.muXj<-j.mu*Xj
      pj<-(j.mu*Xj)+r
      data.frame(Xi=Xi,county=i,pj=pj)
    })
    local.rates<-do.call(rbind, local.rates)
    
    # Step 2 
    # total rate of infection for all townships, Λ = ∑jρj
    total.rate<-sum(local.rates$pj)
    
    # now update infection status?
    
    # Step 3
    # Compute the waiting time before a county becomes infected next
    # Draw a random number to determine the elapsed time. 
    # we assume waiting times are distributed exponentially with rate parameter Λ.
    # After computing the elapsed time, we see whether the date when rabies was observed in one of the forced townships preceded the simulated event. If so, the borders are forced, and no event is simulated.
    elapsed.t<-round(rexp(1, rate = total.rate))
    current.time<-elapsed.t+current.time
    
    if (elapsed.t != 0) {
      n.infected.new<-(n.counties-sum(all.counties$X))-n.infected
      
      # force
      # (D) Check to see if any of the edges had become infected in the elapsed interval. 
      to.force<-presence.df %>% 
        filter(pand.years<= current.time) %>% 
        distinct(county) %>% pull(county)
      
      if (length(to.force)!=0) {
        # (F) Infect the forced edge or the infected county
        all.counties<-all.counties %>% mutate(X=ifelse(county%in%to.force,0,X))
        # return(all.counties)
      } else {
        # (E) If no edges were forced, select a random county to infect. 
        # townships that had higher rates of infection had higher probabilities of becoming infected.
        # Random townships were chosen from the multinomial distribution
        # the probability that the ith county was chosen was ρi/Λ.
        
        uninfected <- all.counties %>% left_join(local.rates,by = "county") %>% filter(X==1) %>% column_to_rownames("county")
        infect.rate<-stats::rmultinom(n=1,size=nrow(uninfected),prob = as.vector(uninfected$pj)/total.rate)
        to.infect<-uninfected[sample(x = local.rates %>% filter(Xi==1) %>% pull(county),size = 1,prob = infect.rate),]
        # Finally, the state of the infected county is updated
        all.counties<-all.counties %>% mutate(X=ifelse(county%in%rownames(to.infect),0,X),
                                              date.infected=current.time)
        # return(all.counties)
        # all.counties["county"==to.infect,"X"]<-0 # update records of infected counties
      }
    } else {
      next
    }
    n.infected<-all.counties %>% filter(X==0) %>% distinct(county) %>% nrow()
    current.time<-elapsed.t+current.time
    print(paste("Current time: ",current.time,"    Elapsed time: ",elapsed.t,"    Number of Infected Counties: ",n.infected))
    # results<-bind_rows(results,data.frame("permutation"=perm,"Current time"=current.time,"Elapsed time"=elapsed.t,"Number of Infected Counties"=n.infected))
    results<-bind_rows(results,bind_cols(perm,all.counties,current.time))
  }
  perm<-perm+1
}

results %>% 
  filter(X == 0) %>% 
  group_by(`...1`,county) %>% 
  filter(`...4`==min(`...4`)) %>% 
  left_join(presence.df %>% select(pand.years,county),by="county") %>% 
  filter(!is.na(county)) %>% 
  group_by(county,pand.years) %>% 
  mutate(e=((pand.years-`...4`)^2)/`...4`) %>% summarise(e=sum(e)) %>% 
  ggplot(aes(x=e,y=pand.years))+geom_point(size=2)+geom_smooth(method = "lm")+ggrepel::geom_text_repel(aes(label=county))+
  labs(x="Expected",y="Observed")+
  see::theme_modern()
ggsave(plot=last_plot(),filename = "results.png",dpi = 300,height=8,width=8,units="in")  

presence.df %>% select(pand.years)
  # arrange(pand.years) %>% 
  # mutate(status=ifelse(WNS_STATUS=="Confirmed",1,0),
         # cumsum=cumsum(status),
         # dataset="truth") %>% 
  left_join(results %>% rename("pand.years"="Current.time"),by="pand.years") %>% 
  ggplot(aes(x=pand.years,y=cumsum))+geom_path(aes(color=dataset))

presence.df %>% ggplot()+geom_sf()

write.csv(x=results,file="workflow/data/SmithSimNull.csv",quote=FALSE,row.names = FALSE)
