# fit glm model to data
glm.fit<-function(both.weights,relevant.records,glm.out,exp.growth.rate){

  require(tidyverse)
  
  # for testing
  # input
  ## num shared users
  both_weights<-read.csv("workflow/data/gc.weights.csv",header=T) %>% 
    mutate(touching.infected=replace_na(ifelse(county1.incidence==TRUE & touching==1,1,0),0))
  ## incidence
  full_df<-read.csv("workflow/data/full-occurrance-df.csv") %>% 
    mutate(incidence=ifelse(WNS_STATUS=="Confirmed",1,0),
           year=lubridate::year(gsub("-.+","/01/01",WNS_MAP_YR)))
  
  ## touching infected county
  total_touching<-read.csv("workflow/data/total-touching-infected.csv")
  ## gc records
  relevant.records<-read.csv("workflow/data/relevant-records.csv")
  ## infection rate
  infection.rate<-read.csv("workflow/data/infection-rate.csv",header=T)
  # output
  ## exponential growth rate figure
  exp.growth.rate<-"workflow/figures/exp-growth-rate.png"
  
  # model 1
  touch.df<-total_touching %>% rename("county"="county.y") %>% 
    left_join(full_df)
  
  model1 <- glm(incidence ~ year+n.touching ,data = touch.df, family = binomial(link = "cloglog"))
  summary(model1)
  
  # model 2
  sum.rr<-relevant.records %>% group_by(county,gc.year,presence.year) %>% summarise(total.users=n_distinct(User)) %>% filter(gc.year==presence.year) %>%
    left_join(full_df,by=c("county","presence.year"="year"))
  
  model2 <- glm(incidence ~ total.users+total.users ,data = sum.rr, family = binomial(link = "cloglog"))
  summary(model2)
  
  # intrinsic growth rate of infection
  lambda=as.numeric(model1$coefficients[3])
  lwr.lambda<-confint(model1)["n.touching",1]
  upr.lambda<-confint(model1)["n.touching",2]

  gamma=1/3; #infectious period
  R0=(lambda/gamma)+1 #basic reproduction number

  # taking the total number of sites infected per year and the cumulative sum
  cave_rate<-relevant.records %>%
    mutate(date=lubridate::ymd(wns.map.yr)) %>%
    arrange(date) %>%
    group_by(date) %>%
    summarise(cave.count = n_distinct(GC)) %>% ungroup() %>%
    mutate(inf.caves = cumsum(cave.count)) %>%
    ggplot(aes(x=inf.caves,y=cave.count))+
    geom_point()+
    geom_line()+
    labs(x="cumulative number of sites within infected counties",y="number of sites within newly infected counties")+
    ggtitle(label = substitute(`Exponential growth rate`~lambda==i,list(i=format(round(lambda,2)))))+
    geom_abline(aes(intercept=1, slope=lambda),alpha=0.5,size=2,color="red")+ # slope line is gone
    theme_classic()
  ggsave(plot=cave_rate,file=exp.growth.rate,dpi=300)
  
  # both_weights %>%
  #   arrange(date) %>%
  #   group_by(date) %>%
  #   summarise(inf.caves = cumsum(incidence),
  #             touching.count = sum(touching)) %>%
  #   ggplot(aes(y=inf.caves,x=touching.count))+
  #   geom_point()+
  #   theme_classic()
  # 
  # Stll missing some parts...
}

glm.fit(snakemake@input[[1]],snakemake@input[[2]],snakemake@output[[1]],snakemake@output[[2]])
