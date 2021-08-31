##cave dataset
# fit glm model to data
glm.fit<-function(both.weights,relevant.records,glm.out,exp.growth.rate){

  require(tidyverse)
  
  # for testing
  both_weights<-read.csv("workflow/data/gc.weights.csv",header=T)
  relevant_records<-"workflow/data/relevant-records.csv"
  # glm.out<-
  exp.growth.rate<-"workflow/figures/exp-growth-rate.png"
  
  # both_weights<-read.csv(both.weights)
  relevant.records<-read.csv(relevant_records)
  
  model <- glm(incidence ~ touching ,data = both_weights, family = binomial(link = "cloglog"))
  # model <- glm(incidence ~ num_shared ,data = both_weights, family = binomial(link = "cloglog"))
  
  sink(glm.out)
  print(summary(model))
  sink()  # returns output to the console
  # Is p value worth keeping in here?

  # intrinsic growth rate of infection
  lambda=as.numeric(model$coefficients[2])
  lwr.lambda<-confint(model)[1]
  upr.lambda<-confint(model)[2]

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
