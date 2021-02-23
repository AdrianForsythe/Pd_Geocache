##cave dataset
# fit glm model to data
require(tidyverse)
glm.fit<-function(both.weights,relevant.records,glm.out,exp.growth.rate){

  both_weights<-read.csv(both.weights)
  model <- glm(as.factor(incidence) ~ as.factor(touching) ,data = both_weights, family = binomial(link = "cloglog"))

  sink(glm.out)
  print(summary(model))
  sink()  # returns output to the console
  # Is p value worth keeping in here?

  #intrinsic growth rate of infection
  lambda=as.numeric(model$coefficients[2])
  lwr.lambda<-confint(model)[1]
  upr.lambda<-confint(model)[2]

  gamma=1/3; #infectious period
  R0=(lambda/gamma)+1 #basic reproduction number

  # taking the total number of sites infected per year and the cumulative sum
  cave_rate<- read.csv(relevant.records) %>%
    mutate(date=lubridate::ymd(wns.map.yr)) %>%
    arrange(date) %>%
    group_by(date) %>%
    summarise(cave.count = n_distinct(GC)) %>% ungroup() %>%
    mutate(inf.caves = cumsum(cave.count)) %>%
    ggplot(aes(x=inf.caves,y=cave.count))+
    geom_point()+
    labs(x="cumulative number of infected caves",y="number of new caves")
    ggtitle(substitute(paste0("Exponential growth rate", ~lambda," = ",round(lambda,2))))+
    geom_abline(aes(intercept=1, slope=lambda))+ # slope line is gone
    theme_classic()
  ggsave(plot=cave_rate,file=exp.growth.rate,dpi=300)
  #Stll missing some parts
}

glm.fit(snakemake@input[[1]],snakemake@input[[2]],snakemake@output[[1]],snakemake@output[[2]])
