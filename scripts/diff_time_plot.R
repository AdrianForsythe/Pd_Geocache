
eu.gc<-read.table("data/euro-cave-not-complete-results.tab",header=T,fill = T,sep = "\t",na.strings = "",quote = "",comment.char = "")
eu.gc$date<-mdy(eu.gc$date)

na.gc<-na.omit(read.table("data/cave-mines-not-complete-results.tab",header=T,fill = T,sep = "\t",na.strings = "",quote = "",comment.char = ""))
na.gc$date<-mdy(na.gc$date)

na.gc.td<-na.gc %>% group_by(users,date) %>% summarise()
na.gc.td<-diff(group_by(na.gc.td,users)$date)
na.gc.td<-as.data.frame(na.gc.td[na.gc.td>0])

ggplot(na.gc.td,aes(x=na.gc.td[,1]))+geom_histogram(fill="gray",color="black")+
  geom_vline(xintercept = 30,color="red",linetype="dashed",size=2)+
  geom_text(aes(x=10,y=7500,label="Spore viability limit at 23°C"),color="red",hjust=-1,size=6)+
  labs(x="Days between visits",y="Number of users")+
  theme_classic()+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size = 12))
ggsave(filename = "figures/diff.time.na.png",dpi = 300)

x<-na.gc[unique(na.gc$users) %in% unique(eu.gc$users),]

y<-eu.gc[unique(eu.gc$users) %in% unique(na.gc$users),]

td<-difftime(x$date,y$date)
td.p<-as.data.frame(as.numeric(td[td>0]))

ggplot(td.p,aes(x=td.p[,1]))+geom_histogram(fill="gray",color="black")+
  geom_vline(xintercept = 30,color="red",linetype="dashed",size=2)+
  geom_text(aes(x=10,y=15,label="Spore viability limit at 23°C"),color="red",hjust=-1,size=6)+
  labs(x="Days between visits",y="Number of users")+
  theme_classic()+
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size = 12))
ggsave(filename = "figures/diff.time.na.eu.png",dpi = 300)
