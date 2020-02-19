sample.description<-function(dat){
set.seed(1234)

rand.locs<-sample(1:nrow(dat), 100, replace=FALSE)

sampled_set<-dat[rand.locs,]

rD <- rsDriver(port = 4445L, browser = 'firefox')
remDr <- rD$client

# navigate to geocaching.com
# going directly to the user sign in page
remDr$navigate(url = "https://www.geocaching.com/account/signin?returnUrl=%2fplay")

# send username
username <- remDr$findElement(using = "id", value = "UsernameOrEmail")
username$sendKeysToElement(list("a_forsythe"))

# send password and click Enter
passwd <- remDr$findElement(using = "id", value = "Password")
passwd$sendKeysToElement(list("v3X@s45aqePN"))

SignOn <- remDr$findElement(using = "id", "SignIn")
SignOn$clickElement()

#####
keywords <- c("flashlight","cave","mine","bat","ore","mineral")
full_set<-NULL

for (i in as.character(sampled_set$url)) {
  # navigate to page i
  remDr$navigate(url = i)
  
  page_source<-remDr$getPageSource()

  description<-xml2::read_html(page_source[[1]]) %>% html_nodes(".UserSuppliedContent") %>%
    html_text()

  column_matches<-NULL
    for (j in keywords) {
      if(length(grep(paste(" ",j,sep=""),description,ignore.case = TRUE,)>0)){
        match<-grep(paste(" ",j,sep=""),description,ignore.case = TRUE)
      } else {
        match <- 0
      }
      column_matches<-cbind(column_matches,match)
    }
  full_set<-rbind(full_set,cbind(column_matches,i))
  colnames(full_set) <- c(keywords,"url")
  Sys.sleep(0.5)
}

#
long_full_set <- full_set %>% as.data.frame(full_set) %>%
  pivot_longer(cols = -url,names_to = "keyword") %>% mutate(value=as.numeric(levels(value))[value]) %>% mutate(presence = ifelse(value > 0 ,1,0))

wide_full_set<-long_full_set %>% pivot_wider(names_from = keyword,values_from = presence,id_cols = url,values_fn = list(presence = sum))

png("figures/keyword_combinations_sample.png",res = 300,height = 800,width = 1000,units = "px")
UpSetR::upset(as.data.frame(wide_full_set),nintersects = NA,nsets = length(keywords),order.by = "freq")
dev.off()
}