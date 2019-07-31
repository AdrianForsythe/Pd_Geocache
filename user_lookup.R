# geocache user lookup
library(RSelenium)
library(rvest)
library(lubridate)
library(dplyr)
library(ggplot2)

#start RSelenium
#connect to running server
rD <- rsDriver(port = 4445L, browser = 'firefox')
remDr <- rD$client # You dont need to use the open method 

# navigate to geocaching.com
remDr$navigate(url = "https://www.geocaching.com/account/signin?returnUrl=%2fplay")

# geocaching.com
#send username
username <- remDr$findElement(using = "id", value = "UsernameOrEmail")
username$sendKeysToElement(list("a_forsythe"))

#send password and Enter
passwd <- remDr$findElement(using = "id", value = "Password")
passwd$sendKeysToElement(list("qazwsxedc"))

SignOn <- remDr$findElement(using = "id", "SignIn")
SignOn$clickElement()

# generate list of urls
user_gc<-read.table("cave-mines-not-complete-results.tab",header=T,sep="\t",comment.char = "",quote = "")

#navigate to your page
all_results<-NULL
for (i in unique(user_gc$users)[[1]]) {
  page_results<-NULL
  remDr$navigate(url = paste("https://www.geocaching.com/seek/nearest.aspx?ul=",i,sep=""))
  
  ### First page
  #get the page html
  page_source<-remDr$getPageSource()
  
  # total finds
  num.finds<-xml2::read_html(page_source[[1]]) %>% html_nodes(".NoBottomSpacing span b:nth-child(1)")%>%
    html_text()
  
  # locations of finds
  gcs.first<-xml2::read_html(page_source[[1]]) %>% html_nodes(".Merge .small") %>%
    html_text()
  
  # cache visit date
  visit.date<-xml2::read_html(page_source[[1]]) %>% html_nodes(".AlignCenter~ td+ td .small")%>%
    html_text()
  
  first.page<-cbind(gcs.first,visit.date)
  
  ### Next pages  
  #click through j times, determined by num.finds
  next.pages<-NULL
  for(j in 1:(as.numeric(num.finds)/20+2)){      
    if(length(remDr$findElement(using="xpath",value="//a[(((count(preceding-sibling::*) + 1) = 16) and parent::*)]//b")) != 0) {# element exists, then click
    next.page<-remDr$findElement(using="xpath",value="//a[(((count(preceding-sibling::*) + 1) = 16) and parent::*)]//b")
    next.page$clickElement()
    
    #get the page html
    page_source<-remDr$getPageSource()
    
    # locations of finds
    gcs.next<-xml2::read_html(page_source[[1]]) %>% html_nodes(".Merge .small") %>%
      html_text()
    
    # cache visit date
    visit.date.next<-xml2::read_html(page_source[[1]]) %>% html_nodes(".AlignCenter~ td+ td .small")%>%
      html_text()
    next.pages<-cbind(gcs.next,visit.date.next)
    Sys.sleep(1.5)    
    }
  }
  page_results <- cbind(i,num.finds,rbind(first.page,next.pages))
  all_results<-rbind(all_results,page_results) 
}
