#Loading the rvest package
library(RSelenium)
library(rvest)
library(lubridate)
library(dplyr)
library(ggplot2)
library(gganimate)
library(ggmap)

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
m_gc<-read.table("euro-cave-not-complete.csv",header=T,fill = T,sep = "\t",na.strings = "",quote = "",comment.char = "")
m_gc$url <- paste("https://www.geocaching.com/geocache/",m_gc$GC,sep = "")
m_gc$numpage <- round(m_gc$numfinds/25,digits = 0)

#navigate to your page
all_results<-NULL
for (i in m_gc$url) {
  
remDr$navigate(url = i)

#scroll down j times, waiting for the page to load at each time
for(j in 1:m_gc[m_gc$url==i,]$numpage){      
  remDr$executeScript(paste("scroll(0,",j*10000,");"))
  Sys.sleep(1.5)    
}

#get the page html
page_source<-remDr$getPageSource()

#parse it
users<-xml2::read_html(page_source[[1]]) %>% html_nodes(".h5") %>%
  html_text()

status<-xml2::read_html(page_source[[1]]) %>% html_nodes(".LogType .log-meta") %>%
  html_text(trim = T)

date<-xml2::read_html(page_source[[1]]) %>% html_nodes(".LogDate") %>%
  html_text()

log<-xml2::read_html(page_source[[1]]) %>% html_nodes(".LogText") %>%
  html_text(trim = T)

if (length(users)==0) {
  users<-""
}

if (length(status)==0) {
  status<-""
}

if (length(date)==0) {
  date<-""
}

if (length(log)==0) {
  log<-""
}

#replace multiple whitespaces with single space
clean_log <- gsub('\\s+',' ',log)
#trim spaces from ends of elements
clean_log <- trimws(clean_log)

page_results <- cbind.data.frame(users,status,date,clean_log,i)
all_results<-rbind(all_results,page_results)
}

# save results
write.table(all_results,file = "euro-cave-not-complete-results.tab",row.names = F,col.names = T,quote = F,sep = "\t")