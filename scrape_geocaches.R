#Loading the rvest package
library(RSelenium)
library(wdman)
library(rvest)

#start RSelenium
#connect to running server
rD <- rsDriver(port = 4445L, browser = 'firefox')
remDr <- rD$client # You dont need to use the open method 

#navigate to your page
remDr$navigate(url = "https://project-gc.com/Home/Dashboard")

# GCs!
GC<-"GC392A" 

SearchBar<-remDr$findElement(using = "id","gccode")
SearchBar$sendKeysToElement(list(GC))

SearchEnter<-remDr$findElement(using = "id","gccode_cache")
SearchEnter$clickElement()

page <- remDr$getPageSource()
provs <- page[[1]] %>% read_html() %>% 
  html_node("#app > div > div.c-col-results > div:nth-child(3) > div > section") %>% 
  html_text()

LogBook <- remDr$findElement(using = "link text", "View Logbook")
LogBook$clickElement()


# geocaching.com
#send username
username <- remDr$findElement(using = "id", value = "UsernameOrEmail")
username$sendKeysToElement(list("a_forsythe"))

#send password and Enter
passwd <- remDr$findElement(using = "id", value = "Password")
passwd$sendKeysToElement(list("qazwsxedc"))

SignOn <- remDr$findElement(using = "id", "SignIn")
SignOn$clickElement()




#get the page html
page_source<-remDr$getPageSource()

#parse it
users<-html(page_source[[1]]) %>% html_nodes(".h5") %>%
  html_text()

status<-html(page_source[[1]]) %>% html_nodes(".LogType .log-meta") %>%
  html_text(trim = T)

date<-html(page_source[[1]]) %>% html_nodes(".LogDate") %>%
  html_text()

as.table(results)