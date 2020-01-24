# geocache user lookup

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

# URL encoding of users
user_gc$users.url<-htmltools::urlEncodePath(as.character(user_gc$users))

# length of time to sleep
s<-1

user.list<-unique(user_gc$users.url)

# start again after last user!
user.list<-user.list[4:22749]

# start fresh
# all_results<-NULL

for (i in user.list) {
  page_results<-NULL
  #navigate to your page
  
  remDr$navigate(url = paste("https://www.geocaching.com/seek/nearest.aspx?ul=",i,sep=""))
  
  # wait a bit for page to load
  Sys.sleep(0.5)
  
  ### First page
  #get the page html
  page_source<-remDr$getPageSource()
  
  # total finds
  num.finds<-xml2::read_html(page_source[[1]]) %>% html_nodes(".NoBottomSpacing span b:nth-child(1)")%>%
    html_text()
  
  # cache title
  gc.title.first<-xml2::read_html(page_source[[1]]) %>% html_nodes(".lnk span")%>%
    html_text()
  
  # locations of finds
  gcs.first<-xml2::read_html(page_source[[1]]) %>% html_nodes(".Merge .small") %>%
    html_text()
  
  #replace multiple whitespaces with single space
  clean.gcs.first <- gsub('\\s+',' ',gcs.first)
  #trim spaces from ends of elements
  clean.gcs.first <- trimws(clean.gcs.first)
  
  # cache visit date
  visit.date<-xml2::read_html(page_source[[1]]) %>% html_nodes(".AlignCenter~ td+ td .small")%>%
    html_text()
  
  first.page<-cbind(gc.title.first,clean.gcs.first,visit.date)
  
  num.pages<-round((as.numeric(num.finds)/20),0)
  
  ### Next pages  
  #click through j times, determined by num.pages
  next.pages<-NULL
  for(j in 2:num.pages){
    if (sum(j == seq(from=11,to=num.pages,by=10))>0) {
      next.page<-remDr$findElement(using="css",value="a:nth-child(16) b")
      next.page$clickElement()
      
      # wait a bit after clicking
      Sys.sleep(0.5)
      
      #get the page html
      page_source<-remDr$getPageSource()
      
      # locations of finds
      gcs.next<-xml2::read_html(page_source[[1]]) %>% html_nodes(".Merge .small") %>%
        html_text()
      
      # cache title
      gc.title.next<-xml2::read_html(page_source[[1]]) %>% html_nodes(".lnk span")%>%
        html_text()
      
      #replace multiple whitespaces with single space
      clean.gcs.next <- gsub('\\s+',' ',gcs.next)
      
      #trim spaces from ends of elements
      clean.gcs.next <- trimws(clean.gcs.next)
      
      # cache visit date
      visit.date.next<-xml2::read_html(page_source[[1]]) %>% html_nodes(".AlignCenter~ td+ td .small")%>%
        html_text()
      
      next.pages<-rbind(next.pages,cbind(gc.title.next,clean.gcs.next,visit.date.next))
      } else {
      next.page<-remDr$findElement(using="css",value=paste("#ctl00_ContentBody_pgrTop_lbGoToPage_",j,sep=""))
      next.page$clickElement()
      
      # alternative?
      # remDr$executeScript(paste("__doPostBack('ctl00$ContentBody$pgrTop$lbGoToPage_",j,"','')",sep=""), args = list())

      
      # wait a bit before clicking
      Sys.sleep(0.5)
      
      #get the page html
      page_source<-remDr$getPageSource()
      
      # locations of finds
      gcs.next<-xml2::read_html(page_source[[1]]) %>% html_nodes(".Merge .small") %>%
        html_text()
      
      # cache title
      gc.title.next<-xml2::read_html(page_source[[1]]) %>% html_nodes(".lnk span")%>%
        html_text()
      
      #replace multiple whitespaces with single space
      clean.gcs.next <- gsub('\\s+',' ',gcs.next)
      
      #trim spaces from ends of elements
      clean.gcs.next <- trimws(clean.gcs.next)
      
      # cache visit date
      visit.date.next<-xml2::read_html(page_source[[1]]) %>% html_nodes(".AlignCenter~ td+ td .small")%>%
        html_text()
      
      next.pages<-rbind(next.pages,cbind(gc.title.next,clean.gcs.next,visit.date.next))
      }
  } 
  page_results <- cbind(i,num.finds,rbind(first.page,next.pages))
  all_results<-rbind.data.frame(all_results,page_results) 
}


write.csv(all_results,"user.history.csv",quote = F)

all_results[grep("Canada",all_results$clean.gcs.first,invert = T),]
