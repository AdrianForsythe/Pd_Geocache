##### Sraping geocache records from a list of known cave/mine sites in North America

start_scrape<-function(dat){
  # connect to running server
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
  
  # generate list of urls
  # filtered_gc_dat$url <- paste("https://www.geocaching.com/geocache/",filtered_gc_dat$GC,sep = "")
  
  # the log page loads result dynamically, a la "infinite scrolling"
  # we already know the number of total results from these pages
  # and there are 25 results per page
  # so we known how many times we need to scroll to the end
  filtered_gc_dat$numpage <- round(as.numeric(filtered_gc_dat$numfinds)/25,digits = 0)
  
  ##### Start scraping
  # this takes a few hours to run!
  scrape_dat<-NULL
  for (i in as.character(filtered_gc_dat$url)) {
    # navigate to page i
    remDr$navigate(url = i)
    
    #scroll down j times, waiting for the page to load at each time
    for(j in 1:filtered_gc_dat[filtered_gc_dat$url==i,]$numpage){      
      remDr$executeScript(paste("scroll(0,",j*10000,");"))
      Sys.sleep(1.5)    
    }
    
    # get the page html
    page_source<-remDr$getPageSource()
    
    # parse it
    # for user names
    users<-xml2::read_html(page_source[[1]]) %>% html_nodes(".h5") %>%
      html_text()
    # log type
    status<-xml2::read_html(page_source[[1]]) %>% html_nodes(".LogType .log-meta") %>%
      html_text(trim = T)
    # date
    date<-xml2::read_html(page_source[[1]]) %>% html_nodes(".LogDate") %>%
      html_text()
    # and log text
    log<-xml2::read_html(page_source[[1]]) %>% html_nodes(".LogText") %>%
      html_text(trim = T)
    
    # discard empty entries
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
    
    ### Clean logs
    # replace multiple whitespaces with single space
    clean_log <- gsub('\\s+',' ',log)
    # trim spaces from ends of elements
    clean_log <- trimws(clean_log)
    
    ### Bind it all together
    page_results <- cbind.data.frame(users,status,date,clean_log,i)
    scrape_dat<-rbind(scrape_dat,page_results)
  }
  
  # save results
  write.table(scrape_dat,file = "data/gc-scrape-results.tab",row.names = F,col.names = T,quote = F,sep = "\t")
}