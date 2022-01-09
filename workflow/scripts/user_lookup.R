# geocache user lookup

rev_lookup <- function (...) {
  scraped$users.url <- htmltools::urlEncodePath(as.character(scraped$users))
  s <- 1
  user.list <- unique(scraped$users.url)
  # why?
  user.list <- user.list[4:22749]
  for (i in user.list) {
    page_results <- NULL
    remDr$navigate(url = paste("https://www.geocaching.com/seek/nearest.aspx?ul=", 
                               i, sep = ""))
    Sys.sleep(0.5)
    page_source <- remDr$getPageSource()
    num.finds <- xml2::read_html(page_source[[1]]) %>% html_nodes(".NoBottomSpacing span b:nth-child(1)") %>% 
      html_text()
    gc.title.first <- xml2::read_html(page_source[[1]]) %>% 
      html_nodes(".lnk span") %>% html_text()
    gcs.first <- xml2::read_html(page_source[[1]]) %>% html_nodes(".Merge .small") %>% 
      html_text()
    clean.gcs.first <- gsub("\\s+", " ", gcs.first)
    clean.gcs.first <- trimws(clean.gcs.first)
    visit.date <- xml2::read_html(page_source[[1]]) %>% html_nodes(".AlignCenter~ td+ td .small") %>% 
      html_text()
    first.page <- cbind(gc.title.first, clean.gcs.first, 
                        visit.date)
    num.pages <- round((as.numeric(num.finds)/20), 0)
    next.pages <- NULL
    for (j in 2:num.pages) {
      if (sum(j == seq(from = 11, to = num.pages, by = 10)) > 
          0) {
        next.page <- remDr$findElement(using = "css", 
                                       value = "a:nth-child(16) b")
        next.page$clickElement()
        Sys.sleep(0.5)
        page_source <- remDr$getPageSource()
        gcs.next <- xml2::read_html(page_source[[1]]) %>% 
          html_nodes(".Merge .small") %>% html_text()
        gc.title.next <- xml2::read_html(page_source[[1]]) %>% 
          html_nodes(".lnk span") %>% html_text()
        clean.gcs.next <- gsub("\\s+", " ", gcs.next)
        clean.gcs.next <- trimws(clean.gcs.next)
        visit.date.next <- xml2::read_html(page_source[[1]]) %>% 
          html_nodes(".AlignCenter~ td+ td .small") %>% 
          html_text()
        next.pages <- rbind(next.pages, cbind(gc.title.next, 
                                              clean.gcs.next, visit.date.next))
      }
      else {
        next.page <- remDr$findElement(using = "css", 
                                       value = paste("#ctl00_ContentBody_pgrTop_lbGoToPage_", 
                                                     j, sep = ""))
        next.page$clickElement()
        Sys.sleep(0.5)
        page_source <- remDr$getPageSource()
        gcs.next <- xml2::read_html(page_source[[1]]) %>% 
          html_nodes(".Merge .small") %>% html_text()
        gc.title.next <- xml2::read_html(page_source[[1]]) %>% 
          html_nodes(".lnk span") %>% html_text()
        clean.gcs.next <- gsub("\\s+", " ", gcs.next)
        clean.gcs.next <- trimws(clean.gcs.next)
        visit.date.next <- xml2::read_html(page_source[[1]]) %>% 
          html_nodes(".AlignCenter~ td+ td .small") %>% 
          html_text()
        next.pages <- rbind(next.pages, cbind(gc.title.next, 
                                              clean.gcs.next, visit.date.next))
      }
    }
    page_results <- cbind(i, num.finds, rbind(first.page, 
                                              next.pages))
    user_history <- rbind.data.frame(user_history, page_results)
  }
  write.csv(user_history, "user.history.csv", quote = F)
  user_history[grep("Canada", user_history$clean.gcs.first, invert = T), 
              ]
  list(time = Sys.time(), tempfile = tempfile())
}
