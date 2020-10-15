filter.description<-function(dat){
  
  rD <- rsDriver(port = 4444L, browser = 'firefox')
  remDr <- rD$client
  
  # set timeout
  remDr$setTimeout(type = 'page load', milliseconds = 20000)
  
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
  keywords <- list(c("flashlight","cave","mine","bat","ore","mineral"), # English
  c("фенерче", "пещера", "рудна мина", "прилеп", "руда", "минерал"), # Bulgarian
  c("lommelygte", "hule", "malmminer", "flagermus", "malm", "mineral"), # Danish
  c("taskulamp", "koobas", "maagikaevandus", "nahkhiir", "maak", "mineraal"), # Estonian
  c("lampe de poche", "grotte", "mine de minerai", "chauve-souris", "minerai", "minéral"), #French
  c("ficklampa", "grotta", "malmgruva", "fladdermus", "malm", "mineral"), # Swedish
  c("baterka", "jaskyňa", "rudná baňa", "netopier", "ruda", "minerál"), # Slovak
  c("lanterna", "pestera", "mina de minereu", "liliac", "minereu", "mineral"), # Romanian
  c("lommelykt", "hule", "malmgruve", "flaggermus", "malm", "mineral"), #Norwegian
  c("батеријска лампа", "пећина", "рудник руде", "слепи миш", "руда", "минерал"), # Serbian
  c("linterna", "cueva", "mina de mineral", "murciélago", "mineral", "mineral"), #Spanish
  c("el feneri", "mağara", "cevher madeni", "yarasa", "cevher", "mineral"), # Turkish
  c("zaklamp", "grot", "ertsmijn", "vleermuis", "erts", "mineraal"), # Dutch
  c("Taschenlampe", "Höhle", "Erzmine", "Fledermaus", "Erz", "Mineral"), # German
  c("ліхтарик", "печера", "рудна шахта", "кажан", "руда", "мінерал"), # Ukrainian
  c("svítilna", "jeskyně", "rudný důl", "netopýr", "ruda", "minerál"), #Czech
  c("taskulamppu", "luola", "malmikaivos", "lepakko", "malmi", "mineraali"), #Finnish
  c("φακός", "σπήλαιο", "ορυχείο μεταλλεύματος", "ρόπαλο", "μεταλλεύματα", "ορυκτά"), #Greek
  c("torcia", "grotta", "miniera di minerale", "pipistrello", "minerale", "minerale"), # Italian
  c("lukturītis", "ala", "rūdas raktuves", "sikspārnis", "rūdas", "minerāls"), # Latvian
  c("latarka", "jaskinia", "kopalnia rudy", "nietoperz", "ruda", "minerał"), # Polish
  c("фонарик", "пещера", "рудник", "летучая мышь", "руда", "минерал"), # Russian
  c("svetilka", "jama", "rudnik rude", "netopir", "ruda", "mineral")) # Slovenian
  
  keywords<-setNames(keywords,c("English","Bulgarian","Danish","Estonian","Frensh","Swedish","Slovak","Romania","Norwegian","Serbian","Spanish","Turkish","Dutch","German","Ukranian","Czech","Finnish","Greek","Italian","Latvian","Polish","Russian","Slovenian"))
  full_set<-NULL
  
  for (i in as.character(dat$url)) {
    # navigate to page i
    remDr$navigate(url = i)
    
    # wait for page to finish loading. Is this necessary?
    Sys.sleep(0.5)
    
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
  }
  
  #
  long_full_set <- full_set %>% as.data.frame(full_set) %>%
    pivot_longer(cols = -url,names_to = "keyword") %>% 
    mutate(value=as.integer(levels(value))[value]) %>% mutate(presence = ifelse(value > 0 ,1,0))
  
  wide_full_set<-long_full_set %>% 
    pivot_wider(names_from = keyword,values_from = presence,id_cols = url,values_fn = list(presence = min)) %>% 
    # mutate_all(as.integer()) %>%
    as.data.frame()
  
  png("figures/keyword_combinations_eu.png",res = 300,height = 800,width = 1000,units = "px")
  UpSetR::upset(wide_full_set,nintersects = NA,nsets = length(keywords),order.by = "freq")
  dev.off()

##### Filtering
filtered_set<-subset(wide_full_set,  wide_full_set$flashlight > 0 & wide_full_set$cave > 0 | 
                       wide_full_set$ore > 0 & wide_full_set$mine > 0 | 
                       wide_full_set$mineral > 0 & wide_full_set$mine > 0) 

filtered_dat<-na.omit(dat[match(filtered_set$url,dat$url),])

write.csv(filtered_dat,"data/gc-list-eu-filtered.csv")
}