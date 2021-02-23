filter.description<-function(data_path,out_path,keyword_plot){
  #####
  require(tidyverse)
  require(UpSetR)
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

  gc_filtered_dat = na.omit(read.csv(data_path,header=T,strip.white = T))

  full_set<-NULL

  for (i in as.character(gc_filtered_dat$GC)) {
    column_matches<-NULL
    for (j in keywords) {
      s<-filter(gc_filtered_dat,GC==i)
      if(length(grep(paste(" ",j,sep=""),s$Description,ignore.case = TRUE,)>0)){
        matches<-grep(paste(" ",j,sep=""),s$Description,ignore.case = TRUE)
      } else {
        matches <- 0
      }
      column_matches<-cbind(column_matches,matches)
    }
    full_set<-rbind(full_set,cbind(column_matches,i))
    colnames(full_set) <- c(keywords,"GC")
  }

  long_full_set <- full_set %>% as.data.frame(full_set) %>%
    pivot_longer(cols = -GC,names_to = "keyword") %>%
    mutate(value=as.integer(levels(value))[value],
           presence = ifelse(value > 0 ,1,0))

  wide_full_set<-long_full_set %>%
    pivot_wider(names_from = keyword,values_from = presence,id_cols = GC,values_fn = list(presence = min)) %>%
    # mutate_all(as.integer()) %>%
    as.data.frame()

  png(keyword_plot,res = 300,height = 800,width = 1000,units = "px")
  UpSetR::upset(wide_full_set,nintersects = NA,nsets = length(keywords),order.by = "freq")
  dev.off()

##### Filtering
filtered_set<-subset(wide_full_set,  wide_full_set$flashlight > 0 & wide_full_set$cave > 0 |
                       wide_full_set$ore > 0 & wide_full_set$mine > 0 |
                       wide_full_set$mineral > 0 & wide_full_set$mine > 0)

filtered_dat<-na.omit(gc_filtered_dat[match(filtered_set$GC,gc_filtered_dat$GC),])

write.csv(filtered_dat,out_path)
}

filter.description(snakemake@input[[1]],snakemake@output[[1]],snakemake@output[[2]])
