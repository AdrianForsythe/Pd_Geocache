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
  keywords<-as.data.frame(keywords)

  gc_filtered_dat = na.omit(read.csv(data_path,header=T,strip.white = T))

  full_set<-gc_filtered_dat %>%
  group_by(GC) %>%
  mutate(flashlight=ifelse(grepl(paste(as.vector(keywords[,1]), collapse = "|"),Description,ignore.case = TRUE),1,0),
         cave=ifelse(grepl(paste(as.vector(keywords[,2]), collapse = "|"),Description,ignore.case = TRUE),1,0),
         mine=ifelse(grepl(paste(as.vector(keywords[,3]), collapse = "|"),Description,ignore.case = TRUE),1,0),
         bat=ifelse(grepl(paste(as.vector(keywords[,4]), collapse = "|"),Description,ignore.case = TRUE),1,0),
         ore=ifelse(grepl(paste(as.vector(keywords[,5]), collapse = "|"),Description,ignore.case = TRUE),1,0),
         mineral=ifelse(grepl(paste(as.vector(keywords[,6]), collapse = "|"),Description,ignore.case = TRUE),1,0))


  #png(keyword_plot,res = 300,height = 800,width = 1000,units = "px")
  #UpSetR::upset(full_set %>% select(-lon,-lat,-type,-Description),nintersects = NA,nsets = nrow(keywords),order.by = "freq")
  #dev.off()

##### Filtering
filtered_set<- full_set %>% filter(flashlight > 0 & cave > 0 | ore > 0 & mine > 0 | mineral > 0 & mine > 0)

filtered_dat<- gc_filtered_dat %>% filter(GC %in% filtered_set$GC)

write.csv(filtered_dat,out_path,quote=FALSE,row.names=FALSE)
}

filter.description(snakemake@input[[1]],snakemake@output[[1]],snakemake@output[[2]])
