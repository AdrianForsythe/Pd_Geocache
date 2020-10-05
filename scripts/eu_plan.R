##### Drake Plan
library(drake)
plan <- drake_plan(
  
  # don't forget!
  # sudo docker run -d -p 4445:4444 --shm-size 2g selenium/standalone-firefox
  
  source("scripts/packages.R"),
  
  ### Start with raw list
  # Read in GC list
  gc_dat = read.csv("data/gc-list-eu-unfiltered.csv",header=T,row.names = NULL,strip.white = T,fill = T,sep = ",",na.strings = "",quote = "",comment.char = ""),
  
  # generate url for scraping
  gc_dat$url<-paste("https://www.geocaching.com/geocache/",gc_dat$GC,"_",gsub(" ","-",tolower(gsub("[^[:alnum:] ]", "", gc_dat$title))),sep=""),
  
  # Take a sample of the description sections from GC pages
  source("scripts/sample_eu_descriptions.R"),
  sampled = sample.description(gc_dat),
  
  # clean the list of GC sites based on sampling keywords from descriptions
  source("scripts/filter_eu_descriptions.R"),
  filtered = filter_eu_description(gc_dat),
  gc_filtered_dat = na.omit(read.csv("data/gc-list-eu-filtered.csv",header=T)),
)