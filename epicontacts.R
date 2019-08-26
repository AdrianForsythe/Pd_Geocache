library(linelist)
library(outbreaks)
library(epicontacts)

# Each row of the line list should represent unique observations of cases
# Each row of the contact list should represent unique pairs of contacts

# linelist = 
source("coordinate-overlap.R")
geocache.presence.df$i<-gsub(pattern = "_(.*)","",geocache.presence.df$i)
geocache.presence.df$i<-gsub(pattern = "https://www.geocaching.com/geocache/","",geocache.presence.df$i)
clean.df<-clean_data(geocache.presence.df)

# contacts = geocache records?

epi<-make_epicontacts(clean.df,all_results_merge,id = "i", from = "GC", to = "users",directed = FALSE)
epi["province.state"=="New Brunswick","contacts"]
get_pairwise(epi,"county")
