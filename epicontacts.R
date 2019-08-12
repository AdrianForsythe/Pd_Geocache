library(outbreaks)
library(epicontacts)

# Each row of the line list should represent unique observations of cases
# Each row of the contact list should represent unique pairs of contacts

str(mers_korea_2015)
merskor15 <- make_epicontacts(linelist = mers_korea_2015$linelist,
                              contacts = mers_korea_2015$contacts, 
                              directed = FALSE)
