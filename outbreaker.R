library(dplyr)
library(lubridate)
library(outbreaker2)
library(EpiEstim)
library(ape)
library(data.table)

#### example ####
col <- "#6666cc"
fake_outbreak$dna

plot(fake_outbreak$w, type = "h", xlim = c(0, 5), 
     lwd = 30, col = col, lend = 2, 
     xlab = "Days after infection", 
     ylab = "p(new case)", 
     main = "Generation time distribution")

args(outbreaker)

dna <- fake_outbreak$dna
dates <- fake_outbreak$sample
ctd <- fake_outbreak$ctd
w <- fake_outbreak$w
data <- outbreaker_data(dna = dna, dates = dates, ctd = ctd, w_dens = w)

## we set the seed to ensure results won't change
set.seed(1)

res <- outbreaker(data = data)

plot(res)
#######


FillIn <- function(D1, D2, Var1, Var2 = NULL, KeyVar = c("iso2c", "year"))
{
  # Give Var2 the same name as var1 if Var2 is NULL
  if (is.null(Var2)){
    Var2 <- Var1
  } else {
    Var2 <- Var2
  }
  
  # Give var a generic name
  names(D1)[match(Var1, names(D1))] <- "VarGen"
  names(D2)[match(Var2, names(D2))] <- "VarGen.1"
  
  # Convert data frames to data.table type objects
  D1Temp <- data.table::data.table(D1, key = KeyVar)
  D2Temp <- data.table::data.table(D2, key = KeyVar)
  
  # Merge data.tables
  OutDT <- D2Temp[D1Temp]
  
  # Tell the user how many values will be filled in
  SubNA <- OutDT[, list(VarGen, VarGen.1)]
  SubNA <- subset(SubNA, is.na(VarGen) & !is.na(VarGen.1))
  print(paste(nrow(SubNA), "NAs were replaced."))
  
  # Fill in missing values from D1 with values from D2
  OutDT <- OutDT[is.na(VarGen), VarGen := VarGen.1]
  
  # Convert back to data frame
  OutDF <- data.frame(OutDT)
  
  # # Tell the user what the correlation coefficient is between the variables
  # SubNoNA <- subset(OutDF, !is.na(VarGen) & !is.na(VarGen.1))
  # HowMany <- nrow(SubNoNA)
  # CORR <- cor(SubNoNA$VarGen, SubNoNA$VarGen.1, use = "complete.obs")
  # print(paste("The correlation between", Var1, "and", Var2, "is", round(CORR, digits = 3), "based on", HowMany, "shared observations." ))
  
  # Remove uncombined variable and return main variable's name
  names(OutDF)[match("VarGen", names(OutDF))] <- Var1
  Keepers <- setdiff(names(OutDF), "VarGen.1")
  OutDF <- OutDF[, Keepers]
  OutDF
}

#  input should be a list containing:
# dat, a simulated outbreak (obtained bysimOutbreak)
# a generationtime distribution (w)
# collection dates for the DNA sequences (collecDates)


### Generation-time distribution
## Computing the discrete serial interval of influenza
clean.df <- read.csv("clean-coords.csv",header = T,na.strings = " ")

# clean space from date
clean.df$SAMPLEDATE<-gsub(pattern = " 06/09/2018",replacement = "06/09/2018",clean.df$SAMPLEDATE)

#### temporary assumptions ####
clean.df$SAMPLEDATE<-mdy(gsub(pattern = "/00/",replacement = "/01/",clean.df$SAMPLEDATE))
clean.df$WNS_MAP_YR<-ymd(gsub("-.+","/01/01",clean.df$WNS_MAP_YR))

# clean.fill<-FillIn(D1 = clean.df,D2 = clean.df,Var1 = "SAMPLEDATE",Var2 = "WNS_MAP_YR",KeyVar = "X")

incidence<-clean.df %>% 
  # filter(WNS_MAP_YR != "UNKNOWN") %>%
  group_by(WNS_MAP_YR) %>% 
  summarise(I = n())

colnames(incidence) <- c("dates","I")

# diff time
incidence$dates<-(as.numeric(difftime(min(incidence$dates),incidence$dates,units = "days"))*-1)

estR<-estimate_R(incid = incidence,method = "uncertain_si",
# this is arbitrary
           config = make_config(list(
             mean_si = 2.6, std_mean_si = 1,
             min_mean_si = 1, max_mean_si = 4.2,
             std_si = 1.5, std_std_si = 0.5,
             min_std_si = 0.5, max_std_si = 2.5,
             n1 = 100, n2 = 100)))

### DNA sequences
read.dna()

fakeOutbreak$dat$call
