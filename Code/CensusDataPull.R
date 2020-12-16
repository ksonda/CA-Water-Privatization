###
##last pull: sep 30 2019, lauren adams

setwd("C:/Users/leadams/Desktop/waterRates")
getwd()

install.packages("sf")
install.packages("tidycensus")

library(tidyverse)
options(tigris_use_cache = TRUE)

library("sf")
library("tidycensus")

#get hh size and income data
#using method: https://walkerke.github.io/tidycensus/articles/basic-usage.html

#######household size
#find variable for household size
hhsizefindvar = load_variables(year=2010, "sf1", cache = TRUE)
View(hhsizefindvar)
varssf1 = c("P037001")
#w geometry, geom output is a list, not sure if it is usable
hhsizegeo = get_decennial(geography = "county", state="CA", variables=varssf1,  
                       year=2010, sumfile="sf1", geometry=TRUE)
write.csv(hhsizegeo, "HouseholdSizebyCountywGeom.csv")
#without geometry
hhsize = get_decennial(geography = "county", state="CA", variables=varssf1,  
                       year=2010, sumfile="sf1", geometry=FALSE)
write.csv(hhsize, "HouseholdSizebyCounty.csv")

########income
#find variable for income
#v17 <- load_variables(geography="state", state="CA", year=2010, "sf3", cache = TRUE)
#View(v17)
#varssf3=c("HCT012001")

#dates do not match between hhsize and income
#moe is margin of error, see website for details: https://walkerke.github.io/tidycensus/articles/basic-usage.html
acsview <- load_variables(year=2017, "acs5", cache = TRUE)
View(acsview)
varsacs5=c("B19127_001")

#w geometry, geom output is a list, not sure if it is usable
incomegeo=get_acs(geography = "county", 
               variables = c(medincome = "B19013_001"), 
               state = "CA", geometry=TRUE)
write.csv(incomegeo, "IncomebyCountywGeo.csv")

#without geometry
income=get_acs(geography = "county", 
        variables = c(medincome = "B19013_001"), 
        state = "CA")
write.csv(income, "IncomebyCounty.csv")








