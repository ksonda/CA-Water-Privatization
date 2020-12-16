
#---------------------------------------------------------------------------------------------------------------
#---------- This code makes a few plots based on cleaned summry_table
#---------------------------------------------------------------------------------------------------------------

rm(list = ls()) #cleaning workspace, rm for remove
#dev.off() #clearing the plots
getwd()

my_packages <- c("rgeos","raster","tidyverse","magrittr","haven","readxl","tools","usethis","RColorBrewer","ggridges","sf","tictoc","lubridate","viridis")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select

#All variables capitalized ; values str_to_title ; remove accents from all variables and values

#-----------------------------------------------------------------------------------------------------------------

summary_table <- read_csv("Data/OWRS/summary_table_cleaned.csv")

glimpse(summary_table)

summary_table %<>%
  mutate(Tier_number = as.character(Tier_number))

ggplot(summary_table, aes(x = Tier_volume, y = Tier_number, fill = Tier_number)) +
  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) +
  theme_ridges() +
  scale_x_continuous(limits=c(0, 150), expand = c(0.01, 0)) +
  #scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_brewer(palette = "YlOrRd")
  #scale_y_reverse(breaks=c(2000, 1980, 1960, 1940, 1920, 1900), expand = c(0.01, 0))


ggplot(summary_table, aes(x = Tier_price, y = Tier_number, fill=Tier_number)) +
  geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) +
  theme_ridges() +
  scale_x_continuous(limits=c(0, 20), expand = c(0.01, 0)) +
  scale_fill_brewer(palette = "YlOrRd")
#scale_y_reverse(breaks=c(2000, 1980, 1960, 1940, 1920, 1900), expand = c(0.01, 0))