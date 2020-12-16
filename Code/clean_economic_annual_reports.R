
#---------------------------------------------------------------------------------------------------------------
#---------- This code clean the economic annual reports
#---------------------------------------------------------------------------------------------------------------

rm(list = ls()) #cleaning workspace, rm for remove
#dev.off() #clearing the plots
getwd()

my_packages <- c("rgeos","raster","tidyverse","magrittr","haven","readxl","tools","usethis","RColorBrewer","ggrepel","sf","tictoc","lubridate","pastecs")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select


#------------------------------------------- Generate variable list ------------------------------------------------------------

#audits <- read_csv("Data/Raw/Audit_losses/water_audit_data_28042019.csv") #conv_28042019

ea_reports <- read_excel("Data/Raw/Economic_annual_reports/EAR2018LWS-waterloss-v51419v2.xlsx") #conv_28042019
glimpse(ea_reports)

ea_reports %<>%
  rename(Water_System_Name = `Water System Name`) %>%
  mutate(Water_System_Name = str_to_title(Water_System_Name)) %>%
  mutate(Water_System_Name = str_remove(Water_System_Name,","))

glimpse(ea_reports)         

saveRDS(ea_reports,"Workspace/cleaned_ea_reports.rds")
         
#var_ear <- names(ea_reports) %>%
 # as_tibble()
#write_csv(var_ear,"Data/Processed/list_variables_ear.csv")


