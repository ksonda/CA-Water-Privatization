
#---------------------------------------------------------------------------------------------------------------
#---------- This code loads Zillow Zip-code data on median house prices
#---------------------------------------------------------------------------------------------------------------

rm(list = ls()) #cleaning workspace, rm for remove
#dev.off() #clearing the plots
getwd()

my_packages <- c("rgeos","raster","tidyverse","magrittr","haven","readxl","tools","usethis","RColorBrewer","ggrepel","sf","tictoc","tidycensus","tigris")
#install.packages(my_packages, repos = "http://cran.rstudio.com")
lapply(my_packages, library, character.only = TRUE)

map<-purrr::map #For the nice iterations
select<-dplyr::select

options(tigris_use_cache = TRUE)

#---------------------------------------------------------------------------------------------------------------



#Getting zillow
zillow <- read_csv("Data/Raw/Zip_MedianValuePerSqft_AllHomes.csv")
glimpse(zillow)

zillow %<>%
  filter(State == "CA") %>%
  select(RegionID,RegionName,City,State,Metro,CountyName,`2018-01`) %>%
  rename(Median_price_201801 = `2018-01`) %>%
  mutate("RegionName" = as.numeric(RegionName)) 

summary(zillow$RegionName)





#Getting zip codes
ca_zip.sf <- get_acs(geography = "zcta",
                 variables = "B19013_001",  
                 geometry = TRUE)

glimpse(ca_zip.sf)

ca_zip.sf %<>%
  select(-NAME,-variable,-estimate,-moe) %>%
  mutate("GEOID" = str_sub(GEOID, 1,5)) %>%
  mutate("GEOID" = as.numeric(GEOID)) 

#For CA Only
ca_zip.sf %<>%
  filter(GEOID > 90000 & GEOID < 96162)


zillow_ca.sf <- ca_zip.sf %>%
  inner_join(zillow, by = c("GEOID" = "RegionName") ) 


#-------------------- Getting full CA counties for base map
ca.sf <- get_acs(geography = "county", 
                 state = "CA",
                 variables = "B19013_001",
                 geometry = TRUE)
glimpse(ca.sf)
ca.sf %<>%
  select(-variable,-estimate,-moe) 

ca.sf %<>%
  mutate("GEOID" = str_sub(GEOID,3,5)) %>%
  mutate("GEOID" = as.numeric(GEOID)) 





#--------------------------------------- Now getting zillow prices at the county level ---------------------------------------

zillow_county <- read_csv("Data/Raw/County_MedianValuePerSqft_AllHomes.csv") %>%
  filter(State == "CA") %>%
  select(RegionID,State,MunicipalCodeFIPS,`2018-01`) %>%
  rename(Median_price_201801 = `2018-01`) %>%
  mutate("MunicipalCodeFIPS" = as.numeric(MunicipalCodeFIPS)) 


glimpse(zillow_county)
summary(zillow_county$MunicipalCodeFIPS)
summary(ca.sf$GEOID)

zillow_ca_county.sf <- ca.sf %>%
  inner_join(zillow_county, by = c("GEOID" = "MunicipalCodeFIPS") ) 



summary(zillow$Median_price_201801)
summary(zillow_county$Median_price_201801)
#-------------------------------------------- Going for the plot --------------------------------------------

ggplot() +
  geom_sf(data = ca.sf, fill = NA) +
  geom_sf(data = zillow_ca.sf, aes(fill = Median_price_201801), size = 0.1) + # alpha = 0.1
  scale_fill_viridis_c(trans = "sqrt") + # alpha = .4
  #geom_point(aes(x = Longitude, 
  ##          data = geo.sf,
  #         color = "blue",
  #        size = 0.9) +  
  coord_sf(crs = "+proj=laea +lat_0=35 +lon_0=-115") +
  #guides(fill = FALSE) +
  ggtitle("Median Home Value Per Square Foot ($)") +
  labs(subtitle = "Data from Zillow, Zip-code level")+
  #colScale +
  theme(plot.title = element_text(hjust=0, size=18, margin=margin(b=10), family="Arial Narrow", face="bold"),
        plot.subtitle = element_text(hjust=0, size=12, margin=margin(b=15), family="Arial Narrow", face="plain"),
        plot.caption=element_text(hjust=1, size=9, margin=margin(t=10), family="Arial Narrow", face="italic"),
        text=element_text(family="Arial Narrow"),
        axis.text.x=element_text(size=11.5, margin=margin(t=0)),
        axis.text.y=element_text(size=11.5, margin=margin(r=0)),
        axis.title=element_text(size=9, family="Arial Narrow"),
        axis.title.x=element_text(size="Arial Narrow", family="Arial Narrow", face="plain"), #hjust=xj,
        axis.title.y=element_text(size="Arial Narrow", family="Arial Narrow", face="plain"), #hjust=yj, 
        axis.title.y.right=element_text(size="Arial Narrow", angle=90, family="Arial Narrow", face="plain"), #hjust=yj,
        strip.text=element_text(hjust=0, size=12, face="plain", family="Arial Narrow"),
        plot.margin=margin(30, 30, 30, 30),
        #plot.margin = unit(c(0.05,0.05,0.05,0.05), "inches"),
        legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        #legend.position="bottom",
        #legend.box = "horizontal",
        panel.background = element_rect(fill = "white"),
        panel.grid=element_line(color="#cccccc", size=0.2),
        panel.grid.major=element_line(color="#cccccc", size=0.2),
        panel.grid.minor=element_line(color="#cccccc", size=0.15))
ggsave("Figures/map_zillow_medianprice_zipcode.png", width = 7, height = 7*1.2)





#-------------- County level plot

ggplot() +
  geom_sf(data = ca.sf, fill = NA) +
  geom_sf(data = zillow_ca_county.sf, aes(fill = Median_price_201801), size = 0.1) + # alpha = 0.1
  scale_fill_viridis_c(trans = "sqrt") + # alpha = .4
  #geom_point(aes(x = Longitude, 
  ##          data = geo.sf,
  #         color = "blue",
  #        size = 0.9) +  
  coord_sf(crs = "+proj=laea +lat_0=35 +lon_0=-115") +
  #guides(fill = FALSE) +
  ggtitle("Median Home Value Per Square Foot ($)") +
  labs(subtitle = "Data from Zillow, County level")+
  #colScale +
  theme(plot.title = element_text(hjust=0, size=18, margin=margin(b=10), family="Arial Narrow", face="bold"),
        plot.subtitle = element_text(hjust=0, size=12, margin=margin(b=15), family="Arial Narrow", face="plain"),
        plot.caption=element_text(hjust=1, size=9, margin=margin(t=10), family="Arial Narrow", face="italic"),
        text=element_text(family="Arial Narrow"),
        axis.text.x=element_text(size=11.5, margin=margin(t=0)),
        axis.text.y=element_text(size=11.5, margin=margin(r=0)),
        axis.title=element_text(size=9, family="Arial Narrow"),
        axis.title.x=element_text(size="Arial Narrow", family="Arial Narrow", face="plain"), #hjust=xj,
        axis.title.y=element_text(size="Arial Narrow", family="Arial Narrow", face="plain"), #hjust=yj, 
        axis.title.y.right=element_text(size="Arial Narrow", angle=90, family="Arial Narrow", face="plain"), #hjust=yj,
        strip.text=element_text(hjust=0, size=12, face="plain", family="Arial Narrow"),
        plot.margin=margin(30, 30, 30, 30),
        #plot.margin = unit(c(0.05,0.05,0.05,0.05), "inches"),
        legend.title=element_text(size=12),
        legend.text=element_text(size=10),
        #legend.position="bottom",
        #legend.box = "horizontal",
        panel.background = element_rect(fill = "white"),
        panel.grid=element_line(color="#cccccc", size=0.2),
        panel.grid.major=element_line(color="#cccccc", size=0.2),
        panel.grid.minor=element_line(color="#cccccc", size=0.15))
ggsave("Figures/map_zillow_medianprice_county.png", width = 7, height = 7*1.2)





