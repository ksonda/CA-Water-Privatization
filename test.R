library(tidyverse)
library(ggplot2)
library(sf)

load("Data/census_blockgroups.rds")

places <- st_read("places_income.gpkg")
load("Data/FinalData.rds")
pl.pws <- st_read("Data/pws.gpkg")
pl.pws <- filter(pl.pws,PWSID %in% pws.income2$PWSID)

places <- places %>% st_drop_geometry() %>%
  pivot_wider(id_cols=GEOID,names_from=variable,values_from=estimate)


places$perc50_pl <- places$median_income
places$perc20_pl <- places$QUL1 

pl.pws$GEOID <- gsub("https://geoconnex.us/ref/places/","",pl.pws$CITY_SERVED_uri)

pl_pws <- filter(pl.pws,!is.na(GEOID))

pl_pws <- left_join(pl_pws,places,by="GEOID")
pl_pws <- left_join(pl_pws,pws.income2,by="PWSID")

pl_pws$rat20 <- pl_pws$`20th Percentile`/pl_pws$perc20_pl
pl_pws$rat50 <- pl_pws$`50th Percentile`/pl_pws$perc50_pl

plot <- ggplot(data=pl_pws,aes(x=SYSTEM_SIZE,y=rat20)) + geom_dotplot(binaxis="y", stackdir="center")

pl_pws <- pl_pws %>% mutate(inc_bracket_boundary_20 = case_when(
  `20th Percentile` == 5000 ~ "1. $0-$9,999",
  `20th Percentile` == 12500 ~ "2. $10,000-$14,999",
  `20th Percentile` == 17500 ~ "3. $15,000-$19,999",
  `20th Percentile` == 22500 ~ "4. $20,000-$24,999",
  `20th Percentile` == 27500 ~ "5. $25,000-$29,999",
  `20th Percentile` == 32500 ~ "6. $30,000-$34,999",
  `20th Percentile` == 37500 ~ "7. $35,000-$39,999",
  `20th Percentile` == 42500 ~ "8. $40,000-$44,999",
  `20th Percentile` == 47500 ~ "9. $45,000-$49,999",
  `20th Percentile` == 55000 ~ "10. $50,000-$59,999",
  `20th Percentile` == 67500 ~ "11. $60,000-$74,999",
  `20th Percentile` == 87500 ~ "12. $75,000-$99,999",
  `20th Percentile` == 112500 ~ "13. $100,000-$124,999",
  `20th Percentile` == 137500 ~ "14. $125,000-$149,999",
  `20th Percentile` == 175000 ~ "15. $150,000-$199,999",
  `20th Percentile` == 225000 ~ "16. >$200,000"
  
),
inc_bracket_boundary_50 = case_when(
  `50th Percentile` == 5000 ~ "1. $0-$9,999",
  `50th Percentile` == 12500 ~ "2. $10,000-$14,999",
  `50th Percentile` == 17500 ~ "3. $15,000-$19,999",
  `50th Percentile` == 22500 ~ "4. $20,000-$24,999",
  `50th Percentile` == 27500 ~ "5. $25,000-$29,999",
  `50th Percentile` == 32500 ~ "6. $30,000-$34,999",
  `50th Percentile` == 37500 ~ "7. $35,000-$39,999",
  `50th Percentile` == 42500 ~ "8. $40,000-$44,999",
  `50th Percentile` == 47500 ~ "9. $45,000-$49,999",
  `50th Percentile` == 55000 ~ "10. $50,000-$59,999",
  `50th Percentile` == 67500 ~ "11. $60,000-$74,999",
  `50th Percentile` == 87500 ~ "12. $75,000-$99,999",
  `50th Percentile` == 112500 ~ "13. $100,000-$124,999",
  `50th Percentile` == 137500 ~ "14. $125,000-$149,999",
  `50th Percentile` == 175000 ~ "15. $150,000-$199,999",
  `50th Percentile` == 225000 ~ "16. >$200,000"
  
),
inc_bracket_boundary_numeric_20 = case_when(
  `20th Percentile` == 5000 ~ 1,
  `20th Percentile` == 12500 ~ 2,
  `20th Percentile` == 17500 ~ 3,
  `20th Percentile` == 22500 ~ 4,
  `20th Percentile` == 27500 ~ 5,
  `20th Percentile` == 32500 ~ 6,
  `20th Percentile` == 37500 ~ 7,
  `20th Percentile` == 42500 ~ 8,
  `20th Percentile` == 47500 ~ 9,
  `20th Percentile` == 55000 ~ 10,
  `20th Percentile` == 67500 ~ 11,
  `20th Percentile` == 87500 ~ 12,
  `20th Percentile` == 112500 ~ 13,
  `20th Percentile` == 137500 ~ 14,
  `20th Percentile` == 175000 ~ 15,
  `20th Percentile` == 225000 ~ 16
),
inc_bracket_boundary_numeric_50 = case_when(
  `50th Percentile` == 5000 ~ 1,
  `50th Percentile` == 12500 ~ 2,
  `50th Percentile` == 17500 ~ 3,
  `50th Percentile` == 22500 ~ 4,
  `50th Percentile` == 27500 ~ 5,
  `50th Percentile` == 32500 ~ 6,
  `50th Percentile` == 37500 ~ 7,
  `50th Percentile` == 42500 ~ 8,
  `50th Percentile` == 47500 ~ 9,
  `50th Percentile` == 55000 ~ 10,
  `50th Percentile` == 67500 ~ 11,
  `50th Percentile` == 87500 ~ 12,
  `50th Percentile` == 112500 ~ 13,
  `50th Percentile` == 137500 ~ 14,
  `50th Percentile` == 175000 ~ 15,
  `50th Percentile` == 225000 ~ 16
),
inc_bracket_place_numeric_20 = case_when(
  perc20_pl >= 200000  ~ 16,
  perc20_pl < 200000 & perc20_pl >= 150000 ~ 15,
  perc20_pl < 150000 & perc20_pl >= 125000 ~ 14,
  perc20_pl < 125000 & perc20_pl >= 100000 ~ 13,
  perc20_pl < 100000 & perc20_pl >= 75000 ~ 12,
  perc20_pl < 75000 & perc20_pl >= 60000 ~ 11,
  perc20_pl < 60000 & perc20_pl >= 50000 ~ 10,
  perc20_pl < 50000 & perc20_pl >= 45000 ~ 9,
  perc20_pl < 45000 & perc20_pl >= 40000 ~ 8,
  perc20_pl < 40000 & perc20_pl >= 35000 ~ 7,
  perc20_pl < 35000 & perc20_pl >= 30000 ~ 6,
  perc20_pl < 30000 & perc20_pl >= 25000 ~ 5,
  perc20_pl < 25000 & perc20_pl >= 20000 ~ 4,
  perc20_pl < 20000 & perc20_pl >= 15000 ~ 3,
  perc20_pl < 15000 & perc20_pl >= 10000 ~ 2,
  perc20_pl < 10000 ~ 1
),
inc_bracket_place_numeric_50 = case_when(
  perc50_pl >= 200000  ~ 16,
  perc50_pl < 200000 & perc50_pl >= 150000 ~ 15,
  perc50_pl < 150000 & perc50_pl >= 125000 ~ 14,
  perc50_pl < 125000 & perc50_pl >= 100000 ~ 13,
  perc50_pl < 100000 & perc50_pl >= 75000 ~ 12,
  perc50_pl < 75000 & perc50_pl >= 60000 ~ 11,
  perc50_pl < 60000 & perc50_pl >= 50000 ~ 10,
  perc50_pl < 50000 & perc50_pl >= 45000 ~ 9,
  perc50_pl < 45000 & perc50_pl >= 40000 ~ 8,
  perc50_pl < 40000 & perc50_pl >= 35000 ~ 7,
  perc50_pl < 35000 & perc50_pl >= 30000 ~ 6,
  perc50_pl < 30000 & perc50_pl >= 25000 ~ 5,
  perc50_pl < 25000 & perc50_pl >= 20000 ~ 4,
  perc50_pl < 20000 & perc50_pl >= 15000 ~ 3,
  perc50_pl < 15000 & perc50_pl >= 10000 ~ 2,
  perc50_pl < 10000 ~ 1
)
)

pl_pws2 <- st_drop_geometry(pl_pws)

ggplot(pl_pws2) +
  aes(x = inc_bracket_boundary_20, y = inc_bracket_place_numeric_20) +
  geom_boxplot(shape = "circle", 
               fill = "#112446") +
  theme_minimal()

