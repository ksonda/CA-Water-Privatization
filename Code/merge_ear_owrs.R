summary <- read.csv("Data/OWRS/summary_table.csv", as.is = TRUE)
deliveries <- read.csv("Data/EAR/EAR 2013-2016 DELIVERIES FINAL 06-22-2018.csv", as.is = TRUE)
production1 <- read.csv("Data/EAR/EAR 2013-2016 PRODUCTION FINAL 06-22-2018_1.csv", as.is=TRUE)
production2 <- read.csv("Data/EAR/EAR 2013-2016 PRODUCTION FINAL 06-22-2018_2.csv", as.is=TRUE)

production_all <- rbind(production2, production2)
production_all$PWSID<- as.character(production_all$PWSID)
production_all$PWSID<- gsub(" ", "", production_all$PWSID)

colnames(summary)[2] <- "PWSID"

deliveries$Date1 <- mdy(deliveries$Date)
production_all$Date1 <- mdy(production_all$Date)

summary$Year <- strftime(as.character(summary$effective_date) ,"%y")
deliveries$Year <- strftime(as.character(deliveries$Date1) ,"%y")

production_all$Year <- strftime(as.character(production_all$Date1) ,"%y")

summary_deliveries <- left_join(summary, deliveries, by=c("Year", "PWSID"))
summary_deliveries_production <- left_join(summary_deliveries, production_all, intersect(colnames(summary_deliveries), colnames(production_all)))

saveRDS(summary_deliveries_production, "Data/summary_deliveries_production.rds")

