#########################################
# Firestation Project: Scrape and Clean #
# Wildfire Ignition Cause Prediction    #
#########################################
library(maps)
library(RSQLite)
library(dbplyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyverse)
library(xts)
library(ggfortify)
library(maps)
library(mapdata)
library(leaflet)
library(lubridate)
library(rvest)
####
## Load and Clean Wildfire Data 
## Wildfire Dataset Source: https://www.fs.usda.gov/rds/archive/Catalog/RDS-2013-0009.5
setwd("C:\\Users\\ejtur\\OneDrive\\Desktop\\Data\\projectCAP")
conn = dbConnect(SQLite(), 'FiresTo18.sqlite')
fires = tbl(conn, "Fires") %>% 
  collect() %>% 
  select("DISCOVERY_DATE", "DISCOVERY_DOY", "FIRE_YEAR", "NWCG_CAUSE_CLASSIFICATION", "NWCG_GENERAL_CAUSE", 
         "NWCG_CAUSE_AGE_CATEGORY", "CONT_DATE", "FIRE_SIZE", "FIRE_SIZE_CLASS", "LATITUDE", "LONGITUDE", "STATE", 
         "FIPS_CODE") 

dbDisconnect(conn)
colnames(fires) <- c("StartDate", "StartDOY", "Year", "CauseClass", "Cause", "CauseAgeCategory", "EndDate", "Size", "SizeClass",
         "LAT", "LON", "State", "FIPS")

fires$EndDate <- as.Date(mdy_hm(fires$EndDate)) 
fires$StartDate <- as.Date(mdy_hm(fires$StartDate))

fires$BurnTime <- as.numeric(fires$EndDate - fires$StartDate)
fires$Cause <- as.factor(fires$Cause)

## % of wildfires with unknown cause--------->>> for motivation
fires %>% group_by(Cause) %>% summarise(n = n()) %>% slice(6) %>% select(n)/nrow(fires)*100



######
## EDA
fires %>% 
  group_by(Year) %>%
  summarize(n_fires = n()) %>%
  ggplot(aes(x = Year, y = n_fires/1000)) + 
  geom_bar(stat = 'identity', fill = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, color = 'red') + 
  labs(x = '', y = '# of US wildfires (thousands)', title = 'US Wildfires by Year')

fires %>%
  group_by(Cause) %>%
  summarize(n_fires = n()/1000) %>%
  ggplot(aes(x = reorder(Cause, n_fires), y = n_fires)) +
  geom_bar(stat = 'identity', fill = 'red') + 
  coord_flip() + 
  labs(x = '', y = 'Number of Fires(Thousands)', title = 'Number of US Wildfires by Cause 1992 to 2018')

fires %>% ggplot(aes(factor(Cause), LAT, color=factor(Cause))) + 
  geom_violin(aes(fill = factor(Cause))) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Cause by Latitude") + xlab("Cause") + ylab("Latitude") +
  scale_x_discrete(labels = c('','','','','','','','','','','','',''))

fires %>% ggplot(aes(factor(Cause), Size, color=factor(Cause))) + 
  geom_violin(aes(fill = factor(Cause))) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Cause by Size") + xlab("Cause") + ylab("Size (acres)") +
  scale_x_discrete(labels = c('','','','','','','','','','','','',''))

fires %>% ggplot(aes(factor(Cause), Size, color=factor(Cause))) + 
  geom_violin(aes(fill = factor(Cause))) + ylim(0, 10) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Cause by Size 2 Acre Maximum") + xlab("Cause") + ylab("Size (acres)") +
  scale_x_discrete(labels = c('','','','','','','','','','','','',''))


fires %>% count(Cause)
cor(fires[, sapply(fires, is.numeric)])

par(mfrow = c(3, 4), mar = c(1, 2, 2, 1))
setz <- split(fires, factor(fires$Cause))
setz <- setz[-6]
l <- as.list(names(setz))

for(i in 1:12){
  plot(fires$LON, fires$LAT, ylim=c(min(fires$LAT),max(fires$LAT)), xlim=c(min(fires$LON),max(fires$LON)), 
       type = "n", xlab = "Latitude", ylab = "Longitude")
  maps :: map("world", add = TRUE, lwd = 2, col = "darkgreen")
  title(main = l[[i]], cex=2)
  points(setz[[i]]$LON, setz[[i]]$LAT, pch = 16, cex=0.2, col = "red")
}
#dev.off()


####
## WildFire Sample
fires <- fires %>% drop_na(EndDate)

twoK15fires <- fires[fires$StartDate > "2015-01-01", ] %>% 
  filter(Cause != "Missing data/not specified/undetermined")
samFire <- sample_n(twoK15fires, 20000, replace = FALSE)


######## 
#######   Adding in NOAA weather data
######
#####
####
##NOAA station Locations 
library(rnoaa)
?ghcnd_stations
ghcnd <- rnoaa::ghcnd_stations() 
## PRCP Stations
noaaPRCP <- ghcnd %>% filter(element == "PRCP", last_year >= 2019, first_year <= 1992)
noaaPRCP <-  noaaPRCP[!(is.na(noaaPRCP$state) | noaaPRCP$state==""), ] %>% 
  distinct(id, .keep_all = "TRUE")
## TMAX Stations
noaaTMAX <- ghcnd %>% filter(element == "TMAX", last_year >= 2019, first_year <= 1992)
noaaTMAX <-  noaaTMAX[!(is.na(noaaTMAX$state) | noaaTMAX$state==""), ] %>% 
  distinct(id, .keep_all = "TRUE")


####
## Find closest weather station with PRCP to each wildfire
library(data.table)
firescoorPRCP <- samFire[,c(10,11)]
distPRCP <- function(x){
  df1PRCP <- data.table((noaaPRCP$latitude - x[1])^2 + (noaaPRCP$longitude - x[2])^2)
  locPRCP <- which.min(as.vector(df1PRCP$V1))
  return(noaaPRCP$id[locPRCP])}

firestationPRCP <- apply(firescoorPRCP, 1, distPRCP)
samFire$NearStationPRCP <- firestationPRCP


####
## Find closest weather station with TMAX to each wildfire
firescoorTMAX <- samFire[,c(10,11)]
distTMAX <- function(x){
  df1TMAX <- data.table((noaaTMAX$latitude - x[1])^2 + (noaaTMAX$longitude - x[2])^2)
  locTMAX <- which.min(as.vector(df1TMAX$V1))
  return(noaaTMAX$id[locTMAX])}

firestationTMAX <- apply(firescoorTMAX, 1, distTMAX)
samFire$NearStationTMAX <- firestationTMAX



####
## Load desired weather data

NSPRCP <- samFire[,c(15,1)]
NSPRCP$Precip <- "na"
NSTMAX <- samFire[,c(16,1)]
NSTMAX$TempMax <- "na"
for (i in 1:20000){
  g <- ghcnd_search(NSPRCP$NearStationPRCP[i], var = "PRCP", date_min = NSPRCP$StartDate[i], date_max = NSPRCP$StartDate[i])
  t <- ghcnd_search(NSTMAX$NearStationTMAX[i], var = "TMAX", date_min = NSTMAX$StartDate[i], date_max = NSTMAX$StartDate[i])
  
  NSPRCP$Precip[i]<- ifelse( dim(g$prcp)[[1]]>0, g$prcp$prcp[[1]], "NA")
  NSTMAX$TempMax[i] <- ifelse( dim(t$tmax)[[1]]>0,t$tmax$tmax[[1]],"NA")
}

g$prcp
g$prcp$prcp[[1]]
NSPRCP
t$tmax
t$tmax$tmax[[1]]
NSTMAX

NSPRCP$X <- seq.int(nrow(NSPRCP))
NSTMAX$X <- seq.int(nrow(NSTMAX))
samFire$X <- seq.int(nrow(samFire))

samFireWeather <- right_join(samFire, NSPRCP, by = "X")
samFireWeather <- right_join(samFireWeather, NSTMAX, by = "X")
samFireWeather <- samFireWeather %>% select(-c(15,16,17,18,19,21,22))
write.csv(samFireWeather, "firestation20k.csv")

####
## Load Drought Data
## Source: https://droughtmonitor.unl.edu/DmData/DataTables.aspx
library(tidyverse)
setwd("C:\\Users\\ejtur\\OneDrive\\Desktop\\Ithaca College\\CAP2\\DroughtData")

files <- list.files("C:/Users/ejtur/OneDrive/Desktop/Ithaca College/CAP2/DroughtData")
states <- gsub(".csv", "", files)

for(i in 1:length(files)) {
  assign(paste0(states[i]), read.csv(paste0(files[i])))
}
Drought <- bind_rows(AK, AL, AR, AZ, CA, CO, CT, DC, DE, FL, GA, HI, IA, ID, IL, IN, KS, 
                     KY, LA, MA, MD, ME, MI, MN, MO, MS, MT, NC, ND, NE, NH, NJ, NM, NV, 
                     NY, OH, OK, OR, PA, PR, RI, SC, SD, TN, TX, UT, VA, VT, WA, WI, WV, 
                     WY)

Drought$Date <- as.Date(Drought$Week, format = "%m/%d/%y")
Drought$Week <- week(ymd(Drought$Date))
Drought$Year <- year(ymd(Drought$Date))

samFireWeather$Week <- week(ymd(samFireWeather$StartDate.x))

####
## Add Drought data to fires df
samFireTotal <- inner_join(samFireWeather, Drought, by=c("State", "Year", "Week"))




####
## Export
setwd("C:/Users/ejtur/OneDrive/Desktop/Data/projectCAP")
write.csv(samFireTotal, "firestationTotal.csv")

