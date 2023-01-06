######################################
# Reference: https://www.fs.usda.gov/rds/archive/Catalog/RDS-2013-0009.5
######################################
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
## Load and clean Wildfire Data 
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
setz = split(fires, factor(fires$Cause))

?title
plot(fires$LON, fires$LAT, ylim=c(min(fires$LAT),max(fires$LAT)), xlim=c(min(fires$LON),max(fires$LON)), 
     type = "n", xlab = "Latitude", ylab = "Longitude")
maps :: map("world", add = TRUE, lwd = 2, col = "darkgreen")
title(main = "Missing", cex=2)
points(setz$`Missing data/not specified/undetermined`$LON, setz$`Missing data/not specified/undetermined`$LAT, pch = 16, cex=0.2)

plot(fires$LON, fires$LAT, ylim=c(min(fires$LAT),max(fires$LAT)), xlim=c(min(fires$LON),max(fires$LON)), 
     type = "n", xlab = "Latitude", ylab = "Longitude")
maps :: map("world", add = TRUE, lwd = 2, col = "darkgreen")
title(main = "Debris and Open Burning", cex=2)
points(setz$`Debris and open burning`$LON, setz$`Debris and open burning`$LAT, pch = 16, cex=0.2, col = "red")

plot(fires$LON, fires$LAT, ylim=c(min(fires$LAT),max(fires$LAT)), xlim=c(min(fires$LON),max(fires$LON)), 
     type = "n", xlab = "Latitude", ylab = "Longitude")
maps :: map("world", add = TRUE, lwd = 2, col = "darkgreen")
title(main = "Equipment and Vehicle use", cex=2)
points(setz$`Equipment and vehicle use`$LON, setz$`Equipment and vehicle use`$LAT, pch = 16, cex=0.2, col = "red")

plot(fires$LON, fires$LAT, ylim=c(min(fires$LAT),max(fires$LAT)), xlim=c(min(fires$LON),max(fires$LON)), 
     type = "n", xlab = "Latitude", ylab = "Longitude")
maps :: map("world", add = TRUE, lwd = 2, col = "darkgreen")
title(main = "Firearms and Explosives Use", cex=2)
points(setz$`Firearms and explosives use`$LON, setz$`Firearms and explosives use`$LAT, pch = 16, cex=0.2, col = "red")

plot(fires$LON, fires$LAT, ylim=c(min(fires$LAT),max(fires$LAT)), xlim=c(min(fires$LON),max(fires$LON)), 
     type = "n", xlab = "Latitude", ylab = "Longitude")
maps :: map("world", add = TRUE, lwd = 2, col = "darkgreen")
title(main = "Fireworks", cex=2)
points(setz$Fireworks$LON, setz$Fireworks$LAT, pch = 16, cex=0.2, col = "red")

plot(fires$LON, fires$LAT, ylim=c(min(fires$LAT),max(fires$LAT)), xlim=c(min(fires$LON),max(fires$LON)), 
     type = "n", xlab = "Latitude", ylab = "Longitude")
maps :: map("world", add = TRUE, lwd = 2, col = "darkgreen")
title(main = "Misuse of Fire by a Minor", cex=2)
points(setz$`Misuse of fire by a minor`$LON, setz$`Misuse of fire by a minor`$LAT, pch = 16, cex=0.2, col = "red")

plot(fires$LON, fires$LAT, ylim=c(min(fires$LAT),max(fires$LAT)), xlim=c(min(fires$LON),max(fires$LON)), 
     type = "n", xlab = "Latitude", ylab = "Longitude")
maps :: map("world", add = TRUE, lwd = 2, col = "darkgreen")
title(main = "Natural", cex=2)
points(setz$Natural$LON, setz$Natural$LAT, pch = 16, cex=0.2, col = "red")

plot(fires$LON, fires$LAT, ylim=c(min(fires$LAT),max(fires$LAT)), xlim=c(min(fires$LON),max(fires$LON)), 
     type = "n", xlab = "Latitude", ylab = "Longitude")
maps :: map("world", add = TRUE, lwd = 2, col = "darkgreen")
title(main = "Other", cex=2)
points(setz$`Other causes`$LON, setz$`Other causes`$LAT, pch = 16, cex=0.2, col = "red")

plot(fires$LON, fires$LAT, ylim=c(min(fires$LAT),max(fires$LAT)), xlim=c(min(fires$LON),max(fires$LON)), 
     type = "n", xlab = "Latitude", ylab = "Longitude")
maps :: map("world", add = TRUE, lwd = 2, col = "darkgreen")
title(main = "Power Generation", cex=2)
points(setz$`Power generation/transmission/distribution`$LON, setz$`Power generation/transmission/distribution`$LAT, pch = 16, cex=0.2, col = "red")

plot(fires$LON, fires$LAT, ylim=c(min(fires$LAT),max(fires$LAT)), xlim=c(min(fires$LON),max(fires$LON)), 
     type = "n", xlab = "Latitude", ylab = "Longitude")
maps :: map("world", add = TRUE, lwd = 2, col = "darkgreen")
title(main = "Railroads", cex=2)
points(setz$`Railroad operations and maintenance`$LON, setz$`Railroad operations and maintenance`$LAT, pch = 16, cex=0.2, col = "red")

plot(fires$LON, fires$LAT, ylim=c(min(fires$LAT),max(fires$LAT)), xlim=c(min(fires$LON),max(fires$LON)), 
     type = "n", xlab = "Latitude", ylab = "Longitude")
maps :: map("world", add = TRUE, lwd = 2, col = "darkgreen")
title(main = "Recreation and Ceremony", cex=2)
points(setz$`Recreation and ceremony`$LON, setz$`Recreation and ceremony`$LAT, pch = 16, cex=0.2, col = "red")

plot(fires$LON, fires$LAT, ylim=c(min(fires$LAT),max(fires$LAT)), xlim=c(min(fires$LON),max(fires$LON)), 
     type = "n", xlab = "Latitude", ylab = "Longitude")
maps :: map("world", add = TRUE, lwd = 2, col = "darkgreen")
title(main = "Smoking", cex=2)
points(setz$Smoking$LON, setz$Smoking$LAT, pch = 16, cex=0.2, col = "red")

#dev.off()





####
## WildFire Sample
colSums(is.na(fires))
fires <- fires %>% drop_na(EndDate)
colSums(is.na(fires))

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
NSPRCP
NSTMAX <- samFire[,c(16,1)]
NSTMAX$TempMax <- "na"
NSTMAX
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
setwd("C:\\Users\\ejtur\\OneDrive\\Desktop\\Ithaca College\\CAP2\\DroughtData")

AK <- read.csv("AK.csv")
AK$State <- "AK"
AL <- read.csv("AL.csv")
AL$State <- "AL"
AR <- read.csv("AR.csv")
AR$State <- "AR"
AZ <- read.csv("AZ.csv")
AZ$State <- "AZ"
CA <- read.csv("CA.csv")
CA$State <- "CA"
CO <- read.csv("CO.csv")
CO$State <- "CO"
CT <- read.csv("CT.csv")
CT$State <- "CT"
DC <- read.csv("DC.csv")
DC$State <- "DC"
DE <- read.csv("DE.csv")
DE$State <- "DE"
FL <- read.csv("FL.csv")
FL$State <- "FL"
GA <- read.csv("GA.csv")
GA$State <- "GA"
HI <- read.csv("HI.csv")
HI$State <- "HI"
IA <- read.csv("IA.csv")
IA$State <- "IA"
ID <- read.csv("ID.csv")
ID$State <- "ID"
IL <- read.csv("IL.csv")
IL$State <- "IL"
IN <- read.csv("IN.csv")
IN$State <- "IN"
KS <- read.csv("KS.csv")
KS$State <- "KS"
KY <- read.csv("KY.csv")
KY$State <- "KY"
LA <- read.csv("LA.csv")
LA$State <- "LA"
MA <- read.csv("MA.csv")
MA$State <- "MA"
MD <- read.csv("MD.csv")
MD$State <- "MD"
ME <- read.csv("ME.csv")
ME$State <- "ME"
MI <- read.csv("MI.csv")
MI$State <- "MI"
MN <- read.csv("MN.csv")
MN$State <- "MN"
MO <- read.csv("MO.csv")
MO$State <- "MO"
MS <- read.csv("MS.csv")
MS$State <- "MS"
MT <- read.csv("MT.csv")
MT$State <- "MT"
NC <- read.csv("NC.csv")
NC$State <- "NC"
ND <- read.csv("ND.csv")
ND$State <- "ND"
NE <- read.csv("NE.csv")
NE$State <- "NE"
NH <- read.csv("NH.csv")
NH$State <- "NH"
NJ <- read.csv("NJ.csv")
NJ$State <- "NJ"
NM <- read.csv("NM.csv")
NM$State <- "NM"
NV <- read.csv("NV.csv")
NV$State <- "NV"
NY <- read.csv("NY.csv")
NY$State <- "NY"
OH <- read.csv("OH.csv")
OH$State <- "OH"
OK <- read.csv("OK.csv")
OK$State <- "OK"
OR <- read.csv("OR.csv")
OR$State <- "OR"
PA <- read.csv("PA.csv")
PA$State <- "PA"
PR <- read.csv("PR.csv")
PR$State <- "PR"
RI <- read.csv("RI.csv")
RI$State <- "RI"
SC <- read.csv("SC.csv")
SC$State <- "SC"
SD <- read.csv("SD.csv")
SD$State <- "SD"
TN <- read.csv("TN.csv")
TN$State <- "TN"
TX <- read.csv("TX.csv")
TX$State <- "TX"
UT <- read.csv("UT.csv")
UT$State <- "UT"
VA <- read.csv("VA.csv")
VA$State <- "VA"
VT <- read.csv("VT.csv")
VT$State <- "VT"
WA <- read.csv("WA.csv")
WA$State <- "WA"
WI <- read.csv("WI.csv")
WI$State <- "WI"
WV <- read.csv("WV.csv")
WV$State <- "WV"
WY <- read.csv("WY.csv")
WY$State <- "WY"

AL$Week <- format(as.Date(AL$Week, format = "%m/%d/%Y"), "%Y-%m-%d")
Drought <- bind_rows(AK, AL, AR, AZ, CA, CO, CT, DC, DE, FL, GA, HI, IA, ID, IL, IN, KS, 
                     KY, LA, MA, MD, ME, MI, MN, MO, MS, MT, NC, ND, NE, NH, NJ, NM, NV, 
                     NY, OH, OK, OR, PA, PR, RI, SC, SD, TN, TX, UT, VA, VT, WA, WI, WV, 
                     WY)
colnames(Drought) <- c("Date", "None", "D0.D4", "D1.D4", "D2.D4", "D3.D4", "D4", "DSCI", "State")
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

