library("tidyverse")
library("DBI")
library("RMySQL")
library("rsconnect")
library("lubridate")
library("RMariaDB")
library("ncdf4")
library("rnoaa")

#Using the developer DBI, but RMariaDB is not available for the newest R version in developer mode
#devtools::install_github("r-dbi/DBI")


#Loading the .env file
readRenviron(".env")


# In case if there are too many connections open
lapply(dbListConnections(MySQL()), dbDisconnect)


###Sensor Data

con <- dbConnect(RMariaDB::MariaDB(),
                 user="LCRoysterproject", 
                 password=Sys.getenv("password"),
                 dbname=Sys.getenv("dbname"), 
                 host=Sys.getenv("host"), 
                 port= 3359)

# Listing all of the columns in the database
dbListTables(con)

wq <- dbReadTable(conn = con, name = 'lcroyster_buoyobservation')

#Changing the format of a new date for ggplot, plotting, Posixct
wq$date<- as.POSIXct(wq$observation_datetime, tz="EST",usetz=TRUE)

#Changing the format for these date for filtere in dplyr
wq$observation_datetime<- as.Date(wq$observation_datetime, tz="EST",usetz=TRUE)

#standard=42.914
#wq$sal <- convert_RtoS(wq$conductivity_mS_cm/standard, 
#t= wq$temperature_c, p= 0)

colnames(wq) <- c("ID", "Obs_Date", "In_Service", "Pressure", "Temperature", "Conductivity", "Salinity_OG", "Sound_velo",  "Site","Sensor_ID", "Salinity","Date")

write.csv(wq,file = "wq_lcr_shiny_app/data/wq_total.csv")

## Removing any values over 40 ppt
wq<-wq %>% 
  filter(!(Salinity > 40))

## Removing dates where sensors were malfunctioning in Site 2, oyster growth from 10/31/2017- 12/1/2017
wq<- wq %>% 
  filter(!(Site == 2 & Date > "2017-10-31 23:00:00" & Date < "2017-12-01 23:00:00" ))

#Removing Site 10 values before a certain time of when the sensor was function
wq<-wq %>% 
  filter(!(Site == 10 & Date < "2019-03-08 15:03:00"))

#Removing this values because of removal and redeployment of sensor
wq<- wq %>% 
  filter(!(Site == 2 & Date > "2019-04-27 19:00:00" & Date < "2019-05-06 14:00:00" ))

#Removing values from site 6, sensor is faulty between July 16, 2019 to August 20, 2019, starting at 17:00 
wq<- wq %>% 
  filter(!(Site == 6 & Date > "2019-07-16 23:00:00" & Date < "2019-08-20 17:00:00" ))

# Removing a flatline reading in the code 
wq<-wq %>% 
  filter(!(Site == 11 & Date > "2019-07-29 23:00:00" & Date < "2019-08-02 23:00:00"))

# Removing a flatline reading in the code for site 9
wq<-wq %>% 
  filter(!(Site == 9 & Date > "2019-09-05 23:00:00" & Date < "2019-09-27 23:00:00"))

#Removing observations from site 4, Flatiline in early September 
wq<-wq %>% 
  filter(!(Site == 4 & Date > "2019-08-28 23:00:00" & Date < "2019-09-10 23:00:00"))

#Removing observations from site 4, Flatline in mid October
wq<-wq %>% 
  filter(!(Site == 4 & Date > "2019-10-14 23:00:00" & Date < "2019-11-04 23:00:00"))

#Removing observations from site 2, Flatline in early October 
wq<-wq %>% 
  filter(!(Site == 2 & Date > "2019-10-01 23:00:00" & Date < "2019-10-15 23:00:00"))


#Removing observations from site 4, Flatlined and muddy starting november 12- november 18
wq<-wq %>% 
  filter(!(Site == 4 & Date > "2019-11-11 23:00:00" & Date < "2019-11-18 23:00:00"))

#Removing all the observations from after 2020/04/09
wq<-wq %>% 
  filter(!(Date > "2020-04-09 23:00:00" & Date < "2020-06-10 23:00:00"))

#Removing observations from site 5, flatlined
wq<-wq %>% 
  filter(!(Site == 5 & Date > "2020-07-15 00:00:00" & Date < "2020-07-31 00:00:00"))

#Removing observations from site 10, Flatlined on last day
wq<-wq %>% 
  filter(!(Site == 10 & Date > "2020-08-26 00:00:00" & Date < "2020-08-27 23:00:00"))

#Removing observations from site 8, Flatlined on last observation
wq<-wq %>% 
  filter(!(Site == 8 & Date > "2020-08-26 00:00:00" & Date < "2020-08-27 23:00:00"))


#Removing observations from site 5, Flatlined during this whole duration, barnacle growing on sensor
wq<-wq %>% 
  filter(!(Site == 5 & Date > "2020-09-01 00:00:00" & Date < "2020-09-23 23:00:00"))

#Removing observations from site 10, sensor dropped observation on last day, removing
wq<-wq %>% 
  filter(!(Site == 10 & Date > "2020-09-22 00:00:00" & Date < "2020-09-23 23:00:00"))

#Removing observations that were found on lost sensor 10072, do not need here
wq<-wq %>% 
  filter(!(Site == 2 & Date > "2020-07-30 00:00:00" & Sensor_ID == 36))

#Removing observations on these dates on site 3, they seem to be very sporadic, as well as too high, only these 5 days
wq<-wq %>% 
  filter(!(Site == 3 & Date > "2020-11-29 00:00:00" & Date < "2020-12-03 23:00:00"))


#Removing observations on these dates on site 7, they start getting way too high starting 12-28-2020
wq<-wq %>% 
  filter(!(Site == 7 & Date > "2020-12-28 00:00:00" & Date < "2021-01-03 23:00:00"))

#Removing the last observation on these dates on site 2, just flat lined
wq<-wq %>% 
  filter(!(Site == 2 & Date > "2021-01-06 00:00:00" & Date < "2021-01-07 00:00:00"))

#Removing observations on these dates on site 7, these are very high salinity measurements
wq<-wq %>% 
  filter(!(Site == 7 & Date > "2021-01-03 00:00:00" & Date < "2021-01-19 23:00:00"))

#Removing observations on these dates on site 9, these are very high salinity measurements
wq<-wq %>% 
  filter(!(Site == 9 & Date > "2020-12-020 00:00:00" & Date < "2021-01-13 23:00:00"))

#Removing observations on these dates on site 6, flat lined
wq<-wq %>%
  filter(!(Site == 6 & Date > "2021-04-01 00:00:00" & Date < "2021-04-13 23:00:00"))

#Removing all trial 
wq<-wq %>% 
  filter(!(Site == 0))

#Annalees sensors
wq<-wq %>% 
  filter(!(Site == 11))
wq<-wq %>% 
  filter(!(Site == 21))
wq<-wq %>% 
  filter(!(Site ==22))
wq<-wq %>% 
  filter(!(Site ==23))


wqs1<-wq %>%
  filter(Site == 1) %>% 
  mutate(Obs_Date = as.Date(Obs_Date)) %>%
  complete(Obs_Date = seq.Date(min(Obs_Date), max(Obs_Date), by="day"))

wqs1['Site'] = as.numeric(1)

wqs2<-wq %>%
  filter(Site == 2) %>% 
  mutate(Obs_Date = as.Date(Obs_Date)) %>%
  complete(Obs_Date = seq.Date(min(Obs_Date), max(Obs_Date), by="day"))

wqs2['Site'] = as.numeric(2)

wqs3<-wq %>%
  filter(Site == 3) %>% 
  mutate(Obs_Date = as.Date(Obs_Date)) %>%
  complete(Obs_Date = seq.Date(min(Obs_Date), max(Obs_Date), by="day"))

wqs3['Site'] = as.numeric(3)

wqs4<-wq %>%
  filter(Site == 4) %>% 
  mutate(Obs_Date = as.Date(Obs_Date)) %>%
  complete(Obs_Date = seq.Date(min(Obs_Date), max(Obs_Date), by="day"))

wqs4['Site'] = as.numeric(4)

wqs5<-wq %>%
  filter(Site == 5) %>% 
  mutate(Obs_Date = as.Date(Obs_Date)) %>%
  complete(Obs_Date = seq.Date(min(Obs_Date), max(Obs_Date), by="day"))

wqs5['Site'] = as.numeric(5)

wqs6<-wq %>%
  filter(Site == 6) %>% 
  mutate(Obs_Date = as.Date(Obs_Date)) %>%
  complete(Obs_Date = seq.Date(min(Obs_Date), max(Obs_Date), by="day"))

wqs6['Site'] = as.numeric(6)

wqs7<-wq %>%
  filter(Site == 7) %>% 
  mutate(Obs_Date = as.Date(Obs_Date)) %>%
  complete(Obs_Date = seq.Date(min(Obs_Date), max(Obs_Date), by="day"))

wqs7['Site'] = as.numeric(7)

wqs8<-wq %>%
  filter(Site == 8) %>% 
  mutate(Obs_Date = as.Date(Obs_Date)) %>%
  complete(Obs_Date = seq.Date(min(Obs_Date), max(Obs_Date), by="day"))

wqs8['Site'] = as.numeric(8)

wqs9<-wq %>%
  filter(Site == 9) %>% 
  mutate(Obs_Date = as.Date(Obs_Date)) %>%
  complete(Obs_Date = seq.Date(min(Obs_Date), max(Obs_Date), by="day"))

wqs9['Site'] = as.numeric(9)

wqs10<-wq %>%
  filter(Site == 10) %>% 
  mutate(Obs_Date = as.Date(Obs_Date)) %>%
  complete(Obs_Date = seq.Date(min(Obs_Date), max(Obs_Date), by="day"))

wqs10['Site'] = as.numeric(10)


wqs12<-wq %>%
  filter(Site == 12) %>% 
  mutate(Obs_Date = as.Date(Obs_Date)) %>%
  complete(Obs_Date = seq.Date(min(Obs_Date), max(Obs_Date), by="day"))

wqs12['Site'] = as.numeric(12)

wqs13<-wq %>%
  filter(Site == 13) %>% 
  mutate(Obs_Date = as.Date(Obs_Date)) %>%
  complete(Obs_Date = seq.Date(min(Obs_Date), max(Obs_Date), by="day"))

wqs13['Site'] = as.numeric(13)

wq<- rbind(wqs1,wqs2,wqs3,wqs4,wqs5,wqs6,wqs7,wqs8, wqs9,wqs10, wqs12, wqs13)


#Writting as a .csv for the Shiny App
write.csv(wq,file = "wq_lcr_shiny_app/data/wq.csv")



###Lakewatch and YSI Data, from the MySQL workbench


#Connecting to Database to update file 
lab <- dbReadTable(conn = con, name = 'lcroyster_waterobservation')

#Changing column names for an easier read in the Shiny App
colnames(lab) <- c("ID", "Date", "Phosphorus", "Nitrogen", "Chlorophyll", "Secchi", "Color", "Temperature", "Conductivity", "Site", "Sensor_Type","Salinity", "Sun_Code", "DO", "Depth")

#This will change the format to just yyyy/mm/dd
#lab$Date <- as.Date(lab$Date)

#Changing secchi from FT to Meters
lab$Secchi<- (lab$Secchi/ 3.28)

# We need to update the sensor_type to the correct names for facetting, 4= YSI and 5= Lakewatch 
lab$Sensor_Type[lab$Sensor_Type == "34"] <- "LAKEWATCH"
lab$Sensor_Type[lab$Sensor_Type == "35"] <- "YSI"
lab$Sensor_Type[lab$Sensor_Type == "37"] <- "YSI"

lab<-lab %>% 
  filter(!(Site == 0))

#Writting as a .csv for the Shiny App
write.csv(lab, file = "wq_lcr_shiny_app/data/lab.csv")

lakewatch<-lab %>% 
  filter(Sensor_Type == "LAKEWATCH")

write.csv(lab, file = "wq_lcr_shiny_app/data/lakewatch.csv")



###Wind data

wind17 <- buoy(dataset='cwind',buoyid='CDRF1', datatype='c', year=2017)
wind18 <- buoy(dataset='cwind',buoyid='CDRF1', datatype='c', year=2018)
wind19 <- buoy(dataset='cwind',buoyid='CDRF1', datatype='c', year=2019)
wind20 <- buoy(dataset='cwind',buoyid='CDRF1', datatype='c', year=2020)
wind21 <- buoy(dataset='cwind',buoyid='CDRF1', datatype='c', year=2021)
wind <- rbind(wind17$data, wind18$data, wind19$data, wind20$data, wind21$data) %>% dplyr::distinct()

write_rds(wind, "wq_lcr_shiny_app/data/wind.rds")



