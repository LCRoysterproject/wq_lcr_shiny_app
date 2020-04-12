library("tidyverse")
library("DBI")
library("RMySQL")
library("rsconnect")
library("lubridate")
library("RMariaDB")

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

#Removing observations from site 4, Flatiline in mid October
wq<-wq %>% 
  filter(!(Site == 4 & Date > "2019-10-14 23:00:00" & Date < "2019-11-04 23:00:00"))

#Removing observations from site 2, Flatiline in early October 
wq<-wq %>% 
  filter(!(Site == 2 & Date > "2019-10-01 23:00:00" & Date < "2019-10-15 23:00:00"))


#Removing observations from site $, Flatilined and muddy starting november 12- november 18
wq<-wq %>% 
  filter(!(Site == 4 & Date > "2019-11-11 23:00:00" & Date < "2019-11-18 23:00:00"))

#Removing all the observations from after 2020/04/09
wq<-wq %>% 
  filter(!(Date > "2020-04-10 23:00:00"))

#Removing all trial 
wq<-wq %>% 
  filter(!(Site == 0))

wq<-wq %>% 
  filter(!(Site == 11))

#Annalees sensors
wq<-wq %>% 
  filter(!(Site == 21))
wq<-wq %>% 
  filter(!(Site ==22))
wq<-wq %>% 
  filter(!(Site ==23))


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

lab<-lab %>% 
  filter(!(Site == 0))

#Writting as a .csv for the Shiny App
write.csv(lab, file = "wq_lcr_shiny_app/data/lab.csv")


