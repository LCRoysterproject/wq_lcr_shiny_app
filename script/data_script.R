library("tidyverse")
library("DBI")
library("RMySQL")
library("rsconnect")
library("lubridate")
library("RMariaDB")

#Using the developer DBI, but RMariaDB is not available for the newest R version in developer mode
#devtools::install_github("r-dbi/DBI")


# In case if there are too many connections open
lapply(dbListConnections(MySQL()), dbDisconnect)


###Sensor Data

con <- dbConnect(RMariaDB::MariaDB(),
                 user="LCRoysterproject", 
                 password="HLLV6Pske0vTzhIZfSya",
                 dbname="LCRoysterproject", 
                 host="ict-prod-hosting05.mysql.osg.ufl.edu", 
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

# A quick ggplot test
ggplot(data= wq,aes( x=Date, y= Salinity))+
  geom_point()+
  facet_wrap (~Site)

## Removing dates where sensors were malfunctioning in Site 2, oyster growth
wq<- wq %>% 
  filter(!(Site == 2 & Salinity < 0.5))

## Removing dates where sensors were malfunctioning in Site 3
wq<-wq %>% 
  filter(!(Site == 3 & Salinity < 4))

## Removing any values over 40 ppt
wq<-wq %>% 
  filter(!(Salinity > 40))


#Writting as a .csv for the Shiny App
write.csv(wq,file = "wq_app/data/wq.csv")


###Lakewatch Data, from the MySQL workbench


#Connecting to Database to update file 
lab <- dbReadTable(conn = con, name = 'lcroyster_waterobservation')

#Changing column names for an easier read in the Shiny App
colnames(lab) <- c("ID", "Date", "Phosphorus", "Nitrogen", "Chlorophyll", "Secchi", "Color", "Temperature", "Conductivity", "Site", "Sensor_Type","Salinity", "Sun_Code", "DO", "Depth")

#This will change the format to just yyyy/mm/dd
#lab$Date <- as.Date(lab$Date)

#Changing secchi from FT to Meters
lab$Secchi<- (lab$Secchi/ 3.28)

# We need to update the sensor_type to the correct names for facetting, 4= YSI and 5= Lakewatch 
lab$Sensor_Type[lab$Sensor_Type == "4"] <- "LAKEWATCH"
lab$Sensor_Type[lab$Sensor_Type == "5"] <- "YSI"

#Writting as a .csv for the Shiny App
write.csv(lab, file = "wq_app/data/lab.csv")



