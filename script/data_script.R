<<<<<<< HEAD
library("DBI")
library("lubridate")
library("RMySQL")
library("RMariaDB")
library("rsconnect")
library("tidyverse")

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

## Removing any values over 40 ppt
wq<-wq %>% 
  filter(!(Salinity > 40))

## Removing dates where sensors were malfunctioning in Site 2, oyster growth
wq<- wq %>% 
  filter(!(Site == 2 & Salinity < 0.5))

## Removing dates where sensors were malfunctioning in Site 3
wq<-wq %>% 
  filter(!(Site == 3 & Salinity < 4))

#Removing Site 10 values before a certain time of when the sensor was function
wq<-wq %>% 
  filter(!(Site == 10 & Date < "2019-03-08 15:03:00"))

wq<- wq %>% 
  filter(!(Site == 2 & Date > "2019-04-27 19:00:00" & Date < "2019-05-06 14:00:00" ))

wq<-wq %>% 
  filter(!(Site == 11))

wq<-wq %>% 
  filter(!(Site == 21))

wq<-wq %>% 
  filter(!(Site == 0))

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

lab<-lab %>% 
  filter(!(Site == 0))

#Writting as a .csv for the Shiny App
write.csv(lab, file = "wq_app/data/lab.csv")



=======
library("DBI")
library("lubridate")
library("RMySQL")
library("RMariaDB")
library("rsconnect")
library("tidyverse")

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

## Removing any values over 40 ppt
wq<-wq %>% 
  filter(!(Salinity > 40))

## Removing dates where sensors were malfunctioning in Site 2, oyster growth
wq<- wq %>% 
  filter(!(Site == 2 & Salinity < 0.5))

## Removing dates where sensors were malfunctioning in Site 3
wq<-wq %>% 
  filter(!(Site == 3 & Salinity < 4))

#Removing Site 10 values before a certain time of when the sensor was function
wq<-wq %>% 
  filter(!(Site == 10 & Date < "2019-03-08 15:03:00"))

wq<- wq %>% 
  filter(!(Site == 2 & Date > "2019-04-27 19:00:00" & Date < "2019-05-06 14:00:00" ))

wq<-wq %>% 
  filter(!(Site == 11))

wq<-wq %>% 
  filter(!(Site == 21))

wq<-wq %>% 
  filter(!(Site == 0))

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

lab<-lab %>% 
  filter(!(Site == 0))

#Writting as a .csv for the Shiny App
write.csv(lab, file = "wq_app/data/lab.csv")



>>>>>>> b44eed71aee5a526cf684c405dfa299c4c8c76b7
