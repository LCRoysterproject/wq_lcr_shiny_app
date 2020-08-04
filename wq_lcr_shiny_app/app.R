library("tidyverse")
library("shinythemes")
library("ggplot2")
library("lubridate")
library("naniar")
library("zoo")
library("ncdf4")
library("rnoaa")
library("plotly")
library("RColorBrewer")

#Make sure to be on the project directory before starting the Shiny App

#Reading in the files from the "data" tab
lab <- read.csv("data/lab.csv", header= T) 
wq <- read.csv("data/wq.csv", header= T) 

#Removing all -999 from data frames
lab[lab == -999] <- NA
wq[wq == -999] <- NA

#Updating the date zones to UTC for water quality and lab data
wq$Date<- ymd_hms(wq$Date, tz="UTC") %>%
  round_date("hour")

lab$Date <- lab$Date %>%
  ymd_hms() %>%
  date() %>%
  paste("12:00:00") %>%
  ymd_hms(tz="UTC")


# Function to make factors with levels based on site input and not numerical
# So that ggplot facet display the sequence correctly.

factor_site_seq <- function (vec, site1, site2) {
  # The condition is required such that you don't declare levels of same values
  # or when they pick "none"
  if (site2 != 0 & site1 != site2) {
    f <- factor(vec, levels = c(site1, site2))
  } else f <- vec
  return(f)
}


###Wind data

wind <- read_rds("data/wind.rds")



#### Front of the app, the user interface

ui <- fluidPage(
  #Selecting a theme from pre-made shiny themes
  theme = shinytheme("yeti"),
  
  #Creating the design and designations of the side bar, width=4 is too large and will not display the graphs correctly
  sidebarLayout(
    sidebarPanel(
      
      width = 4,
      
      selectInput("site1", label= h5("SITE (input needed for all tabs, except Wind Rose)"), 
                  choices=c(unique(wq$Site) %>% sort()), selected = 1),
      
      selectInput("site2", label=h5("COMPARISON SITE (input needed for all tabs, except Wind Rose)"), 
                  choices=c("None" = 0, unique(wq$Site) %>% sort()), selected = 6),
      
      
      dateRangeInput("date",
                     label =h5('DATE RANGE (input needed for Data Logger, Rolling Averages and Windrose Tabs)'),
                     start = Sys.Date() -45 , end = Sys.Date()),
  
      radioButtons("variable",
                   label = h5("OBSERVATIONS (input needed for Data Logger Measurements and Rolling Averages tabs)"),
                   choices = list("Salinity (ppt)" = "Salinity",
                                  "Conductivity (mS/cm)"= "Conductivity",
                                  "Temperature (C) " = "Temperature"),
                   selected = "Salinity"),
      
      h6("Overlay only available in `Hourly` Temporal Resolution (Data Logger Measurements tab)"),
      
      checkboxInput("overlay", 
                    label = "Overlay point sample data (Salinity (YSI only), Conductivity (Lakewatch and YSI), or Temperature (YSI only))?",
                    value = T),
      
      radioButtons("temp_res",
                   label = h5("TEMPORAL RESOLUTION (input needed for only Data Logger Measurements tab)"),
                   choices = list("Hourly" = "Hourly",
                                  "Daily Mean" = "Daily"),
                   selected = "Hourly"),
      
      
      
      h4("LAKEWATCH TAB OPTIONS"),
      
      dateRangeInput("date2",
                     label =h5('DATE RANGE (Lakewatch)'),
                     start ="2019-02-01" , end = "2020-02-01"),
      
      radioButtons("variable2",
                   label = h5("LAKEWATCH OBSERVATIONS"),         
                   choices = list(#"Salinity (ppt)" = "Salinity",
                                  #"Conductivity (mS/cm)"= "Conductivity",
                                  #"Temperature (C)" = "Temperature",
                                  "Phosphorus (ug/L)" = "Phosphorus",
                                  "Nitrogen (ug/L)" = "Nitrogen",
                                  "Color (Pt-Co Units)" = "Color",
                                  "Secchi (m)" = "Secchi",
                                  "Chlorophyll (ug/L)" = "Chlorophyll"), 
                   selected = c("Phosphorus"))),
      
    
    
    # The display of the main panel
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel(title= "MAP OF SELECTED SITES", 
                           br(), 
                           h3("Map functionality"),
                           p("Use the drop-down boxes Site and Comparison Site to dynamically change this map. The selected sites will be circled on the map."),
                           uiOutput("map", height = "800px")),
                  tabPanel(title="DATA LOGGER MEASUREMENTS",
                           br(), 
                           h3("Data selection"),
                           p("Hourly observations are collected using data loggers (Diver and Star-Oddi) as of September 2017. Select the desired Date Range, Site and Comparison Site options. Select additional information such as the type of Observations and/or Temporal Resolution. An overlay of YSI/ Lakewatch measurements can also be added to this figure, in the Hourly Temporal Resolution. Missing observations are due to corrupt data or temporarily removed sensors."),
                           plotOutput("sensorplot", height = "600px")),

                  tabPanel(title="ROLLING AVERAGES", 
                           br(), 
                           h3("Rolling averages definition"),
                           p("Rolling or moving averages are a way to reduce noise and smooth time series data. Rolling averages were calculated using the function `rollmean()` in the R package `zoo`. Select the desired Date Range, Site and Comparison Site. Select additional information such as the type of Observations (Salinity, Conductivity, or Temperature) for the figure to display."),
                           plotOutput("rollingplot", height = "600px")),
                  tabPanel(title="LAKEWATCH", 
                           br(), 
                           h3("Data selection"),
                           p("Updated measurements are available every 4 months through Lakewatch (https://lakewatch.ifas.ufl.edu/). Lakewatch measurements are only available for Sites 1-6. Select the desired Date Range, Site and Comparison Site. Select the desired Observation type under LAKEWATCH TAB OPTIONS. Missing observations are due to processing lab time. If no values display in this figure, please select a broader date range."),
                           plotOutput("labplot", height = "600px")),
                  tabPanel(title="WIND ROSE", 
                           br(), 
                           h3("Wind data"),
                           p("The wind rose below displays the magnitude and wind direction of a desired Date Range. Wind speed and direction data are provided by the R package `rnoaa`. Wind data are updated periodically through USGS (monthly basis). If wind data are not displaying in this figure, please select a broader date range. Wind roses are subject to change as new wind data become available."),
                           plotOutput("wind", height = "600px"))
                
                  
      
      )
    )
  )
)

server <- shinyServer(function(input, output) {
  
  # Can directly use renderPlot instead of having reactive first
  output$sensorplot <- renderPlot({
    site1 <- as.numeric(input$site1)
    site2 <- as.numeric(input$site2)
    startDate <- paste(input$date[1], "00:00:00") %>% ymd_hms(tz="UTC")
    endDate <- paste(input$date[2], "23:00:00") %>% ymd_hms(tz="UTC")
    
    ### sensorplot
    # Filter WQ table
    wq1 <- wq %>% 
      filter(Site == site1 | Site == site2,
             Date >= startDate & Date <= endDate) %>% 
      select(Site, Date, Measure = input$variable)
    
    
    # Build a data table based on input daterange and temporal resolution
    # Note: We're building a table with all possible times first and merge it with
    # WQ table so that NAs and daterange of plot is preserved.
    # NAs are required for line plot
    
    
    if (input$temp_res == "Hourly") {
      d <- seq(startDate, endDate, by = "hour")
      df <- data.frame(Site = rep(c(site1, site2), each = length(d)),
                       Date = rep(d, 2)) %>%
        distinct() %>% 
        left_join(wq1)
      
    } else if (input$temp_res == "Daily") {
      
      # Need to convert WQ from hourly to daily
      wq2 <- wq1 %>%
        mutate(Date1 = date(Date)) %>%
        group_by(Site, Date1) %>%
        summarise(Measure = mean(Measure)) %>%
        select(Site, Date=Date1, Measure)
      
      d <- seq(startDate, endDate, by = "day") %>% date
      df <- data.frame(Site = rep(c(site1, site2), each = length(d)),
                       Date = rep(d, 2)) %>%
        distinct() %>%
        left_join(wq2)
    }
    
    # Remove Site 0 from the df we built
    df <- df %>%
      filter(Site != 0)
    
    # Adjusting Site's levels such that site1 goes before site2 regardless of
    # numerical order.
    df$Site <- factor_site_seq(df$Site, site1, site2)
    
    # Base version of the plot
    sensorplot <- ggplot(df, aes(x = Date, y = Measure)) +
      ylab(input$variable) +
      geom_line() +
      theme_gray(base_size = 14) +
      theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid")) +
      facet_wrap(~Site, ncol = 1, labeller = label_both) +
      theme(strip.text = element_text(size=30), 
            axis.text = element_text(size=20), 
            axis.title = element_text(size=20),
            panel.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill=NA, size=1))
    
    
    # Add feature if we want to overlay the point sample data
    if (input$overlay) {
      lab1 <- lab %>% 
        filter(Site == site1 | Site == site2,
               Date >= startDate & Date <= endDate) %>% 
        select(Site, Date, Measure = input$variable, Sensor_Type)
      
      # Same thing as we did for df$Site
      lab1$Site <- factor_site_seq(lab1$Site, site1, site2)
      
      if (input$temp_res == "Hourly") {
        sensorplot <- sensorplot + 
          geom_point(data = lab1, aes(x = Date, y = Measure, colour = Sensor_Type), shape = 17, size = 5) +
          scale_color_manual(name = "Method", values = c("red", "blue")) 
        
      } else if (input$temp_res == "Daily") {
        sensorplot 
      }
      
    }
    
    sensorplot
  })
  
  output$labplot<-renderPlot({
    site1 <- as.numeric(input$site1)
    site2 <- as.numeric(input$site2)
    startDate <- paste(input$date2[1], "00:00:00") %>% ymd_hms(tz="UTC")
    endDate <- paste(input$date2[2], "23:00:00") %>% ymd_hms(tz="UTC")
    
    lab1 <- lab %>% 
      filter(Site == site1 | Site == site2,
             Date >= startDate & Date <= endDate) %>% 
      filter(Sensor_Type == "LAKEWATCH") %>% 
      select(Site, Date, Measure = input$variable2, Sensor_Type)
    
    
    # Same thing as we did for sensorplot to force sequence of display
    lab1$Site <- factor_site_seq(lab1$Site, site1, site2)
    
    # Use similar trick as we did in sensorplot (e.g. build a df)
    labplot <- ggplot(lab1, aes(x = Date, y = Measure)) +
      ylab(input$variable2) +
      geom_point(shape = 17, size = 5) +
      #scale_color_manual(name = "Method", values = c("red", "blue")) +
      scale_x_datetime(
        #   breaks = date_breaks("month") ,
        #   labels = date_format("%m/%Y")
        limits = c(startDate, endDate)) +
      theme_gray(base_size = 14) +
      theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid")) +
      facet_wrap(~ Site, ncol = 1, labeller = label_both) +
      theme(strip.text = element_text(size=30), 
            axis.text = element_text(size=20), 
            axis.title = element_text(size=20),
            panel.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill=NA, size=1))
    
    
    labplot
  })
  
  
  output$map <- renderUI({
    if(input$site1== "1" & input$site2 == "6" ){            
      img(height = 600, width = 800, src = "1_6.jpg")
    } 
    else if(input$site2== "1" & input$site1 == "6" ){            
      img(height = 600, width = 800, src = "1_6.jpg")
    }  
    
    else if(input$site2== "1" & input$site1 == "1" ){            
      img(height = 600, width = 800, src = "1_N.jpg")
    }  
    else if(input$site1== "1" & input$site2 == "1"){
      img(height = 600, width = 800, src = "1_N.jpg")
    }
    else if(input$site1== "1" & input$site2 == "2"){
      img(height = 600, width = 800, src = "1_2.jpg")
    }
    else if(input$site2== "1" & input$site1 == "2"){
      img(height = 600, width = 800, src = "1_2.jpg")
    }
    else if(input$site1== "1" & input$site2 == "3"){
      img(height = 600, width = 800, src = "1_3.jpg")
    }
    else if(input$site2== "1" & input$site1 == "3"){
      img(height = 600, width = 800, src = "1_3.jpg")
    }
    
    else if(input$site1== "1" & input$site2 == "4"){
      img(height = 600, width = 800, src = "1_4.jpg")
    }
    else if(input$site2== "1" & input$site1 == "4"){
      img(height = 600, width = 800, src = "1_4.jpg")
    }
    else if(input$site1== "1" & input$site2 == "5"){
      img(height = 600, width = 800, src = "1_5.jpg")
    }
    else if(input$site2== "1" & input$site1 == "5"){
      img(height = 600, width = 800, src = "1_5.jpg")
    }
    
    else if(input$site1== "1" & input$site2 == "7"){
      img(height = 600, width = 800, src = "1_7.jpg")
    }
    else if(input$site2== "1" & input$site1 == "7"){
      img(height = 600, width = 800, src = "1_7.jpg")
    }
    
    else if(input$site1== "1" & input$site2 == "8"){
      img(height = 600, width = 800, src = "1_8.jpg")
    }
    else if(input$site2== "1" & input$site1 == "8"){
      img(height = 600, width = 800, src = "1_8.jpg")
    }
    
    else if(input$site1== "1" & input$site2 == "9"){
      img(height = 600, width = 800, src = "1_9.jpg")
    }
    else if(input$site2== "1" & input$site1 == "9"){
      img(height = 600, width = 800, src = "1_9.jpg")
    }
    else if(input$site1== "1" & input$site2 == "10"){
      img(height = 600, width = 800, src = "1_10.jpg")
    }
    else if(input$site2== "1" & input$site1 == "10"){
      img(height = 600, width = 800, src = "1_10.jpg")
    }
    else if(input$site1== "1" & input$site2 == "0"){
      img(height = 600, width = 800, src = "1_N.jpg")
    }
    else if(input$site2== "1" & input$site1 == "0"){
      img(height = 600, width = 800, src = "1_N.jpg")
    }
    else if(input$site1== "2" & input$site2 == "3"){
      img(height = 600, width = 800, src = "2_3.jpg")
    }
    else if(input$site2== "2" & input$site1 == "3"){
      img(height = 600, width = 800, src = "2_3.jpg")
    }
    else if(input$site1== "2" & input$site2 == "4"){
      img(height = 600, width = 800, src = "2_4.jpg")
    }
    else if(input$site2== "2" & input$site1 == "4"){
      img(height = 600, width = 800, src = "2_4.jpg")
    }
    
    else if(input$site1== "2" & input$site2 == "5"){
      img(height = 600, width = 800, src = "2_5.jpg")
    }
    else if(input$site2== "2" & input$site1 == "5"){
      img(height = 600, width = 800, src = "2_5.jpg")
    }
    
    else if(input$site1== "2" & input$site2 == "6"){
      img(height = 600, width = 800, src = "2_6.jpg")
    }
    else if(input$site2== "2" & input$site1 == "6"){
      img(height = 600, width = 800, src = "2_6.jpg")
    }
    
    else if(input$site1== "2" & input$site2 == "7"){
      img(height = 600, width = 800, src = "2_7.jpg")
    }
    else if(input$site2== "2" & input$site1 == "7"){
      img(height = 600, width = 800, src = "2_7.jpg")
    }
    
    else if(input$site1== "2" & input$site2 == "8"){
      img(height = 600, width = 800, src = "2_8.jpg")
    }
    else if(input$site2== "2" & input$site1 == "8"){
      img(height = 600, width = 800, src = "2_8.jpg")
    }
    
    else if(input$site1== "2" & input$site2 == "9"){
      img(height = 600, width = 800, src = "2_9.jpg")
    }
    else if(input$site2== "2" & input$site1 == "9"){
      img(height = 600, width = 800, src = "2_9.jpg")
    }
    
    else if(input$site1== "2" & input$site2 == "10"){
      img(height = 600, width = 800, src = "2_10.jpg")
    }
    else if(input$site2== "2" & input$site1 == "10"){
      img(height = 600, width = 800, src = "2_10.jpg")
    }
    
    else if(input$site1== "2" & input$site2 == "0"){
      img(height = 600, width = 800, src = "2_N.jpg")
    }
    else if(input$site2== "2" & input$site1 == "0"){
      img(height = 600, width = 800, src = "2_N.jpg")
    }
    
    else if(input$site1== "2" & input$site2 == "2"){
      img(height = 600, width = 800, src = "2_N.jpg")
    }
    else if(input$site2== "2" & input$site1 == "2"){
      img(height = 600, width = 800, src = "2_N.jpg")
    }
    
    else if(input$site1== "3" & input$site2 == "4"){
      img(height = 600, width = 800, src = "3_4.jpg")
    }
    else if(input$site2== "3" & input$site1 == "4"){
      img(height = 600, width = 800, src = "3_4.jpg")
    }
    
    else if(input$site1== "3" & input$site2 == "5"){
      img(height = 600, width = 800, src = "3_5.jpg")
    }
    else if(input$site2== "3" & input$site1 == "5"){
      img(height = 600, width = 800, src = "3_5.jpg")
    }
    else if(input$site1== "3" & input$site2 == "6"){
      img(height = 600, width = 800, src = "3_6.jpg")
    }
    else if(input$site2== "3" & input$site1 == "6"){
      img(height = 600, width = 800, src = "3_6.jpg")
    }
    
    else if(input$site1== "3" & input$site2 == "7"){
      img(height = 600, width = 800, src = "3_7.jpg")
    }
    else if(input$site2== "3" & input$site1 == "7"){
      img(height = 600, width = 800, src = "3_7.jpg")
    }
    
    else if(input$site1== "3" & input$site2 == "8"){
      img(height = 600, width = 800, src = "3_8.jpg")
    }
    else if(input$site2== "3" & input$site1 == "8"){
      img(height = 600, width = 800, src = "3_8.jpg")
    }
    
    else if(input$site1== "3" & input$site2 == "9"){
      img(height = 600, width = 800, src = "3_9.jpg")
    }
    else if(input$site2== "3" & input$site1 == "9"){
      img(height = 600, width = 800, src = "3_9.jpg")
    }
    
    else if(input$site1== "3" & input$site2 == "10"){
      img(height = 600, width = 800, src = "3_10.jpg")
    }
    else if(input$site2== "3" & input$site1 == "10"){
      img(height = 600, width = 800, src = "3_10.jpg")
    }
    
    else if(input$site1== "3" & input$site2 == "0"){
      img(height = 600, width = 800, src = "3_N.jpg")
    }
    else if(input$site2== "3" & input$site1 == "0"){
      img(height = 600, width = 800, src = "3_N.jpg")
    }
    
    else if(input$site1== "3" & input$site2 == "3"){
      img(height = 600, width = 800, src = "3_N.jpg")
    }
    else if(input$site2== "3" & input$site1 == "3"){
      img(height = 600, width = 800, src = "3_N.jpg")
    }
    
    else if(input$site1== "4" & input$site2 == "5"){
      img(height = 600, width = 800, src = "4_5.jpg")
    }
    else if(input$site2== "4" & input$site1 == "5"){
      img(height = 600, width = 800, src = "4_5.jpg")
    }
    
    else if(input$site1== "4" & input$site2 == "6"){
      img(height = 600, width = 800, src = "4_6.jpg")
    }
    else if(input$site2== "4" & input$site1 == "6"){
      img(height = 600, width = 800, src = "4_6.jpg")
    }
    
    else if(input$site1== "4" & input$site2 == "7"){
      img(height = 600, width = 800, src = "4_7.jpg")
    }
    else if(input$site2== "4" & input$site1 == "7"){
      img(height = 600, width = 800, src = "4_7.jpg")
    }
    
    else if(input$site1== "4" & input$site2 == "8"){
      img(height = 600, width = 800, src = "4_8.jpg")
    }
    else if(input$site2== "4" & input$site1 == "8"){
      img(height = 600, width = 800, src = "4_8.jpg")
    }
    
    else if(input$site1== "4" & input$site2 == "9"){
      img(height = 600, width = 800, src = "4_9.jpg")
    }
    else if(input$site2== "4" & input$site1 == "9"){
      img(height = 600, width = 800, src = "4_9.jpg")
    }
    
    else if(input$site1== "4" & input$site2 == "10"){
      img(height = 600, width = 800, src = "4_10.jpg")
    }
    else if(input$site2== "4" & input$site1 == "10"){
      img(height = 600, width = 800, src = "4_10.jpg")
    }
    
    else if(input$site1== "4" & input$site2 == "0"){
      img(height = 600, width = 800, src = "4_N.jpg")
    }
    else if(input$site2== "4" & input$site1 == "0"){
      img(height = 600, width = 800, src = "4_N.jpg")
    }
    
    else if(input$site1== "4" & input$site2 == "4"){
      img(height = 600, width = 800, src = "4_N.jpg")
    }
    else if(input$site2== "4" & input$site1 == "4"){
      img(height = 600, width = 800, src = "4_N.jpg")
    }
    
    else if(input$site1== "5" & input$site2 == "6"){
      img(height = 600, width = 800, src = "5_6.jpg")
    }
    else if(input$site2== "5" & input$site1 == "6"){
      img(height = 600, width = 800, src = "5_6.jpg")
    }
    
    else if(input$site1== "5" & input$site2 == "7"){
      img(height = 600, width = 800, src = "5_7.jpg")
    }
    else if(input$site2== "5" & input$site1 == "7"){
      img(height = 600, width = 800, src = "5_7.jpg")
    }
    
    else if(input$site1== "5" & input$site2 == "8"){
      img(height = 600, width = 800, src = "5_8.jpg")
    }
    else if(input$site2== "5" & input$site1 == "8"){
      img(height = 600, width = 800, src = "5_8.jpg")
    }
    
    else if(input$site1== "5" & input$site2 == "9"){
      img(height = 600, width = 800, src = "5_9.jpg")
    }
    else if(input$site2== "5" & input$site1 == "9"){
      img(height = 600, width = 800, src = "5_9.jpg")
    }
    
    else if(input$site1== "5" & input$site2 == "10"){
      img(height = 600, width = 800, src = "5_10.jpg")
    }
    else if(input$site2== "5" & input$site1 == "10"){
      img(height = 600, width = 800, src = "5_10.jpg")
    }
    
    else if(input$site1== "5" & input$site2 == "0"){
      img(height = 600, width = 800, src = "5_N.jpg")
    }
    else if(input$site2== "5" & input$site1 == "0"){
      img(height = 600, width = 800, src = "5_N.jpg")
    }
    
    else if(input$site1== "5" & input$site2 == "5"){
      img(height = 600, width = 800, src = "5_N.jpg")
    }
    else if(input$site2== "5" & input$site1 == "5"){
      img(height = 600, width = 800, src = "5_N.jpg")
    }
    
    else if(input$site1== "6" & input$site2 == "7"){
      img(height = 600, width = 800, src = "6_7.jpg")
    }
    else if(input$site2== "6" & input$site1 == "7"){
      img(height = 600, width = 800, src = "6_7.jpg")
    }
    else if(input$site1== "6" & input$site2 == "8"){
      img(height = 600, width = 800, src = "6_8.jpg")
    }
    else if(input$site2== "6" & input$site1 == "8"){
      img(height = 600, width = 800, src = "6_8.jpg")
    }
    
    else if(input$site1== "6" & input$site2 == "9"){
      img(height = 600, width = 800, src = "6_9.jpg")
    }
    else if(input$site2== "6" & input$site1 == "9"){
      img(height = 600, width = 800, src = "6_9.jpg")
    }
    else if(input$site1== "6" & input$site2 == "10"){
      img(height = 600, width = 800, src = "6_10.jpg")
    }
    else if(input$site2== "6" & input$site1 == "10"){
      img(height = 600, width = 800, src = "6_10.jpg")
    }
    else if(input$site1== "6" & input$site2 == "0"){
      img(height = 600, width = 800, src = "6_N.jpg")
    }
    else if(input$site2== "6" & input$site1 == "0"){
      img(height = 600, width = 800, src = "6_N.jpg")
    }
    
    else if(input$site1== "6" & input$site2 == "6"){
      img(height = 600, width = 800, src = "6_N.jpg")
    }
    else if(input$site2== "6" & input$site1 == "6"){
      img(height = 600, width = 800, src = "6_N.jpg")
    }
    
    else if(input$site1== "7" & input$site2 == "8"){
      img(height = 600, width = 800, src = "7_8.jpg")
    }
    else if(input$site2== "7" & input$site1 == "8"){
      img(height = 600, width = 800, src = "7_8.jpg")
    }
    
    else if(input$site1== "7" & input$site2 == "9"){
      img(height = 600, width = 800, src = "7_9.jpg")
    }
    else if(input$site2== "7" & input$site1 == "9"){
      img(height = 600, width = 800, src = "7_9.jpg")
    }
    
    else if(input$site1== "7" & input$site2 == "10"){
      img(height = 600, width = 800, src = "7_10.jpg")
    }
    else if(input$site2== "7" & input$site1 == "10"){
      img(height = 600, width = 800, src = "7_10.jpg")
    }
    
    else if(input$site1== "7" & input$site2 == "0"){
      img(height = 600, width = 800, src = "7_N.jpg")
    }
    else if(input$site2== "7" & input$site1 == "0"){
      img(height = 600, width = 800, src = "7_N.jpg")
    }
    
    else if(input$site1== "7" & input$site2 == "7"){
      img(height = 600, width = 800, src = "7_N.jpg")
    }
    else if(input$site2== "7" & input$site1 == "7"){
      img(height = 600, width = 800, src = "7_N.jpg")
    }
    
    else if(input$site1== "8" & input$site2 == "9"){
      img(height = 600, width = 800, src = "8_9.jpg")
    }
    else if(input$site2== "8" & input$site1 == "9"){
      img(height = 600, width = 800, src = "8_9.jpg")
    }
    
    else if(input$site1== "8" & input$site2 == "10"){
      img(height = 600, width = 800, src = "8_10.jpg")
    }
    else if(input$site2== "8" & input$site1 == "10"){
      img(height = 600, width = 800, src = "8_10.jpg")
    }
    
    else if(input$site1== "8" & input$site2 == "0"){
      img(height = 600, width = 800, src = "8_N.jpg")
    }
    else if(input$site2== "8" & input$site1 == "0"){
      img(height = 600, width = 800, src = "8_N.jpg")
    }
    
    else if(input$site1== "8" & input$site2 == "8"){
      img(height = 600, width = 800, src = "8_N.jpg")
    }
    else if(input$site2== "8" & input$site1 == "8"){
      img(height = 600, width = 800, src = "8_N.jpg")
    }
    
    else if(input$site1== "9" & input$site2 == "10"){
      img(height = 600, width = 800, src = "9_10.jpg")
    }
    else if(input$site2== "9" & input$site1 == "10"){
      img(height = 600, width = 800, src = "9_10.jpg")
    }
    
    else if(input$site1== "9" & input$site2 == "0"){
      img(height = 600, width = 800, src = "9_N.jpg")
    }
    else if(input$site2== "9" & input$site1 == "0"){
      img(height = 600, width = 800, src = "9_N.jpg")
    }
    
    else if(input$site1== "9" & input$site2 == "9"){
      img(height = 600, width = 800, src = "9_N.jpg")
    }
    else if(input$site2== "9" & input$site1 == "9"){
      img(height = 600, width = 800, src = "9_N.jpg")
    }
    
    else if(input$site1== "10" & input$site2 == "0"){
      img(height = 600, width = 800, src = "10_N.jpg")
    }
    else if(input$site2== "10" & input$site1 == "0"){
      img(height = 600, width = 800, src = "10_N.jpg")
    }
    
    else if(input$site1== "10" & input$site2 == "10"){
      img(height = 600, width = 800, src = "10_N.jpg")
    }
    else if(input$site2== "10" & input$site1 == "10"){
      img(height = 600, width = 800, src = "10_N.jpg")
    }
    
  })
  
  output$rollingplot <- renderPlot({
    
    site1 <- as.numeric(input$site1)
    site2 <- as.numeric(input$site2)
    startDate <- paste(input$date[1], "00:00:00") %>% ymd_hms(tz="UTC")
    endDate <- paste(input$date[2], "23:00:00") %>% ymd_hms(tz="UTC")
    
    ### sensorplot
    # Filter WQ table
    wq1 <- wq %>% 
      filter(Site == site1 | Site == site2,
             Date >= startDate & Date <= endDate) %>% 
      select(Site, Date, Measure = input$variable)
    
    
    wq2<-wq1 %>%
      dplyr::group_by(ymd(Date), Site) %>% 
      dplyr::mutate(
        three_days = zoo::rollmean(Measure, k = 3, fill = NA),
        seven_days = zoo::rollmean(Measure, k = 7, fill = NA),
        fifteen_days = zoo::rollmean(Measure, k = 15, fill = NA)) %>% 
      dplyr::ungroup()
    
    
    wq3 <- wq2 %>% 
      tidyr::pivot_longer(names_to = "rolling_avg", 
                          values_to = "rolling_value", 
                          cols = c(three_days, seven_days, fifteen_days)) 
    
    wq3$rolling_avg <- factor(wq3$rolling_avg, c("three_days", "seven_days", "fifteen_days"))
    
    rollingplot<- wq3 %>% 
      drop_na("rolling_value") %>%
      ggplot(aes(x = Date, y = Measure)) +
      geom_line(aes(y = rolling_value, color = rolling_avg, group= rolling_avg), size = 1.1) +
      ylab(input$variable) + 
      xlab("Date")+
      facet_wrap(~Site + rolling_avg, labeller = label_both) +
      theme(legend.position = "none", 
        strip.text = element_text(size=15), 
            axis.text = element_text(size=15), 
            axis.title = element_text(size=15),
            panel.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill=NA, size=1))
    
    rollingplot
    
  })
  
  datasetInput <- reactive({
    site1 <- as.numeric(input$site1)
    site2 <- as.numeric(input$site2)
    startDate <- paste(input$date[1], "00:00:00") %>% ymd_hms(tz="UTC")
    endDate <- paste(input$date[2], "23:00:00") %>% ymd_hms(tz="UTC")
    
    ### sensorplot
    # Filter WQ table
    wq3 <- wq %>% 
      filter(Site == site1 | Site == site2,
             Date >= startDate & Date <= endDate) %>% 
      mutate(Date = as.character(Date)) %>% 
      select(Date, Site, input$variable)
    
    print(wq3)
    
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    })
  
  output$wind<- renderPlot({
    
    startDate <- paste(input$date[1], "00:00:00") %>% ymd_hms(tz="UTC")
    endDate <- paste(input$date[2], "23:00:00") %>% ymd_hms(tz="UTC")
    
    # Shrink the wind table, and convert the format of time
    wind$time <- ymd_hms(wind$time)
    
    wind <- wind %>%
      filter(time >= startDate & time <= endDate) %>%
      select(time, wind_spd, wind_dir)
    
    
    plot.windrose <- function(data,
                              spd,
                              dir,
                              spdres = 2,
                              dirres = 30,
                              spdmin = 2,
                              spdmax = 20,
                              spdseq = NULL,
                              palette = "YlGnBu",
                              countmax = NA,
                              debug = 0){
      
      
      # Look to see what data was passed in to the function
      if (is.numeric(spd) & is.numeric(dir)){
        # assume that we've been given vectors of the speed and direction vectors
        data <- data.frame(spd = spd,
                           dir = dir)
        spd = "spd"
        dir = "dir"
      } else if (exists("data")){
        # Assume that we've been given a data frame, and the name of the speed 
        # and direction columns. This is the format we want for later use.    
      }  
      
      # Tidy up input data ----
      n.in <- NROW(data)
      dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
      data[[spd]][dnu] <- NA
      data[[dir]][dnu] <- NA
      
      # figure out the wind speed bins ----
      if (missing(spdseq)){
        spdseq <- seq(spdmin,spdmax,spdres)
      } else {
        if (debug >0){
          cat("Using custom speed bins \n")
        }
      }
      # get some information about the number of bins, etc.
      n.spd.seq <- length(spdseq)
      n.colors.in.range <- n.spd.seq - 1
      
      # create the color map
      spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                        n.colors.in.range),
                                                    min(9,
                                                        n.colors.in.range)),                                               
                                                palette))(n.colors.in.range)
      
      if (max(data[[spd]],na.rm = TRUE) > spdmax){    
        spd.breaks <- c(spdseq,
                        max(data[[spd]],na.rm = TRUE))
        spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                              '-',
                              c(spdseq[2:n.spd.seq])),
                        paste(spdmax,
                              "-",
                              max(data[[spd]],na.rm = TRUE)))
        spd.colors <- c(spd.colors, "grey50")
      } else{
        spd.breaks <- spdseq
        spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                            '-',
                            c(spdseq[2:n.spd.seq]))    
      }
      data$spd.binned <- cut(x = data[[spd]],
                             breaks = spd.breaks,
                             labels = spd.labels,
                             ordered_result = TRUE)
      # clean up the data
      data. <- na.omit(data)
      
      # figure out the wind direction bins
      dir.breaks <- c(-dirres/2,
                      seq(dirres/2, 360-dirres/2, by = dirres),
                      360+dirres/2)  
      dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                      paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                            "-",
                            seq(3*dirres/2, 360-dirres/2, by = dirres)),
                      paste(360-dirres/2,"-",dirres/2))
      # assign each wind direction to a bin
      dir.binned <- cut(data[[dir]],
                        breaks = dir.breaks,
                        ordered_result = TRUE)
      levels(dir.binned) <- dir.labels
      data$dir.binned <- dir.binned
      
      # Run debug if required ----
      if (debug>0){    
        cat(dir.breaks,"\n")
        cat(dir.labels,"\n")
        cat(levels(dir.binned),"\n")       
      }  
      
      # deal with change in ordering introduced somewhere around version 2.2
      if(packageVersion("ggplot2") > "2.2"){    
        cat("Hadley broke my code\n")
        data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
        spd.colors = rev(spd.colors)
      }
      
      # create the plot ----
      p.windrose <- ggplot(data = data,
                           aes(x = dir.binned,
                               fill = spd.binned)) +
        geom_bar() + 
        scale_x_discrete(drop = FALSE,
                         labels = waiver()) +
        coord_polar(start = -((dirres/2)/360) * 2*pi) +
        scale_fill_manual(name = "Wind Speed (m/s)", 
                          values = spd.colors,
                          drop = FALSE) +
        #theme_bw() +
        theme(axis.title.x = element_blank(),
              #panel.border = element_rect(colour = "blank"),
              panel.grid.major = element_line(colour="grey65"))
      
      # adjust axes if required
      if (!is.na(countmax)){
        p.windrose <- p.windrose +
          ylim(c(0,countmax))
      }
      
      # print the plot
      print(p.windrose)  
      
      # return the handle to the wind rose
      return(p.windrose)
    }
    
    wind<-plot.windrose(spd = wind$wind_spd,
                  dir = wind$wind_dir)
    
    wind
    
    
 })
 

})


# Run the application 
shinyApp(ui = ui, server = server)