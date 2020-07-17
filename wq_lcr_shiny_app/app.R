library("shiny")
library("tidyverse")
library("shinythemes")
library("ggplot2")
library("lubridate")
library("naniar")
library("zoo")

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



#### Front of the app, the user interface

ui <- fluidPage(
  #Selecting a theme from pre-made shiny themes
  theme = shinytheme("united"),
  
  #Creating the design and designations of the side bar, width=4 is too large and will not display the graphs correctly
  sidebarLayout(
    sidebarPanel(
      
      width = 4,
      
      h4("MAP, DATA LOGGER & ROLLING AVERAGES TAB OPTIONS"),
      
      p("(hourly observations collected starting September 2017)"),
      
      dateRangeInput("date",
                     label =h4('DATE RANGE'),
                     start ="2020-03-10" , end = "2020-04-10"),
      
      selectInput("site1", label= h4("SITE"), 
                  choices=c(unique(wq$Site) %>% sort()), selected = 1),
      
      selectInput("site2", label=h4("COMPARISON SITE"), 
                  choices=c("None" = 0, unique(wq$Site) %>% sort()), selected = 6),
      
      
      radioButtons("variable",
                   label = h5("OBSERVATIONS"),
                   choices = list("Salinity (ppt)" = "Salinity",
                                  "Conductivity (mS/cm)"= "Conductivity",
                                  "Temperature (C)" = "Temperature"),
                   selected = "Salinity"),
      
      radioButtons("temp_res",
                   label = "TEMPORAL RESOLUTION",
                   choices = list("Hourly" = "Hourly",
                                  "Daily Mean" = "Daily"),
                   selected = "Hourly"),
      
      
      p("Please only select if you are viewing 'Hourly' in TEMPORAL RESOLUTION, not representational in 'Daily Mean'"),
      
      checkboxInput("overlay", 
                    label = "Overlay point sample data?",
                    value = T),
      
      
      h4("POINT SAMPLING TAB OPTIONS"),
      p("(updated every 2 weeks for YSI, and every 4 months for LAKEWATCH)"),
      
      
      radioButtons("variable2",
                   label = h5("OBSERVATIONS"),         
                   choices = list("Salinity (ppt)" = "Salinity",
                                  "Conductivity (mS/cm)"= "Conductivity",
                                  "Temperature (C)" = "Temperature",
                                  "Phosphorus (ug/L)" = "Phosphorus",
                                  "Nitrogen (ug/L)" = "Nitrogen",
                                  "Color (Pt-Co Units)" = "Color",
                                  "Secchi (m)" = "Secchi"),
                   selected = c("Salinity"))),
    
    
    # The display of the main panel
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("MAP OF SELECTED SITES", uiOutput("map", height = "800px")),
                  tabPanel("DATA LOGGER MEASUREMENTS", plotOutput("sensorplot", height = "600px")),
                  tabPanel("POINT SAMPLING DATA", plotOutput("labplot", height = "600px")),
                  tabPanel("ROLLING AVERAGES", plotOutput("rollingplot", height = "600px"))
                  

      
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
        sensorplot <- sensorplot + 
          geom_point(data = lab1, aes(x = date(Date), y = Measure, colour = Sensor_Type), shape = 17, size = 5) +
          scale_color_manual(name = "Method", values = c("red", "blue"))
      }
      
    }
    
    sensorplot
  })
  
  output$labplot<-renderPlot({
    site1 <- as.numeric(input$site1)
    site2 <- as.numeric(input$site2)
    startDate <- paste(input$date[1], "00:00:00") %>% ymd_hms(tz="UTC")
    endDate <- paste(input$date[2], "23:00:00") %>% ymd_hms(tz="UTC")
    
    lab1 <- lab %>% 
      filter(Site == site1 | Site == site2,
             Date >= startDate & Date <= endDate) %>% 
      select(Site, Date, Measure = input$variable2, Sensor_Type)
    
    
    # Same thing as we did for sensorplot to force sequence of display
    lab1$Site <- factor_site_seq(lab1$Site, site1, site2)
    
    # Use similar trick as we did in sensorplot (e.g. build a df)
    labplot <- ggplot(lab1, aes(x = Date, y = Measure, colour = Sensor_Type)) +
      ylab(input$variable2) +
      geom_point(shape = 17, size = 5) +
      scale_color_manual(name = "Method", values = c("red", "blue")) +
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
        days_7 = zoo::rollmean(Measure, k = 7, fill = NA),
        days_15 = zoo::rollmean(Measure, k = 15, fill = NA),
        days_21 = zoo::rollmean(Measure, k = 21, fill = NA)) %>% 
      dplyr::ungroup()
    
    
    wq3 <- wq2 %>% 
      tidyr::pivot_longer(names_to = "rolling_avg", 
                          values_to = "rolling_value", 
                          cols = c(days_7, days_15, days_21)) 
    
    wq3$rolling_avg <- factor(wq3$rolling_avg, c("days_7", "days_15", "days_21"))
    
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
})

# Run the application 
shinyApp(ui = ui, server = server)