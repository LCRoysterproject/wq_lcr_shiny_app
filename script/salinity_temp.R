wq <- read.csv("wq_lcr_shiny_app/data/wq.csv", header= T) %>%
  filter(Site != 0) 

#Change class? of date and year
wq$Obs_Date <- as.POSIXct (wq$Obs_Date)

wq$year <- strftime(wq$Obs_Date, format = "%Y")

#Remove 2017
wq <- wq %>%
  subset(year %in% c('2018','2019','2020')) %>%
  subset(Site %in% c(1:9))

#Calculate mean salinity by date and site
wq2 <- aggregate(Salinity ~ Obs_Date + Site, FUN = mean, data = wq) 

#Calculate the standard deviations for salinity by date and site
wq3 <- aggregate(Salinity ~ Obs_Date + Site, FUN = sd, data = wq)  

#Rename salinity sd to prevent redundant names
colnames(wq3)[colnames(wq3)=="Salinity"] <- "sal.sd"

#Add salinity sd vector to df with salinity mean values
wq2$sal.sd <- wq3$sal.sd

#Change the name of the mean salinity column
colnames(wq2)[colnames(wq2)=="Salinity"] <- "sal.mean"

#Factoring the site in the order that they are spatially
wq2$Site <- factor(wq2$Site, levels = c("6", "1", "7", "5", "2", "8","4", "3", "9"))

# Water discharge carpentry (dynamically updating local discharge file)
station = '02323500' 
stinfo  = siteInfo(station)
dis <- read_rds("wq_lcr_shiny_app/data/dis.rds")
if (max(dis$dates) < (Sys.Date() - 5)) {
  sdate <- max(dis$dates) + 1
  newdis <- importDVs(staid = station, code = '00060', stat = '00003', 
                      sdate= as.character(sdate))
  dis <- bind_rows(dis, newdis) %>%
    distinct() # In case there's repeated rows due to code failure
  write_rds(dis, "wq_lcr_shiny_app/data/dis.rds")
}

# Start and end dates for all figures
startDate <- ymd_hms("2020-06-01 00:00:00")
endDate <- ymd_hms("2020-07-15 23:00:00")

dis2 <- dis %>%
  mutate(Datetime = paste(dates, " 12:00:00") %>% ymd_hms()) %>%
  filter(Datetime >= startDate & Datetime <= endDate)

dis3 <- dis2 %>%
  expand(Site=unique(wq2$Site), dates) %>%
  left_join(dis2) %>%
  mutate(Measure = "Discharge", maxVal = val) %>%
  filter(dates >= startDate & dates <= endDate) %>% 
  select(Site, d2 = dates, Measure, maxVal)


lims <- as.POSIXct(strptime(c("2019-10-31 00:00:00","2020-03-01 23:00:00"), format = "%Y-%m-%d"))   


ggplot() +
  
  geom_path(data=dis3, aes(x= as.POSIXct(d2), y=maxVal/400, color= "Discharge", group= 1), size=2, alpha=0.6) +
  
  geom_ribbon(data= wq2, aes( x= Obs_Date, ymin = sal.mean - (1.96*sal.sd), ymax = sal.mean + (1.96*sal.sd), group= 1), fill = "grey70", alpha=0.5) +
  
  #geom_point(data= wq2, aes(x = Obs_Date, y = sal.mean, color="Salinity", group= 1)) +
  
  geom_path(data= wq2, aes(x = Obs_Date, y = sal.mean, color="Salinity", group= 1)) +
  
  xlab("Date") +
  
  ylab("Salinity (ppt)") +
  
  scale_y_continuous(sec.axis = sec_axis(~.*400,name = "River Discharge (cfs)"),limits=c(-10,40),
                     labels = waiver())  +
  
  scale_x_datetime(limits = lims, date_labels = "%b %d %Y",expand = c(0, 0), breaks = "15 days") +
  
  scale_color_manual(name = "Legend", 
                    values = c("Discharge" = "cornflowerblue", 
                               "Salinity" = "black")) +
  
  coord_cartesian(ylim = c(0, 40)) +

  
  theme(legend.position=("top"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA) ,
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=17,face="bold"),
        plot.title =element_text(size=17, face='bold'),
        axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1, face= "bold"),
        axis.text.y = element_text(face= "bold"),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size = 15),
        panel.spacing = unit(1, "lines"),
        axis.line.y.right = element_line(color = "cornflowerblue"), 
        axis.ticks.y.right = element_line(color = "cornflowerblue"),
        axis.text.y.right = element_text(color = "cornflowerblue"),
        axis.title.y.right = element_text(colour = "cornflowerblue"))  +
  
  guides(fill = guide_legend(override.aes = list(colour = NULL, linetype = 0))) +
  
  facet_wrap(~ Site, ncol=3, labeller = label_both)

ggsave("wq_sal_20191031_20200301.png", dpi= 300 , width = 17, height = 10)




wq_temp <- wq %>%
  subset(year %in% c('2018','2019', '2020')) %>%
  subset(Site %in% c(1:9))

# Sensor water quality carpentry
#Calculate mean salinity by date and site
wq_temp2 <- aggregate(Temperature ~ Obs_Date + Site, FUN = mean, data = wq) 

#Calculate the standard deviations for salinity by date and site
wq_temp3 <- aggregate(Temperature ~ Obs_Date + Site, FUN = sd, data = wq)  

#Rename salinity sd to prevent redundant names
colnames(wq_temp3)[colnames(wq_temp3)=="Temperature"] <- "temp.sd"

#Add salinity sd vector to df with salinity mean values
wq_temp2$temp.sd <- wq_temp3$temp.sd

#Change the name of the mean salinity column
colnames(wq_temp2)[colnames(wq_temp2)=="Temperature"] <- "temp.mean"

wq_temp2$Site <- factor(wq_temp2$Site, levels = c("6", "1", "7", "5", "2", "8","4", "3", "9"))

 

lims <- as.POSIXct(strptime(c("2020-06-01 00:00:00","2020-07-15 23:00:00"), format = "%Y-%m-%d"))   

ggplot() +
  
  geom_line(data=dis3, aes(x= as.POSIXct(d2), y=maxVal/450, color= "Discharge"), size=2, alpha=0.6) +
  
  geom_ribbon(data= wq_temp2, aes( x= Obs_Date, ymin = temp.mean - 1.96*temp.sd, ymax = temp.mean + 1.96*temp.sd), fill = "#D55E00", alpha= 0.4) +
  
  
  geom_line(data= wq_temp2, aes(x = Obs_Date, y = temp.mean, color= "Temperature")) +
  
  xlab("Date") +
  
  ylab("Temperature (C)") +
  
  scale_y_continuous(sec.axis = sec_axis(~(.*450),name = "River Discharge (cfs)"),limits=c(0,40),
                     labels = waiver())  +
  
  scale_x_datetime(limits = lims, date_labels = "%b %d %Y",expand = c(0, 0), breaks = "7 days") +
  
  scale_color_manual(name = "Legend", 
                     values = c("Discharge" = "cornflowerblue", 
                                "Temperature" = "#D55E00")) +
  
  coord_cartesian(ylim = c(20, 35)) +
  
  
  theme(legend.position=("top"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA) ,
        panel.border = element_rect(color = "black", size = 1, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=17,face="bold"),
        plot.title =element_text(size=17, face='bold'),
        axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1, face= "bold"),
        axis.text.y = element_text(face= "bold"),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        strip.text = element_text(size = 15),
        panel.spacing = unit(1, "lines"),
        axis.line.y.right = element_line(color = "cornflowerblue"), 
        axis.ticks.y.right = element_line(color = "cornflowerblue"),
        axis.text.y.right = element_text(color = "cornflowerblue"),
        axis.title.y.right = element_text(colour = "cornflowerblue")) +
  
  #guides(fill = guide_legend(override.aes = list(colour = NULL, linetype = 0))) +
  
  facet_wrap(~ Site, ncol=3, labeller = label_both)


ggsave("wq_temp_20191031_20200301s.png", dpi= 300 ,  width = 20, height = 11)
