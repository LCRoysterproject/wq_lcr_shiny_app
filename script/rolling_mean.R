library(zoo) # moving averages        
library(tidyverse) # all tidyverse packages
library(hrbrthemes) # themes for graphs
library(socviz) # %nin%
library(geofacet) # maps
library(usmap) # lat and long
library(socviz) # for %nin%
library(ggmap) # mapping


wq <- read.csv("wq_lcr_shiny_app/data/wq.csv", header= T) 

wq[wq == -999] <- NA

wq$Date1<- ymd(wq$Obs_Date, tz="UTC") 


wq<-wq %>%
  dplyr::group_by(Date1, Site) %>% 
  dplyr::mutate(m = mean(Salinity)) %>% 
  dplyr::ungroup()


wq1<-wq %>%
  dplyr::group_by(Date1, Site) %>% 
  dplyr::mutate(
    salinity_7da = zoo::rollmean(Salinity, k = 7, fill = NA),
    salinity_15da = zoo::rollmean(Salinity, k = 15, fill = NA),
    salinity_21da = zoo::rollmean(Salinity, k = 21, fill = NA)) %>% 
  dplyr::ungroup()


wq2 <- wq1 %>% 
  tidyr::pivot_longer(names_to = "new_conf_av_key", 
                      values_to = "new_conf_av_value", 
                      cols = c(salinity_7da, salinity_15da, salinity_21da)) %>% 
  filter(Date1 > "2020-01-01" & Date1 < "2020-04-01") %>% 
  filter (Site == "1" | Site == "6")


wq2 %>% 
  drop_na("new_conf_av_value") %>%
ggplot(aes(x = Date1)) +
 #geom_line(color= "black") + 
  geom_line(aes(y = new_conf_av_value, color = new_conf_av_key, group= new_conf_av_key), size = 1.1) +
  facet_wrap(~as.factor(Site) + new_conf_av_key, ncol= 3) +
  theme_bw()



