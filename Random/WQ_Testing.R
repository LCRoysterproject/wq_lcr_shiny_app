test<-wq %>% 
  filter(Site == 12 & Date > "2021-05-01 00:00:00") %>% as.data.frame()


library(plotly)

plot_ly(x=~Date,y=~Salinity,data=test,mode="lines")

plot(Salinity~Date,data=test,type="l",bty="l")

library(dplyr)
sites<-rev(c(12))

for(j in 1:length(sites)){
  
  test<-wq %>% 
    filter(Site == sites[j] & Date > "2021-05-01 00:00:00") %>% as.data.frame()
  tested_data<-test %>% dplyr::group_by(Obs_Date) %>% dplyr::summarize("Mean_Sal"=mean(Salinity)) %>% as.data.frame()
  roll_data<-zoo(tested_data$Mean_Sal,tested_data$Obs_Date)



  average_values<-c(1,3,5,7,10,14)
  
  par(mfrow=c(3,2))
  for(i in 1:length(average_values)){
    rolled_data<-rollmean(roll_data,k=average_values[i])
    roll_mean_data<-data.frame("Value"=as.vector(rolled_data),"Date"=as.character(time(rolled_data)))
    roll_mean_data$Date2<-paste(roll_mean_data$Date,"00:00:00",sep=" ")
    roll_mean_data$Date3<-as.POSIXct(roll_mean_data$Date2,format = c("%Y-%m-%d %H:%M:%S"))
    
    # dip_points<-c(mean(roll_mean_data$Value)*0.75)
    # rows<-roll_mean_data$Value <= dip_points[1]
    # flag_dates<-c(roll_mean_data$Date[which(rows == T)[1]],roll_mean_data$Date[which(rows == T)[length(which(rows == T))]])
    # flag_dates<-as.POSIXct(flag_dates,format = c("%Y-%m-%d"))
  
    plot(Salinity~Date,data=test,type="l",bty="l",
         main=paste("Rolling Average (k = ",average_values[i],")",sep=""),
         col=adjustcolor("black",alpha=0.5))
    lines(roll_mean_data$Value~roll_mean_data$Date3,col="red")
    # abline(v=flag_dates[1],col="green")
    # abline(v=flag_dates[2],col="green")
  }
  mtext(text = paste("WQ Site:",sites[j],sep=" "),outer=T,side=3,line = -2)
}


colors<-rainbow(length(average_values))

test<-wq %>% 
  filter(Site == 3 & Date > "2021-05-01 00:00:00") %>% as.data.frame()
tested_data<-test %>% dplyr::group_by(Obs_Date) %>% dplyr::summarize("Mean_Sal"=mean(Salinity)) %>% as.data.frame()
roll_data<-zoo(tested_data$Mean_Sal,tested_data$Obs_Date)

par(mfrow=c(1,1))
for(i in 1:length(average_values)){
  rolled_data<-rollmean(roll_data,k=average_values[i])
  roll_mean_data<-data.frame("Value"=as.vector(rolled_data),"Date"=as.character(time(rolled_data)))
  roll_mean_data$Date2<-paste(roll_mean_data$Date,"00:00:00",sep=" ")
  roll_mean_data$Date3<-as.POSIXct(roll_mean_data$Date2,format = c("%Y-%m-%d"))
  
  if(i == 1){
    plot(Salinity~Date,data=test,type="l",bty="l",
         #main=paste("Rolling Average (k = ",average_values[i],")",sep=""),
         col=adjustcolor("black",alpha=0.5))
  }
  lines(roll_mean_data$Value~roll_mean_data$Date3,col=colors[i])
}
legend("topleft",legend=average_values,col=colors,lty=1,bty = "n",ncol=2,title = "Rolling Average")
