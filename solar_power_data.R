#library(plyr)
require(dplyr)
require(tidyr)
require(reshape2)
library(openxlsx)
library(ggplot2)
library(ggjoy)
library(lubridate)
library(zoo)
library(viridis)
library(ggmap)
    
  solar_data <- read.xlsx(xlsxFile = "solar_system.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  sub_samp <- solar_data
  names(sub_samp) <- tolower(names(sub_samp))
  sub_samp$`beam.irradiance.(w/m^2)`<- NULL
  sub_samp$`diffuse.irradiance.(w/m^2)`<- NULL
  sub_samp$`wind.speed.(m/s)`<- NULL
  sub_samp$`plane.of.array.irradiance.(w/m^2)`<- NULL
  sub_samp$`cell.temperature.(c)`<- NULL
  names(sub_samp) <- c("month","day","hour","temperature","dc.output","ac.output","load_750",	"load_750_W")
  sub_samp$hour <- as.numeric(sub_samp$hour)
  sub_samp$hour <- as.factor(sub_samp$hour)
  sub_samp$month <- as.numeric(sub_samp$month)
  sub_samp$month <- as.factor(sub_samp$month)
  sub_samp<- arrange(sub_samp,month,day,hour)
  
  
  df1 <- sub_samp %>% group_by(month,hour) %>% summarise(hourly_gen=sum(ac.output)/1000)
  png(file="solar_gen.png", width = 1400, height = 750)
  ggplot(df1, aes(hour,hourly_gen,group=month,colour=month)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    labs(colour="Month") +
    scale_color_viridis(discrete=TRUE)+  
    theme_minimal()+theme(
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 16, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Hourly Solar System Generation (Monthly total, kWh)",x="Hour of the Day",
             title="Time Series of Hourly Solar Generation (Monthly Total kWh, estimated)",
             subtitle="Source: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  
  dev.off()
  
  
  df1 <-sub_samp %>% group_by(month) %>% summarise(monthly_gen=sum(ac.output)/1000)
  
  png(file="solar_monthly_gen.png", width = 1400, height = 750)
  ggplot(df1,aes(month,monthly_gen,group = 1)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    scale_color_viridis(discrete=TRUE)+ 
    expand_limits(y = 0)+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Monthly Solar System Generation (total, kWh)",x="Month",
               title="Time Series of Monthly Solar Generation (Total kWh, estimated)",
               subtitle="Source: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  
  dev.off()
    
  #test_data<-merge(sub_samp,res_data)  
  df1 <- sub_samp %>% group_by(month,hour) %>% summarise(hourly_gen=sum(ac.output)/1000,hourly_net=sum(load_750-ac.output/1000))
  png(file="solar_hourly_net.png", width = 1400, height = 750)
  ggplot(df1, aes(hour,hourly_net,group=month,colour=month)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    labs(colour="Month") +
    scale_color_viridis(discrete=TRUE)+  
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Consumption Net of Solar Generation (Monthly total, kWh)",x="Hour of the Day",
               title="Time Series of Hourly Consumption Net of Solar Generation (Monthly Total kWh, estimated)",
               subtitle="Source: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  
  dev.off()
  df1 <- sub_samp %>% group_by(month) %>% summarise(monthly_gen=sum(ac.output)/1000,monthly_net=sum(load_750-ac.output/1000))
  
  png(file="solar_monthly_net.png", width = 1400, height = 750)
  ggplot(df1,aes(month,monthly_net,group = 1)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    scale_color_viridis(discrete=TRUE)+ 
    expand_limits(y = 0)+
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Monthly Net Consumption (total, kWh)",x="Month",
               title="Time Series of Monthly Consumption Net of Solar Generation (Total kWh, estimated)",
               subtitle="Source: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  
  dev.off()

  df2 <- sub_samp
  df2$td <- df2$load_750-df2$ac.output/1000
  df2$td[df2$td < 0] <- 0
  df2$bill <- df2$load_750-df2$ac.output/1000
  df1 <- df2 %>% group_by(month) %>% summarise(monthly_td=sum(td),monthly_net=sum(load_750-ac.output/1000),monthly_load=sum(load_750))
  df1 <- melt(df1, id.vars=c("month"))
  
  png(file="monthly_td.png", width = 1400, height = 750)
  ggplot(df1,aes(month,value,group = variable,colour = variable)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    expand_limits(y = c(-500,1000))+
    scale_color_viridis(labels=c("Monthly Billable T&D","Monthly Billable Energy", "Monthly Estimated Load"),discrete=TRUE)+   
      theme_minimal()+theme(
      legend.title=element_blank(),
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Monthly total (kWh)",x="Month",
               title="Monthly Billable Consumption and Delivery Loads (kWh, estimated)",
               subtitle="Source: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  
  dev.off()

  
  prices <- read.xlsx(xlsxFile = "solar_system.xlsx", sheet = 3, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  prices$price<-prices$price/1000
  test_data<-merge(sub_samp,prices)  
  df2 <- test_data
  df2$td <- df2$load_750-df2$ac.output/1000
  df2$td[df2$td < 0] <- 0
  df2$tdcharge <- df2$td*0.06584615384615384615384615384615
  df2$altbill <- (df2$load_750)*(df2$price+0.06584615384615384615384615384615)
  df2$bill <- (df2$load_750-df2$ac.output/1000)*df2$price+df2$td*0.06584615384615384615384615384615
  df2$loadcharge <- (df2$load_750-df2$ac.output/1000)*df2$price
  df2$altloadcharge <- (df2$load_750)*df2$price
  df2$savings=df2$altbill-df2$bill
  df1 <- df2 %>% group_by(month) %>% summarise(monthly_td=sum(tdcharge),monthly_load=sum(loadcharge),bill=sum(bill),altbill=sum(altbill))
  df1 <- melt(df1, id.vars=c("month"))
  
  
  
  png(file="monthly_billing.png", width = 1400, height = 750)
  ggplot(df1,aes(month,value,group = variable,colour = variable)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    expand_limits(y = c(-20,120))+
    scale_color_viridis(labels=c("Monthly Billable T&D","Monthly Billable Energy", "Monthly Estimated Bill", "Monthly Estimated Bill w/o Solar"),discrete=TRUE)+   
    theme_minimal()+theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Monthly total ($)",x="Month",
               title="Monthly Billable Consumption and Delivery Loads ($/mth, estimated)",
               subtitle="Source: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  
  dev.off()  

  df1 <- df2 %>% group_by(month) %>% summarise(monthly_savings=sum(savings))
  df1 <- melt(df1, id.vars=c("month"))

    png(file="monthly_savings.png", width = 1400, height = 750)
  ggplot(df1,aes(month,value,group = 1,colour = 1)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    expand_limits(y = c(-20,120))+
    scale_color_viridis()+   
    theme_minimal()+theme(
      legend.position = "none",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Monthly savings ($)",x="Month",
               title="Monthly On-Bill Savings ($/mth, estimated)",
               subtitle="Source: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  
  dev.off()     
    
#using 2025 values
  prices <- read.xlsx(xlsxFile = "solar_system.xlsx", sheet = 3, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  prices$price<-prices$price_2025/1000
  test_data<-merge(sub_samp,prices)  
  df2 <- test_data
  df2$td <- df2$load_750-df2$ac.output/1000
  df2$td[df2$td < 0] <- 0
  df2$tdcharge <- df2$td*0.06584615384615384615384615384615*1.035^8
  df2$altbill <- (df2$load_750)*(df2$price+0.06584615384615384615384615384615*1.035^8)
  df2$bill <- (df2$load_750-df2$ac.output/1000)*df2$price+df2$td*0.06584615384615384615384615384615*1.035^8
  df2$loadcharge <- (df2$load_750-df2$ac.output/1000)*df2$price
  df2$altloadcharge <- (df2$load_750)*df2$price
  df2$savings=df2$altbill-df2$bill
  df1 <- df2 %>% group_by(month) %>% summarise(monthly_td=sum(tdcharge),monthly_load=sum(loadcharge),bill=sum(bill),altbill=sum(altbill))
  df1 <- melt(df1, id.vars=c("month"))
  
  df1 <- df2 %>% group_by(month) %>% summarise(monthly_savings=sum(savings))
  df1 <- melt(df1, id.vars=c("month"))
  
  png(file="monthly_savings_2025.png", width = 1400, height = 750)
  ggplot(df1,aes(month,value,group = 1,colour = 1)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    expand_limits(y = c(-20,120))+
    scale_color_viridis()+   
    theme_minimal()+theme(
      legend.position = "none",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Monthly savings ($)",x="Month",
               title="Monthly On-Bill Savings ($/mth, estimated)",
               subtitle="Source: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  
  dev.off()    
  
  
  
  
  #Code is a copy of 'It brings me ggjoy' http://austinwehrwein.com/data-visualization/it-brings-me-ggjoy/

  
  df1 <- sub_samp %>% group_by(month,day) %>% summarise(hourly_gen=sum(ac.output)/1000)
  mins<-min(df1$hourly_gen)
  maxs<-max(df1$hourly_gen)
  ggplot(df1,aes(x =hourly_gen,y=month,height=..density..))+
    geom_joy(scale=1) +
    scale_x_continuous(limits = c(mins,maxs))+
  scale_color_viridis()+   
    theme_minimal()+theme(
      legend.position = "none",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Density by Month",x="Daily Power Generation (kWh)",
               title="Daily Power Generation density, estimated)",
               subtitle="Source: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  
  
  
#EIA Climate Data
  EIA_climate <- read.xlsx(xlsxFile = "EIA_climate.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
  EIA_climate$Date <- ymd(EIA_climate$Date)
  EIA_climate$month <- month(EIA_climate$Date)
  EIA_climate$day <- day(EIA_climate$Date)
  EIA_climate$year <- year(EIA_climate$Date)
  EIA_climate$Date_ID <- as.yearmon(EIA_climate$Date)
  EIA_climate$MDH <- paste(EIA_climate$month,EIA_climate$day,EIA_climate$Hour,sep="-")
  solar_data$MDH <- paste(solar_data$Month,solar_data$Day,solar_data$Hour+1,sep="-")
  all_data<-merge(solar_data,EIA_climate,by="MDH")
  
 
  all_data<-arrange(all_data,Date,Hour.x)
  all_data$Tot.IRR <- all_data$Global.Horiz.Irr+all_data$Direct.Normal.Irr+all_data$Diff.Horiz.Irr
  all_data$Avg.IRR <- all_data$`Beam.Irradiance.(W/m^2)`+ all_data$`Diffuse.Irradiance.(W/m^2)`

  sub_samp <-subset(all_data, year==2013)
  sub_samp <-subset(sub_samp, month==5)
  sub_samp$net.to.grid <- -sub_samp$`AC.System.Output.(W)`+sub_samp$`750_load_W`
  df1<-melt(sub_samp,id=c("Day","Hour.y"),measure.vars = c("net.to.grid"))
  
  png(file="may_net.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(df1,aes(as.factor(Hour.y),value,group=interaction(Day,variable),colour = Day)) +
    geom_line(size=1.5) +
    scale_fill_brewer()+
          theme_minimal()+theme(
      legend.position = "none",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Hourly net load (W)",x="Hour",
               title="Net May load for a prototype Edmonton house",
               subtitle="Assumptions: 750kWh annual load, 7.6kW solar array\nSource: NREL PVWatts for Edmonton, Graph by Andrew Leach")
  dev.off()
  
  
  sub_samp <-subset(all_data, year==2005)
  sub_samp <-subset(sub_samp, month==12)
  sub_samp$net.to.grid <- -sub_samp$`AC.System.Output.(W)`+sub_samp$`750_load_W`
  df1<-melt(sub_samp,id=c("Day","Hour.y"),measure.vars = c("net.to.grid"))
  
  png(file="dec_net.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(df1,aes(as.factor(Hour.y),value,group=interaction(Day,variable),colour = Day)) +
    geom_line(size=1.5) +
    scale_fill_brewer()+
    theme_minimal()+theme(
      legend.position = "none",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 16, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Hourly net load (W)",x="Hour",
               title="Net December Load for a prototype Edmonton house",
             subtitle="Assumptions: 750kWh annual load, 7.6kW solar array\nSource: NREL PVWatts for Edmonton, Graph by Andrew Leach") 
  dev.off()
  
  sub_samp <-subset(all_data, year==2005)
  sub_samp <-subset(sub_samp, month==5)
  sub_samp$net.to.grid <- sub_samp$`AC.System.Output.(W)`-sub_samp$`750_load_W`
  #change net-to-grid to a kWh
  sub_samp$net.to.grid.kW <- sub_samp$net.to.grid/1000
  
  #use total meters in YEG=152184
  #sub_samp$net_20<-.8*sub_samp$YEG_Res_Load-.2*152184*sub_samp$net.to.grid.kW
  sub_samp$net_10<-(.9*sub_samp$YEG_Res_Load-.1*152184*sub_samp$net.to.grid.kW)/1000
  sub_samp$net_20<-(.8*sub_samp$YEG_Res_Load-.2*152184*sub_samp$net.to.grid.kW)/1000
  sub_samp$net_50<-(.5*sub_samp$YEG_Res_Load-.5*152184*sub_samp$net.to.grid.kW)/1000
  #sub_samp$net.to.grid <- sub_samp$`AC.System.Output.(W)`-sub_samp$`750_load_W`
  
  #add in T&D billings
  sub_samp$td.kW<-sub_samp$net.to.grid.kW
  sub_samp$td.kW[sub_samp$td.kW>0] <- 0
  sub_samp$td_net_0<-(.9*sub_samp$YEG_Res_Load-.1*152184*sub_samp$td.kW)/1000
  sub_samp$td_net_10<-(.9*sub_samp$YEG_Res_Load-.1*152184*sub_samp$td.kW)/1000
  sub_samp$td_net_20<-(.8*sub_samp$YEG_Res_Load-.2*152184*sub_samp$td.kW)/1000
  sub_samp$td_net_50<-(.5*sub_samp$YEG_Res_Load-.5*152184*sub_samp$td.kW)/1000
  
  

    df1<-melt(sub_samp,id=c("Day","Hour.y"),measure.vars = c("net_20"))
  
  png(file="yeg_net_20.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(df1,aes(Hour.y,value,group=interaction(Day,variable),colour = Day)) +
    geom_line(size=1.5) +
    scale_fill_brewer()+
    theme_minimal()+theme(
      legend.position = "right",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black")
    )+    labs(y="Hourly net load (MW)",x="Hour",
               title="Estimated Edmonton Residental Net Load for May\n20% of households with solar (MW)",
               subtitle="Source: NREL PVWatts and EPCOR Data, Graph by Andrew Leach")
  dev.off()
  
  df1<-melt(sub_samp,id=c("Day","Hour.y"),measure.vars = c("net_50"))
  
  png(file="yeg_net_50.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(df1,aes(Hour.y,value,group=interaction(Day,variable),colour = Day)) +
    geom_line(size=1.5) +
    scale_fill_brewer()+
    theme_minimal()+theme(
      legend.position = "right",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black")
    )+    labs(y="Hourly net load (MW)",x="Hour",
               title="Estimated Edmonton Residental Net Load for May\n50% of households with solar (MW)",
               subtitle="Source: NREL PVWatts and EPCOR Data, Graph by Andrew Leach")
  dev.off()
  

  #T&D
  
  
  sub_samp <-subset(all_data, year==2005)
  sub_samp$net.to.grid <- sub_samp$`AC.System.Output.(W)`-sub_samp$`750_load_W`
  #change net-to-grid to a kWh
  sub_samp$net.to.grid.kW <- sub_samp$net.to.grid/1000
  
  #use total meters in YEG=152184
  #sub_samp$net_20<-.8*sub_samp$YEG_Res_Load-.2*152184*sub_samp$net.to.grid.kW
  sub_samp$net_10<-(.9*sub_samp$YEG_Res_Load-.1*152184*sub_samp$net.to.grid.kW)/1000
  sub_samp$net_20<-(.8*sub_samp$YEG_Res_Load-.2*152184*sub_samp$net.to.grid.kW)/1000
  sub_samp$net_50<-(.5*sub_samp$YEG_Res_Load-.5*152184*sub_samp$net.to.grid.kW)/1000
  #sub_samp$net.to.grid <- sub_samp$`AC.System.Output.(W)`-sub_samp$`750_load_W`
  
  #add in T&D billings
  sub_samp$td.kW<-sub_samp$net.to.grid.kW
  sub_samp$td.kW[sub_samp$td.kW>0] <- 0
  sub_samp$td_net_0<-(sub_samp$YEG_Res_Load)/1000
  sub_samp$td_net_10<-(.9*sub_samp$YEG_Res_Load-.1*152184*sub_samp$td.kW)/1000
  sub_samp$td_net_20<-(.8*sub_samp$YEG_Res_Load-.2*152184*sub_samp$td.kW)/1000
  sub_samp$td_net_50<-(.5*sub_samp$YEG_Res_Load-.5*152184*sub_samp$td.kW)/1000 
  
  
  sub_samp2 <-subset(sub_samp, month==5)
  df1<-melt(sub_samp2,id=c("Day","Hour.y"),measure.vars = c("td_net_20"))
  
  
  
  png(file="td_may_net_20.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(df1,aes(Hour.y,value,group=interaction(Day,variable),colour = Day)) +
    geom_line(size=1.5) +
    scale_fill_brewer()+
    theme_minimal()+theme(
      legend.position = "right",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black")
    )+    labs(y="Hourly billable load (MW)",x="Hour",
               title="Estimated Edmonton Residental Billable Deliveries for December (MW)\n 20% of households with solar",
               subtitle="Source: NREL PVWatts and EPCOR Data, Graph by Andrew Leach")
  
  dev.off()
  
  
  sub_samp2 <- sub_samp %>% group_by(month) %>% summarise(td_0=sum(td_net_0),td_10=sum(td_net_10),td_20=sum(td_net_20),td_50=sum(td_net_50))
  df1<-melt(sub_samp2,id=c("month"),measure.vars = c("td_0","td_10","td_20","td_50"))
  df1$month<-factor(month.abb[df1$month], levels=month.abb[1:12]) 
  my_pal<-brewer.pal(n = 5, name = 'YlGnBu')[-1]
  
  png(file="yeg_td.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(df1,aes(as.factor(month),value,group=variable,colour = variable)) +
    geom_line(size=1.5) +
    scale_colour_viridis("Solar\nPenetration",labels=c("Current","10% solar","20% solar","50% solar"),discrete = TRUE)+
    theme_minimal()+theme(
      legend.position = "right",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black")
    )+    labs(y="Monthly billable deliveries (MWh)",x="Month",
               title="Estimated Edmonton Residental Billable Distribution and Transmission Loads\nwith High Solar Penetration",
               subtitle="Source: NREL PVWatts and EPCOR Data, Graph by Andrew Leach")
  dev.off()
  
  
  
  
  sub_samp <-subset(all_data, year==2005)
  
  
  sub_samp <-subset(sub_samp, month==12)
  sub_samp$net.to.grid <- sub_samp$`AC.System.Output.(W)`-sub_samp$`750_load_W`
  #change net-to-grid to a kWh
  sub_samp$net.to.grid.kW <- sub_samp$net.to.grid/1000
  
  #use total meters in YEG=152184
  #sub_samp$net_20<-.8*sub_samp$YEG_Res_Load-.2*152184*sub_samp$net.to.grid.kW
  sub_samp$net_10<-(.9*sub_samp$YEG_Res_Load-.1*152184*sub_samp$net.to.grid.kW)/1000
  sub_samp$net_10<-(.9*sub_samp$YEG_Res_Load-.1*152184*sub_samp$net.to.grid.kW)/1000
  sub_samp$net_20<-(.8*sub_samp$YEG_Res_Load-.2*152184*sub_samp$net.to.grid.kW)/1000
  sub_samp$net_50<-(.5*sub_samp$YEG_Res_Load-.5*152184*sub_samp$net.to.grid.kW)/1000
  #sub_samp$net.to.grid <- sub_samp$`AC.System.Output.(W)`-sub_samp$`750_load_W`

  #add in T&D billings
  sub_samp$td.kW<-sub_samp$net.to.grid.kW
  sub_samp$td.kW[sub_samp$td.kW>0] <- 0
  sub_samp$td_net_0<-(.9*sub_samp$YEG_Res_Load-.1*152184*sub_samp$td.kW)/1000
  sub_samp$td_net_10<-(.9*sub_samp$YEG_Res_Load-.1*152184*sub_samp$td.kW)/1000
  sub_samp$td_net_20<-(.8*sub_samp$YEG_Res_Load-.2*152184*sub_samp$td.kW)/1000
  sub_samp$td_net_50<-(.5*sub_samp$YEG_Res_Load-.5*152184*sub_samp$td.kW)/1000
  
  
  
  df1<-melt(sub_samp,id=c("Day","Hour.y"),measure.vars = c("net_20"))
  pal_val<-colorRampPalette(brewer.pal(9,'Blues'))(31)  
  
  png(file="yeg_dec_net_20.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(df1,aes(Hour.y,value,group=interaction(Day,variable),colour = Day)) +
    geom_line(size=1.5) +
    scale_fill_brewer()+
    theme_minimal()+theme(
      legend.position = "right",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black")
    )+    labs(y="Hourly net load (MW)",x="Hour",
               title="Estimated Edmonton Residental Net Load for December (MW)\n 20% of households with solar",
               subtitle="Source: NREL PVWatts and EPCOR Data, Graph by Andrew Leach")
  dev.off()
  
  
  
  df1<-melt(sub_samp,id=c("Day","Hour.y"),measure.vars = c("net_50"))
  
  png(file="yeg__dec_net_50.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(df1,aes(Hour.y,value,group=interaction(Day,variable),colour = Day)) +
    geom_line(size=1.5) +
    scale_fill_brewer()+
    theme_minimal()+theme(
      legend.position = "right",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black")
    )+    labs(y="Hourly net load (MW)",x="Hour",
               title="Estimated Edmonton Residental Net Load for December (MW)\n 50% of households with solar",
               subtitle="Source: NREL PVWatts and EPCOR Data, Graph by Andrew Leach")
  dev.off()
  
  

  df1<-melt(sub_samp,id=c("Day","Hour.y"),measure.vars = c("td_net_20"))
  
  png(file="td_dec_net_20.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(df1,aes(Hour.y,value,group=interaction(Day,variable),colour = Day)) +
    geom_line(size=1.5) +
    scale_fill_brewer()+
    theme_minimal()+theme(
      legend.position = "right",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 12, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 14,face = "bold"),
      axis.text = element_text(size = 14,face = "bold", colour="black")
    )+    labs(y="Hourly billable load (MW)",x="Hour",
               title="Estimated Edmonton Residental Billable Deliveries for December (MW)\n 20% of households with solar",
               subtitle="Source: NREL PVWatts and EPCOR Data, Graph by Andrew Leach")
  dev.off()
  
  
  
#CWEEDS DATA READER
  
  
  
  