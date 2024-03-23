source("../andrew_base.R")


load("hourly_solar_data.RData")

load("solar_data.RData",.GlobalEnv) 

#clean data

sys_data<-sys_data %>% mutate(
  consumptionEnergy=ifelse(consumptionEnergy<0,NA,consumptionEnergy),
  generationEnergy=ifelse(generationEnergy<0,NA,generationEnergy),
  importedEnergy=ifelse(importedEnergy<0,NA,importedEnergy),
  exportedEnergy=ifelse(importedEnergy<0,NA,exportedEnergy))%>%
  fill(consumptionEnergy,generationEnergy,importedEnergy,exportedEnergy)%>%
  mutate(
  net_to_grid=consumptionEnergy-generationEnergy
)

hourly_data<-hourly_data %>% mutate(
  consumptionEnergy=ifelse(consumptionEnergy<0,NA,consumptionEnergy),
  generationEnergy=ifelse(generationEnergy<0,NA,generationEnergy),
  importedEnergy=ifelse(importedEnergy<0,NA,importedEnergy),
  exportedEnergy=ifelse(importedEnergy<0,NA,exportedEnergy))%>%
  fill(consumptionEnergy,generationEnergy,importedEnergy,exportedEnergy)%>%
  mutate(
    net_to_grid=consumptionEnergy-generationEnergy
  )%>%
  slice(-1)


#merge solar data with price data
load("../alberta_power/data/forecast_data.Rdata")

#make an he in the sys_data
sys_data$he_full<-as.POSIXct(paste(date(sys_data$start)," ",hour(sys_data$start)+1,":00",sep=""),"%Y-%m-%d %H:%M",tz = "America/Denver")

forecast_data$he_full<-as.POSIXct(paste(date(forecast_data$date)," ",forecast_data$he,":00",sep=""),"%Y-%m-%d %H:%M",tz = "America/Denver")
test_data<-merge(sys_data,forecast_data,by.x =c("he_full"),by.y=c("he_full"))

hourly_data$he_full<-as.POSIXct(paste(date(hourly_data$start)," ",hour(hourly_data$start)+1,":00",sep=""),"%Y-%m-%d %H:%M",tz = "America/Denver")
hourly_data<-merge(hourly_data,forecast_data,by.x =c("he_full"),by.y=c("he_full"))



house_data <- read.xlsx(xlsxFile = "hourly-house.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
house_data$Date<-as.POSIXct(paste(house_data$Year,"-",house_data$Month,"-",house_data$Day," ",house_data$Hour,":00",sep=""),format='%Y-%m-%d %H:%M')
#View(head(house_data))
#house_data<-NULL

solar_data <- read.xlsx(xlsxFile = "solar_system.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
solar_data$Date<-as.POSIXct(paste(2016,"-",solar_data$Month,"-",solar_data$Day," ",solar_data$Hour+1,":00",sep=""),format='%Y-%m-%d %H:%M')
solar_data$Hour<-solar_data$Hour+1
#solar_data<-solar_data %>% ungroup()



#assignment_data<-merge(house_data,solar_data,by=c("Date","Day","Hour","Month"),all.y = T)

#asst_data<-data.frame(assignment_data$Date,assignment_data$`Use.(kWh)`*750/527,assignment_data$`AC.System.Output.(W)`/1000,assignment_data$`750_load`)
#names(asst_data)<-c("Date","Use_kWh","gen_kWh","avg_load")


#work_data<-subset(hourly_data,start>=as.Date("2017-08-18"))
#work_data<-asst_data

#work_data<-assignment_data


work_data<-hourly_data
dat2 <- data.frame(seq.POSIXt(min(hourly_data$start),max(hourly_data$start),by="1 hour"))
names(dat2)[1]<-"start"

work_data<-merge(dat2,work_data,by=c("start"),all.x = T)
work_data<-work_data %>% mutate(month=month(start),hour=hour(start)+1,day=day(start),min=minute(start),day=as.numeric(day))
work_data<-merge(work_data,solar_data,by.x=c("month","hour","day"),by.y=c("Month","Hour","Day"),all.x = T)

work_data$weekday<-ifelse(wday(work_data$start) %in% seq(1,5),1,0)
work_data$wd<-factor(wday(work_data$start),levels = seq(1,7),labels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
work_data$he<-factor(work_data$hour,levels = unique(work_data$hour),labels=seq(1,24))
work_data$mth<-factor(work_data$month,levels = unique(work_data$month),labels=month.abb[])

work_data<-arrange(work_data,start)



#linearMod <- lm(consumptionEnergy^2 ~ month+hour+wd+wd*hour+month*hour+`Ambient.Temperature.(C)`+`Ambient.Temperature.(C)`^2, data=work_data)  # build linear regression model on full data
#linearMod <- lm(consumptionEnergy ~ wd+wd*he+month*he+month^2*he+month^3*he, data=work_data)  # build linear regression model on full data
#print(linearMod)
#test<-data.frame(predict(linearMod, work_data, se.fit = TRUE))
#work_data<-cbind(work_data,test)
 

#work_data$fit<-work_data$fit^(0.5)

work_data$generationEnergy[is.na(work_data$generationEnergy)]<-work_data$`AC.System.Output.(W)`[is.na(work_data$generationEnergy)]/1000
work_data$consumptionEnergy<-ifelse(is.na(work_data$consumptionEnergy),work_data$fit,work_data$consumptionEnergy)


ggplot(subset(work_data,month==6 & day<16))+
  geom_line(aes(start,`AC.System.Output.(W)`/1000),colour="blue")+
  geom_line(aes(start,generationEnergy),colour="red")

ggplot(subset(work_data,month==6 & day<16))+
  geom_line(aes(start,fit),colour="blue")+
  geom_line(aes(start,consumptionEnergy),colour="red")
  


work_data$exportedEnergy<-ifelse(work_data$generationEnergy-work_data$consumptionEnergy>0,work_data$generationEnergy-work_data$consumptionEnergy,0)
work_data$importedEnergy<-ifelse(work_data$generationEnergy-work_data$consumptionEnergy<0,-work_data$generationEnergy+work_data$consumptionEnergy,0)
work_data$net_to_grid<-work_data$consumptionEnergy-work_data$generationEnergy






ggplot(subset(work_data,month==6 & day<16))+
  geom_line(aes(start,exportedEnergy),colour="red",size=2)+
  geom_line(aes(start,-importedEnergy),colour="blue",size=2)+
  geom_line(aes(start,net_to_grid),colour="green",size=1)

names(work_data)[grep("net_to_grid",names(work_data))]<-"net_from_grid"

dat2<-work_data
work_data<-rbind(work_data,dat2)
work_data$start<-seq.POSIXt(min(work_data$start),min(work_data$start)+years(2)+hours(1),by="1 hour")
work_data$end<-work_data$start+hours(1)
#remove model results if you want
#work_data<-work_data[,-match(c("fit","se.fit","df","residual.scale"),names(work_data))]





home_battery_graph<-function(start_date,battery_size=10){
  lims <- c(start_date,start_date+weeks(2))
  #lims <- c(min(work_data$start), max(work_data$start))
  
  breaks<-seq.POSIXt(min(lims)+hours(12), max(lims), by="1 day")
  end_date<-start_date+weeks(2)
  #breaks<-seq.POSIXt(min(lims)+weeks(2), max(lims), by="1 month")
  
  top_panel<-ggplot(work_data) +
    geom_line(aes(start,net_imp,colour="Net Consumption"),size=2) +
    #geom_line(aes(start,net_from_grid,colour="Original Net"),linetype=1,size=2,alpha=.75)+
    geom_line(aes(start,batt_pwr,colour="Battery Flow"),size=2)+
    #geom_line(aes(start,batt_charge,colour="Battery Charge"),size=2)+
    #geom_line(aes(start,net_imp ,colour="Power/nDeliveries"),size=2)+
    #geom_line(aes(start,generationEnergy,colour="Solar/nGeneration"),size=2)+
    scale_colour_manual("",values = colors_tableau10(),labels = c("Battery Charge (+) or\nDischarge (-) State or Flows", "Purchases (+) or Sales (-)\nof Electricity","Purchases (+) or Sales (-) \nof Electricity w/o Battery"))+
    scale_y_continuous(limits = c(-6,+6))+
    #scale_color_viridis("",option="C",labels = c("Battery Charge (+) or\nDischarge (-) State or Flows", "Purchases (+) or Sales (-)\nof Electricity","Purchases (+) or Sales (-) \nof Electricity w/o Battery"),discrete = TRUE)+
    #scale_color_viridis("",labels = c("Battery\nCharge","Consumption","Net Electricity\nPurchases","Solar\nGeneration"),discrete = TRUE,option="C")+
    scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
    theme_minimal()+
    theme(legend.position="bottom",
          legend.margin=margin(c(0,0,0,0),unit="cm"),
          legend.text = element_text(colour="black", size = 12, face = "bold"),
    )+
    labs(y="Energy Flows (kWh)",x="",
         title=paste("Household Energy Shapes with ",battery_size,"KWh of Eguana Technologies Storage Capacity",sep=""),
         subtitle=paste(month.name[month(start_date)]," ",day(start_date),", ",year(start_date)," to ",month.name[month(end_date)]," ",day(end_date),", ",year(end_date),sep="")
    )
  print(top_panel)
  
  
  bottom_panel<-ggplot(work_data) +
    geom_line(aes(start,batt_charge,colour="Battery Charge"),size=2)+
    #scale_color_viridis("",option="C",labels = c("Battery\nCharge"),discrete = TRUE)+
    scale_colour_manual("",values = colors_tableau10(),labels = c("Battery Charge (+) or\nDischarge (-) State or Flows", "Purchases (+) or Sales (-)\nof Electricity","Purchases (+) or Sales (-) \nof Electricity w/o Battery"))+
    scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
    #scale_y_continuous(limits = c(0,round(batt_size/10)*10))+
    #scale_y_continuous(limits = c(0,round(batt_size/10+1)*10))+
    scale_y_continuous(limits = c(0,26.1))+
    theme_minimal()+theme(    
      legend.position = "bottom",
      plot.caption = element_text(size = 12, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 12,face = "bold", colour="black")
    )+    labs(y="Battery Charge (kWh)",x="")
  #caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
  
  print(bottom_panel)
  
  #extract legend
  #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
  
  mylegend<-g_legend(top_panel)
  
  png<-1
  if(png==1)
    set_png(file=paste("home_battery",month(start_date),"_",day(start_date),".png",sep=""))
  grid.arrange(arrangeGrob(top_panel + theme(legend.position="none",
                                             legend.margin=margin(c(0,0,0,0),unit="cm"),
                                             legend.text = element_text(colour="black", size = 14, face = "bold"),
                                             plot.caption = element_text(size = 12, face = "italic"),
                                             plot.title = element_text(face = "bold"),
                                             plot.subtitle = element_text(size = 14, face = "italic"),
                                             panel.grid.minor = element_blank(),
                                             text = element_text(size = 12,face = "bold"),
                                             axis.text = element_text(size = 12,face = "bold", colour="black"),
                                             axis.text.x = element_blank()
  ),
  bottom_panel + theme(legend.position="none",
                       plot.caption = element_text(size = 12, face = "italic"),
                       plot.title = element_text(face = "bold"),
                       plot.subtitle = element_text(size = 16, face = "italic"),
                       panel.grid.minor = element_blank(),
                       text = element_text(size = 12,face = "bold"),
                       axis.text = element_text(size = 12,face = "bold", colour="black")
  ),
  ncol=1,heights=c(3,1.75)),
  mylegend, nrow=2,heights=c(10, 1),bottom =text_grob(
    "Source: SolarPeople system data via Neurio API, graph and battery simulation by Andrew Leach",
    face = "italic", color = "black",size=14,just="center",lineheight = 1
  )
  )
  
  if(png==1)#set these to only turn on if you're making PNG graphs
    dev.off()
}



#call on battery

batt_size<-13.5
hourly_data<-hourly_data %>% rename(net_from_grid=net_to_grid)
#work_data<-subset(work_data,start<=min(start)+years(1)) #create a one year sample

summary_data_store<-data.frame()
#for(batt_size in c(seq(0,30,by=5),13.5,26.5)){
for(batt_size in 36){
  work_data<-hourly_data %>% filter(start>ymd("2021-03-31"),start<=ymd("2022-04-01"))%>%mutate(generationEnergy=generationEnergy*1.16,
  net_from_grid=consumptionEnergy-generationEnergy
  )
  #batt_size<-13*400
  batt_in<- - 10
  batt_out<-10
  init_charge<-35
  batt_eff<-.9
  #batt_eff<-1
  elec_price<-.05
  t_d_price<-.055
  
  
  
  work_data$batt_call<-work_data$net_from_grid
  work_data$batt_charge<-work_data$exportedEnergy*0+init_charge #fill the variable - it will be replaced later
  work_data$batt_call<- ifelse(work_data$batt_call>batt_out,batt_out,work_data$batt_call)
  work_data$batt_call<- ifelse(work_data$batt_call<batt_in,batt_in,work_data$batt_call)
  work_data$batt_call<- ifelse(work_data$batt_call<0,work_data$batt_call*batt_eff,work_data$batt_call/batt_eff)
  
  
  # 
  rows_data<-nrow(work_data)
  for(row_num in seq(1,rows_data)){
    if(row_num==1)
    {
      work_data$batt_charge[row_num]<-max(0,min(init_charge-work_data$batt_call[row_num],batt_size))
      work_data$batt_pwr[row_num]<-work_data$batt_charge[row_num]-init_charge
      work_data$batt_pwr[row_num]<- ifelse(work_data$batt_pwr[row_num]<0,work_data$batt_pwr[row_num]*batt_eff,work_data$batt_pwr[row_num]/batt_eff)
    }
    else
    {
      work_data$batt_charge[row_num]<-max(0,min(work_data$batt_charge[row_num-1]-work_data$batt_call[row_num],batt_size))
      work_data$batt_pwr[row_num]<-work_data$batt_charge[row_num]-work_data$batt_charge[row_num-1]
      work_data$batt_pwr[row_num]<- ifelse(work_data$batt_pwr[row_num]<0,work_data$batt_pwr[row_num]*batt_eff,work_data$batt_pwr[row_num]/batt_eff)
    }
    work_data$net_imp[row_num]<-work_data$net_from_grid[row_num]+work_data$batt_pwr[row_num]
  }
  
  work_data$charges<-ifelse(work_data$net_imp>0,work_data$net_imp*(elec_price+t_d_price),work_data$net_imp*(elec_price))
  work_data$net_meter<-work_data$net_imp*(elec_price+t_d_price)
  work_data$bill<-work_data$consumptionEnergy*(elec_price+t_d_price)
  work_data$td_paid<-ifelse(work_data$net_imp>0,work_data$net_imp*(t_d_price),0)
  work_data$td_nb<-work_data$net_imp*(t_d_price)
  work_data$td_base<-work_data$consumptionEnergy*(t_d_price)
  #work_data$charges<-ifelse(work_data$net_imp>0,work_data$net_imp*(elec_price+t_d_price),work_data$net_imp*(elec_price))
  
  print(paste("Battery size is ",batt_size," and annual imports are ",round(sum(work_data$net_imp[work_data$net_imp>=0]), digits=2)," Total billable variable costs are ",round(sum(work_data$charges),digits=2)))
  #batt size, imports, exports,bill w/o battery, bill w net bill, bill w net meter
  summary<-c(batt_size,sum(work_data$net_imp[work_data$net_imp>=0]),-sum(work_data$net_imp[work_data$net_imp<=0]),
             sum(work_data$charges),sum(work_data$net_meter),sum(work_data$bill),max(abs(work_data$net_imp)),
             max(abs(work_data$consumptionEnergy)),max(abs(work_data$net_from_grid)),sum(work_data$td_paid),sum(work_data$td_nb),sum(work_data$td_base))
  summary_data_store<-rbind(summary_data_store,summary)
  names(summary_data_store)<-c("batt_size"," imports", "exports","bill_net_bill","bill_net_meter","bill_base",
                               "peak_flow","peak_load","peak_flow_w_solar","t_d_net_bill","t_d_net_meter","t_d_base")
  #write.xlsx(work_data, file = paste("battery_size_",batt_size,".xlsx",sep=""), colNames = TRUE, borders = "columns")
  
  
    #write.xlsx(work_data, file = paste("battery_size_",batt_size,".xlsx",sep=""), colNames = TRUE, borders = "columns")
  if(batt_size==26)
      {
      home_battery_graph(ymd("2018-05-01",tz="America/Denver"),batt_size)
      home_battery_graph(ymd("2017-12-01",tz="America/Denver"),batt_size)
      }
  

  }



cycles<-work_data %>% group_by(date)%>%
  summarize(cycle=max(batt_charge)-min(batt_charge))

top_panel<-ggplot(work_data) +
  geom_line(aes(time,net_imp,colour="Net Consumption"),size=2) +
  geom_line(aes(time,batt_pwr,colour="Battery Flow"),size=2)+
  scale_colour_manual("",values = colors_tableau10(),labels = c("Battery Charge (+) or\nDischarge (-) State or Flows", "Purchases (+) or Sales (-)\nof Electricity","Purchases (+) or Sales (-) \nof Electricity w/o Battery"))+
  scale_y_continuous()+
  #scale_color_viridis("",option="C",labels = c("Battery Charge (+) or\nDischarge (-) State or Flows", "Purchases (+) or Sales (-)\nof Electricity","Purchases (+) or Sales (-) \nof Electricity w/o Battery"),discrete = TRUE)+
  #scale_color_viridis("",labels = c("Battery\nCharge","Consumption","Net Electricity\nPurchases","Solar\nGeneration"),discrete = TRUE,option="C")+
  scale_x_datetime()+
  theme_minimal()+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
  )+
  labs(y="Energy Flows (kWh)",x="",
       title=paste("Household Energy Shapes with ",batt_size,"KWh of Eguana Technologies Storage Capacity",sep=""),
       #subtitle=paste(month.name[month(start_date)]," ",day(start_date),", ",year(start_date)," to ",month.name[month(end_date)]," ",day(end_date),", ",year(end_date),sep="")
   NULL
   )
print(top_panel)


bottom_panel<-ggplot(work_data) +
  geom_line(aes(start,batt_charge,colour="Battery Charge"),size=2)+
  #scale_color_viridis("",option="C",labels = c("Battery\nCharge"),discrete = TRUE)+
  scale_colour_manual("",values = colors_tableau10(),labels = c("Battery Charge (+) or\nDischarge (-) State or Flows", "Purchases (+) or Sales (-)\nof Electricity","Purchases (+) or Sales (-) \nof Electricity w/o Battery"))+
  scale_x_datetime()+
  #scale_y_continuous(limits = c(0,round(batt_size/10)*10))+
  #scale_y_continuous(limits = c(0,round(batt_size/10+1)*10))+
  #scale_y_continuous(limits = c(0,26.1))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    plot.caption = element_text(size = 12, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 12,face = "bold", colour="black")
  )+    labs(y="Battery Charge (kWh)",x="")
#caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")

print(bottom_panel)

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(top_panel)

png<-1
if(png==1)
  set_png(file=paste("home_battery",month(start_date),"_",day(start_date),".png",sep=""))
grid.arrange(arrangeGrob(top_panel + theme(legend.position="none",
                                           legend.margin=margin(c(0,0,0,0),unit="cm"),
                                           legend.text = element_text(colour="black", size = 14, face = "bold"),
                                           plot.caption = element_text(size = 12, face = "italic"),
                                           plot.title = element_text(face = "bold"),
                                           plot.subtitle = element_text(size = 14, face = "italic"),
                                           panel.grid.minor = element_blank(),
                                           text = element_text(size = 12,face = "bold"),
                                           axis.text = element_text(size = 12,face = "bold", colour="black"),
                                           axis.text.x = element_blank()
),
bottom_panel + theme(legend.position="none",
                     plot.caption = element_text(size = 12, face = "italic"),
                     plot.title = element_text(face = "bold"),
                     plot.subtitle = element_text(size = 16, face = "italic"),
                     panel.grid.minor = element_blank(),
                     text = element_text(size = 12,face = "bold"),
                     axis.text = element_text(size = 12,face = "bold", colour="black")
),
ncol=1,heights=c(3,1.75)),
mylegend, nrow=2,heights=c(10, 1),bottom =text_grob(
  "Source: SolarPeople system data via Neurio API, graph and battery simulation by Andrew Leach",
  face = "italic", color = "black",size=14,just="center",lineheight = 1
)
)

if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


























png<-1
if(png==1)
  set_png(file="billing_battery.png")
ggplot(summary_data_store)+
  geom_line(aes(batt_size,bill_net_bill,colour="With Solar and Net Billing"),size=2)+
  geom_line(aes(batt_size,bill_net_meter,colour="With Solar and Net Metering"),size=2)+
  geom_line(aes(batt_size,bill_base,colour="Base Power Bill"),size=2)+
  scale_color_viridis("",option="C",labels = c("Base Power Bill", "With Solar and Net Billing","With Solar and Net Metering"),discrete = TRUE)+
  ajl_line()+
  labs(y="Variable Charges ($/yr)",x="Installed Battery Capacity (kWh)",
       title=paste("Household Energy Billing vs Installed Storage Capacity",sep=""))
if(png==1)
  dev.off()

png<-1
if(png==1)
  set_png(file="td_billing_battery.png")
ggplot(summary_data_store)+
  geom_line(aes(batt_size,t_d_net_bill,colour="With Solar and Net Billing"),size=2)+
  geom_line(aes(batt_size,t_d_net_meter,colour="With Solar and Net Metering"),size=2)+
  geom_line(aes(batt_size,t_d_base,colour="Base Power Bill"),size=2)+
  scale_color_viridis("",option="C",labels = c("Base Power Bill", "With Solar and Net Billing","With Solar and Net Metering"),discrete = TRUE)+
  ajl_line()+
  labs(y="Transmission and Distribution Charges ($/yr)",x="Installed Battery Capacity (kWh)",
       title=paste("Household Transmission and Distribution Billing vs Installed Storage Capacity",sep=""))
if(png==1)
  dev.off()


png<-1
if(png==1)
  set_png(file="td_billing_battery.png")
ggplot(summary_data_store)+
  geom_line(aes(batt_size,t_d_net_bill,colour="With Solar and Net Billing"),size=2)+
  
  geom_line(aes(batt_size,peak_load,colour="Base Power Bill"),size=2)+
  scale_color_viridis("",option="C",labels = c("Base Power Bill", "With Solar and Net Billing","With Solar and Net Metering"),discrete = TRUE)+
  ajl_line()+
  labs(y="Transmission and Distribution Charges ($/yr)",x="Installed Battery Capacity (kWh)",
       title=paste("Household Transmission and Distribution Billing vs Installed Storage Capacity",sep=""))
if(png==1)
  dev.off()





library(ggpubr)
library(gridExtra)
home_battery_graph(ymd("2018-05-01",tz="America/Denver"))
home_battery_graph(ymd("2017-12-01",tz="America/Denver"))

work_data$date<-as.POSIXct(as.Date(work_data$start))
df1 <- work_data %>% group_by(date,month) %>% summarise(gen=sum(generationEnergy),imps=sum(importedEnergy),cons=sum(consumptionEnergy),net=sum(net_from_grid))
df1$month = factor(month.abb[df1$month], levels = month.abb)
#df1<-df1 %>% filter(as.Date(date)>as.Date("2017-10-25"))
df1$cum_net<-cumsum(df1$net)
lims <- c(as.POSIXct(min(df1$date)),as.POSIXct(min(df1$date))+years(1))


breaks<-seq.POSIXt(min(lims), max(lims), by="4 weeks")

png<-1
if(png==1)
  set_png("cumulative_net_sim.png")
ggplot(df1)+
  #geom_smooth(aes(date,-net),method = "lm", formula = y ~ splines::bs(x, 3), se = TRUE)+
  geom_line(aes(date,-cum_net))+
  #  scale_color_viridis("",discrete=TRUE)+
  #  scale_fill_viridis(discrete=TRUE)+
  #scale_x_discrete()+
  #scale_x_discrete(limits = c(0,23))+
  #  scale_y_continuous(expand=c(.05,.05))+
  scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%b\n%d", tz="America/Denver"))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 12, face = "italic",hjust=0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+
  labs(x="",y="Cumulative Net Solar Generation Shortfall (kWh)",
       title="Cumulative Solar Generation Net of Consumption (August 2017-August 2018 (predicted))",
       subtitle="7.6 kW DC south-facing solar array in Edmonton, AB",
       caption=paste("Source: SolarPeople system data via Neurio API, graph and simulated data by Andrew Leach",
                     sep=""))
if(png==1)
  dev.off()


lims <- c(as.POSIXct(min(df1$date)),as.POSIXct(min(df1$date))+years(1))
breaks<-seq.POSIXt(min(lims), max(lims), by="4 weeks")
df1<-solar_data
df1$he<-factor(df1$Hour,levels = unique(df1$Hour),labels=seq(1,24))
df1$generationEnergy<-df1$`AC.System.Output.(W)`/1000
ggplot(subset(df1,Month==6),aes(he,generationEnergy,group=Day,colour = Day)) +
  geom_line(size=1.5) +
  scale_fill_brewer()+
  ajl_hourly()+
  labs(y="Hourly generation (kWh)",x="Hour Ending",
  title="Estimated Edmonton solar generation for 7.6kW south-facing array for May",
  subtitle="Source: NREL PVWatts, Graph by Andrew Leach")





