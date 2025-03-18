library(tidyverse)
library(ggpubr)
library(gridExtra)

load("hourly_solar_data.RData")
hourly_data<-hourly_data %>% rename(net_from_grid=net_to_grid)

#call on battery

batt_size<-1000

#work_data<-subset(work_data,start<=min(start)+years(1)) #create a one year sample

summary_data_store<-data.frame()
#for(batt_size in c(seq(0,30,by=5),13.5,26.5)){
for(batt_size in 2500){
  #batt_size<-13.5
  work_data<-hourly_data %>% 
    filter(start>ymd("2021-03-31"),start<=ymd("2022-04-01"))%>%
    mutate(generationEnergy=generationEnergy,
    net_from_grid=consumptionEnergy-generationEnergy
    )
  #batt_size<-13*400
  batt_in<- - 10
  batt_out<-10
  init_charge<-batt_size
  batt_eff<-.9
  #batt_eff<-1
  elec_price<-.05
  t_d_price<-.055
  
  work_data <-work_data %>% mutate(
    batt_call=net_from_grid, #charge or discharge is surplus in the hour
    batt_charge=exportedEnergy*0+init_charge, #fill the variable - it will be replaced later
    batt_call=ifelse(batt_call>batt_out,batt_out,batt_call), #impose the constraint
    batt_call=ifelse(batt_call<batt_in,batt_in,batt_call),#impose constraint
    batt_call<- ifelse(batt_call<0,batt_call*batt_eff,batt_call/batt_eff) #impose efficiency losses
  )
  
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
  
  work_data<-work_data%>% mutate(
    charges=ifelse(net_imp>0,net_imp*(elec_price+t_d_price),net_imp*(elec_price)),
    net_meter=net_imp*(elec_price+t_d_price),
    bill=consumptionEnergy*(elec_price+t_d_price),
    td_paid=ifelse(net_imp>0,net_imp*(t_d_price),0),
    td_nb<-net_imp*(t_d_price),
    td_base<-consumptionEnergy*(t_d_price))
  
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



top_panel<-
  ggplot(work_data%>%head(8760)) +
  geom_line(aes(start,net_imp,colour="Net Consumption"),linewidth=0.85) +
  geom_line(aes(start,batt_pwr,colour="Battery Flow"),linewidth=0.85)+
  scale_colour_manual("",values = colors_tableau10(),labels = c("Battery Charge (+) or\nDischarge (-) State or Flows", "Purchases (+) or Sales (-)\nof Electricity","Purchases (+) or Sales (-) \nof Electricity w/o Battery"))+
  scale_y_continuous()+
  #scale_color_viridis("",option="C",labels = c("Battery Charge (+) or\nDischarge (-) State or Flows", "Purchases (+) or Sales (-)\nof Electricity","Purchases (+) or Sales (-) \nof Electricity w/o Battery"),discrete = TRUE)+
  #scale_color_viridis("",labels = c("Battery\nCharge","Consumption","Net Electricity\nPurchases","Solar\nGeneration"),discrete = TRUE,option="C")+
  scale_x_datetime(date_breaks = "1 day")+
  theme_minimal()+
  theme(legend.position="bottom",
        legend.margin=margin(c(0,0,0,0),unit="cm"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
  )+
  labs(y="Energy Flows (kWh)",x="",
       title=paste("Household Energy Shapes with ",batt_size,"kWh of Eguana Technologies Storage Capacity",sep=""),
       #subtitle=paste(month.name[month(start_date)]," ",day(start_date),", ",year(start_date)," to ",month.name[month(end_date)]," ",day(end_date),", ",year(end_date),sep="")
   NULL
   )
print(top_panel)


bottom_panel<-
  ggplot(work_data%>%head(8760)) +
  geom_line(aes(start,batt_charge,colour="Battery Charge"),linewidth=0.85)+
  #scale_color_viridis("",option="C",labels = c("Battery\nCharge"),discrete = TRUE)+
  scale_colour_manual("",values = colors_tableau10(),labels = c("Battery Charge (+) or\nDischarge (-) State or Flows", "Purchases (+) or Sales (-)\nof Electricity","Purchases (+) or Sales (-) \nof Electricity w/o Battery"))+
  scale_x_datetime(date_breaks = "30 days",date_labels = "%b\n%d")+
  expand_limits(y=c(0,batt_size*1.10))+
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

combined_plot<-grid.arrange(arrangeGrob(top_panel + theme(legend.position="none",
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

ggsave(plot = combined_plot,filename = "battery_test.png",width=16,height=12,dpi=300)


ggplot(summary_data_store)+
  geom_line(aes(batt_size,bill_net_bill,colour="With Solar and Net Billing"),size=2)+
  geom_line(aes(batt_size,bill_net_meter,colour="With Solar and Net Metering"),size=2)+
  geom_line(aes(batt_size,bill_base,colour="Base Power Bill"),size=2)+
  scale_color_viridis("",option="C",labels = c("Base Power Bill", "With Solar and Net Billing","With Solar and Net Metering"),discrete = TRUE)+
  ajl_line()+
  labs(y="Variable Charges ($/yr)",x="Installed Battery Capacity (kWh)",
       title=paste("Household Energy Billing vs Installed Storage Capacity",sep=""))


ggplot(summary_data_store)+
  geom_line(aes(batt_size,t_d_net_bill,colour="With Solar and Net Billing"),size=2)+
  geom_line(aes(batt_size,t_d_net_meter,colour="With Solar and Net Metering"),size=2)+
  geom_line(aes(batt_size,t_d_base,colour="Base Power Bill"),size=2)+
  scale_color_viridis("",option="C",labels = c("Base Power Bill", "With Solar and Net Billing","With Solar and Net Metering"),discrete = TRUE)+
  ajl_line()+
  labs(y="Transmission and Distribution Charges ($/yr)",x="Installed Battery Capacity (kWh)",
       title=paste("Household Transmission and Distribution Billing vs Installed Storage Capacity",sep=""))


ggplot(summary_data_store)+
  geom_line(aes(batt_size,t_d_net_bill,colour="With Solar and Net Billing"),size=2)+
  
  geom_line(aes(batt_size,peak_load,colour="Base Power Bill"),size=2)+
  scale_color_viridis("",option="C",labels = c("Base Power Bill", "With Solar and Net Billing","With Solar and Net Metering"),discrete = TRUE)+
  ajl_line()+
  labs(y="Transmission and Distribution Charges ($/yr)",x="Installed Battery Capacity (kWh)",
       title=paste("Household Transmission and Distribution Billing vs Installed Storage Capacity",sep=""))
