library(tidyverse)
library(ggpubr)
library(gridExtra)
library(janitor)
library(scales)
options(scipen = 999)
load("hourly_solar_data.RData")
hourly_data<-hourly_data %>% rename(net_from_grid=net_to_grid)

#call on battery

batt_size<-1000

#work_data<-subset(work_data,start<=min(start)+years(1)) #create a one year sample

gen_energy<-sum(hourly_data$generationEnergy)
cons_energy<-sum(hourly_data$consumptionEnergy)
testing<-hourly_data %>% rename(net_gen=net_from_grid)%>%
  mutate(
    consumption=consumptionEnergy/cons_energy*gen_energy,
    net_gen=generationEnergy-consumption,
    cumul=cumsum(net_gen))

max(testing$cumul)-min(testing$cumul)
max(max(testing$net_gen),abs(min(testing$net_gen)))

#hatch ratio
(max(testing$cumul)-min(testing$cumul))*max(max(testing$net_gen),abs(min(testing$net_gen)))


#select AB resources
load("../alberta_power/data/renew_w_load.rdata")
load("../alberta_power/data/forecast_data.rdata")

sim_data<-renew_vols%>%filter(year(date)==2024)%>%
  group_by(date,he,time,Plant_Type)%>%
  summarise(gen=sum(available_mw),ail=first(actual_ail))

sim_data<-sim_data %>%pivot_wider(names_from = Plant_Type,values_from = gen)%>%
            mutate(RENEW=WIND+SOLAR)%>%clean_names()



batt_sim<-function(cons_sent,gen_sent,time_sent){
  testing<-tibble(time_sent,gen_sent,cons_sent)%>%
    mutate(
      cons_norm=cons_sent/sum(cons_sent)*sum(gen_sent),
      net_gen=gen_sent-cons_norm,
      cumul=cumsum(net_gen))%>%
    mutate(cumul=cumul+abs(min(cumul)),
           power_norm=net_gen/mean(cons_norm)*100,
           storage_days=cumul/(mean(cons_norm)*24)
           )
  test_store<<-testing
  
  top_panel<-
    ggplot(testing) +
    #geom_line(aes(start,net_imp,colour="Net Consumption"),linewidth=0.85) +
    geom_line(aes(time_sent,power_norm,colour="Battery Flow"),linewidth=0.85)+
    scale_colour_manual("",values = colors_tableau10(),labels = c("Battery Charge (+) or\nDischarge (-)") )+
    scale_y_continuous(breaks=seq(-100,300,100))+
    expand_limits(y=c(-110,330))+
    #scale_color_viridis("",option="C",labels = c("Battery Charge (+) or\nDischarge (-) State or Flows", "Purchases (+) or Sales (-)\nof Electricity","Purchases (+) or Sales (-) \nof Electricity w/o Battery"),discrete = TRUE)+
    #scale_color_viridis("",labels = c("Battery\nCharge","Consumption","Net Electricity\nPurchases","Solar\nGeneration"),discrete = TRUE,option="C")+
    scale_x_datetime(breaks = pretty_breaks(n = 12),labels = label_date_short(),expand=c(0,0))+
    expand_limits(x=max(testing$time_sent)+days(7))+
    theme_minimal()+
    theme(legend.position="none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          plot.caption = element_text(hjust = 0.5),
          legend.margin=margin(c(0,0,0,0),unit="cm"),
          text = element_text(colour="black", size = 15, face = "bold"),
    )+
    labs(y="Hourly Power Requirement (% avg load)",x="",
         title=paste("Capacity Surplus or Shortfall",sep=""),
         caption=paste("Source requirement of ",abs(round(min(testing$power_norm))),"% of average hourly load and sink requirement of ",round(max(testing$power_norm)),"% of average hourly load",sep=""),
         NULL
    )
  
    bottom_panel<-ggplot(testing) +
      #geom_line(aes(start,net_imp,colour="Net Consumption"),linewidth=0.85) +
      geom_line(aes(time_sent,storage_days,colour="Battery Charge"),linewidth=0.85)+
      scale_colour_manual("",values = colors_tableau10(),labels = c("Battery Charge State") )+
      scale_y_continuous(breaks = seq(0,100,20))+
      expand_limits(y=100)+
      scale_x_datetime(breaks = pretty_breaks(n = 12),labels = label_date_short(),expand=c(0,0))+
      expand_limits(x=max(testing$time_sent)+days(7))+
      theme_minimal()+
      theme(legend.position="none",     
            plot.caption = element_text(hjust = 0.5),
            legend.margin=margin(c(0,0,0,0),unit="cm"),
            text = element_text(colour="black", size = 15, face = "bold"),
      )+
      labs(y="Stored Energy Requirement (days of average load)",x="",
           title=paste("Stored Energy Requirement",sep=""),
           caption=paste("Meeting load implies a storage requirement equivalent to ",round(max(testing$storage_days))," days of average load",sep=""),
           NULL
      )
  gp<-grid.arrange(top_panel,bottom_panel)  
  ggsave(plot=gp, "batt_sim.png",dpi=300,width = 14,height=10,bg="white")
  
}

# ercot_gen<-read_csv("../alberta_power/data/ercot/ercot_gen_sun-wnd_hr_2023.csv",skip=3)
# ercot_load<-read_csv("../alberta_power/data/ercot/ercot_load-temp_hr_2023.csv",skip=3)
# 
# ercot_gen<-ercot_gen%>%left_join(ercot_load)




gen<-sim_data$solar
#gen<-sim_data$wind

#gen<-sim_data$renew



n <- length(sim_data$ail)
# Set AR(1) coefficient (e.g., phi = 0.8)
phi <- 0

# Generate AR(1) noise
set.seed(123)  # for reproducibility
ar_noise <- arima.sim(n = n, model = list(ar = phi), sd = 200)

# Mean-center the noise if desired
ar_noise <- ar_noise - mean(ar_noise)

gen <- sim_data$ail + ar_noise

cons<-sim_data$ail
time_var<-sim_data$time

#gen_sent<-ercot_gen$`ERCOT Total Solar Generation (MW)`
#cons_sent<-ercot_gen$`TOTAL Actual Load (MW)`
#time_sent<-ercot_gen$`Local Timestamp Central Time (Interval Beginning)`


batt_sim(cons%>%head(24*31),gen%>%head(24*31),time_var%>%head(24*31))

batt_sim(cons,gen,time_var)

#AB wind and solar portfolio




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


download.file("https://www.ercot.com/files/docs/2025/04/08/2025-ERCOT-Monthly-Peak-Demand-and-Energy-Forecast.xlsx",
              destfile="ercot_load_2025.xlsx")


  download.file("https://www.ercot.com/files/docs/2023/01/18/ERCOT-Monthly-Peak-Demand-and-Energy-Forecast-2023-2032.xlsx",
              destfile="ercot_load_2023.xlsx")

  download.file("https://www.ercot.com/files/docs/2020/12/28/ERCOT_Monthly_Peak_Demand_and_Energy_Forecast_2021_2030.xlsx",
                destfile="ercot_load_2021.xlsx")
  
  download.file("https://www.ercot.com/files/docs/2016/12/12/ERCOT_Monthly_Peak_Demand_and_Energy_Forecast_2017.xls",
                destfile="ercot_load_2017.xls")
  
  
  #StatCanInvestment in building construction 1, 2, 3, 4
  #Table: 34-10-0286-01
  #Release date: 2025-05-26
  base_year<-2018
  housing_starts<-cansim::get_cansim("34-10-0286-01")
  save(housing_starts,file="housing_starts.rdata")
  housing_epcor<-housing_starts %>% clean_names()%>%
    filter(geo %in% c("Edmonton, Alberta","Calgary, Alberta","Alberta","Canada"))%>%
    filter(type_of_structure=="Total residential")%>%
    filter(type_of_work=="New construction")%>%
    filter(investment_value=="Unadjusted - current")%>%
    mutate(date=ymd(date),year=year(date))%>%
    group_by(geo,year)%>%
    mutate(year_mean=mean(value))%>%
    group_by(geo)%>%
    mutate(index_val=value/max(year_mean*(year==base_year))*100)%>%
    ungroup()%>%
  
    I()
    
  
  ggplot(housing_epcor)+
    geom_line(aes(date,value,colour=geo))
  
  house_prices<-cansim::get_cansim("18-10-0205-01")%>%
    clean_names()%>%filter(geo %in% c("Toronto, Ontario","Vancouver, British Columbia","Edmonton, Alberta","Calgary, Alberta","Alberta","Canada"))%>%
    filter(new_housing_price_indexes=="Total (house and land)")%>%
    mutate(year=year(date))%>%
    mutate(size=case_when(
      geo %in% c("Edmonton, Alberta","Calgary, Alberta")~1.5,
      TRUE ~ 0.85
    ))%>%
    group_by(geo)%>%
    #mutate(index_val=value/last(value*(year==base_year))*100)%>%
    mutate(index_val=100*value/max(value*(date==ymd("2020-01-01")),na.rm = T))%>%
    ungroup()
    
  save(house_prices,file="house_prices.rdata")
  
  
  ggplot(house_prices%>%
           filter(geo!="Vancouver, British Columbia",geo!="Alberta")%>%
           mutate(geo=fct_relevel(geo,"Canada"),
                  geo=fct_relevel(geo,"Toronto, Ontario",after = Inf)
                  ))+
    geom_line(aes(date,value,colour=geo,linewidth = size))+
    scale_linewidth(range = c(0.5, 1.5)) +  # Adjust as needed
    guides(linewidth=FALSE,colour=guide_legend(nrow = 1))+
    #gghighlight(geo %in% c("Edmonton, Alberta", "Calgary, Alberta"),
    #            keep_scales = TRUE, unhighlighted_params = list())+
    expand_limits(x=Sys.Date()+months(3))+
    scale_color_manual("",values = c("red","orange","darkblue","grey50"))+
    scale_x_date(breaks = pretty_breaks(n = 12),labels = label_date_short(),expand=c(0,0))+
    theme_minimal()+
    theme(legend.position="bottom",
          legend.margin=margin(c(0,0,0,0),unit="cm"),
          text = element_text(colour="black", size = 15, face = "bold"),
    )+
    labs(y="House and Land Price Index (Dec 2016 = 100)",x="",
         title=paste("House and Land Price Index",sep=""),
         caption="Data via StatCan Table 18-10-0205-01, graph by Andrew Leach",
         NULL
    )
  ggsave("house_prices.png",width = 11,height = 6,dpi=300,bg="white")
  
  library(httr)
  start_date<-"2025-05-01"
  ercot_hourly<-function(start_date="2025-05-25",end_date="2025-05-26"
                         ){
  GET(url=paste("https://api.eia.gov/v2/electricity/rto/region-data/data/",sep=""),
              query = list(
                api_key=KEY,
                frequency="hourly",
                "data[0]"="value",
                "facets[respondent][]"="ERCO",
                "facets[type][]"="D",
                "facets[type][]"="NG",
                start=start_date,
                end=end_date,
                "sort[0][column]"="period",
                "sort[0][direction]"="asc",
                out="json"
              )
  ) %>% 
    httr::content()%>%
      pluck("response", "data") %>%
      enframe() %>%
      unnest_auto(value)%>%
      slice(1:(n() - 2))#take off the last two rows which will be the first hour of end_date
  }
  
  test<-ercot_hourly()
  
  
  
  
  #aeso projects queeue
  library(readxl)
  library(lubridate)
  
  dir<-"../alberta_power/data/AESO_projects/"
  files <- list.files(dir,pattern = "\\.xlsx?$", #pattern = "2025", 
                      full.names = TRUE)
  
  
  aeso_projects<-list()
  i<-1
  for(file_name in files[7]){
    print(file_name)
    date_string <- sub(".*?/([A-Za-z]+-\\d{4})-Project-List\\.xlsx$", "\\1", file_name)
    date_obj <- dmy(paste("01", date_string))  # "01 April 2025"
  aeso_projects[[i]]<-read_excel(file_name)%>%
     mutate(date=date_obj)
  if(names(aeso_projects[[i]])[1]=="Project Information")
    aeso_projects[[i]]<-read_excel(file_name,skip = 1)%>%
    mutate(date=date_obj)
  
  i<-i+1
  }
  aeso_projects<-do.call(bind_rows, aeso_projects)%>%clean_names()
  
  aeso_combo<-aeso_projects %>% group_by(date,mw_type)%>%
    summarize(sts=sum(en1_sts_mw,na.rm = T)+
                sum(en2_sts_mw,na.rm = T)+sum(en3_sts_mw,na.rm = T),
              dts=sum(en1_dts_mw,na.rm = T)+sum(en2_dts_mw,na.rm = T)
              )%>%
    ungroup()