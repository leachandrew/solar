source("../andrew_base.R")

#curl -i -X POST --data "grant_type=client_credentials&client_id=P8EEG3rvRxyqWd_beUBCJA&client_secret=ki6M5J_hTtiXuMAcHHPmtw" https://api.neur.io/v1/oauth2/token

sensor_id <- "0x0000C47F510354AE"

#
#req <- make_req(straighten())[[1]]

#status check, neurio API
neurio_api <- GET("https://api.neur.io/v1/status")
neurio_r <- fromJSON(rawToChar(neurio_api$content))


#save(list = c("my_client_id", "my_client_secret","sensor_id"), file = "neurio_credentials.Rdata")

load(file = "neurio_credentials.Rdata")

#simon and peter's systems

#load(file = "simon_neurio_credentials.Rdata")
#load(file = "solar_homes_neurio_credentials.Rdata")


source("neurio_scripts.R")



#get a token if you need one
token_id<- get_token(my_client_id,my_client_secret)

#update hourly data
load_file<-"hourly_solar_data.RData"
load(load_file,.GlobalEnv) 

hourly_data<-get_hourly_data(hourly_data%>%filter(start<Sys.time()-days(1)))

# hourly_data %>%
#          select(-net_to_grid,-year,-month,-hour,-he,-day,-date)%>% 
#          pivot_longer(-c(start,end,month,hou),names_to = "measure",values_to="value")%>%
#          filter(!is.na(value))%>% 
#   mutate(month=month(start),hour=hour(start))%>%
#   ggplot()+
#   geom_line(aes(hour,value,group=month,color=month),linewidth=1.5)+
#   #scale_x_discrete(breaks=pretty_breaks(6), expand=c(0,0))+
#   scale_y_continuous(breaks=pretty_breaks(), expand=c(0,0))+
#   labs(
#     title = "Leach Household Electricity Consumption and Generation",
#     y="Hourly Generation (kWh)",x="")+
#   theme_update()+
#   theme(legend.position = "bottom",
#         plot.margin = unit(c(1,1,0.2,1), "cm"))



#hourly_data<-sys_data
save(hourly_data, file= "hourly_solar_data.RData")
write.csv(hourly_data,"leach_solar_hourly.csv")

# write_csv(hourly_data%>%
#             select(-net_to_grid,-year,-month,-hour,-he,-day,-date)%>% 
#             pivot_longer(-c(start,end),names_to = "measure",values_to="value")%>%
#             filter(!is.na(value))%>%
#             mutate(start=as.POSIXct(start,tz="America/Denver"),
#                    end=as.POSIXct(start,tz="America/Denver")),
#           "../../Courses/econ_366_w23/resources/hourly_solar.csv")



#check rate limit
limit<-as.numeric(sample_neurio())
#update 5 min data

load_and_update(rate_limit = as.numeric(limit)-1)


load("solar_data.RData",.GlobalEnv) 





df1 <- hourly_data %>% mutate(month=month(start),date=as.POSIXct(date(start),tz="America/Denver")) %>%
  group_by(he,month,year) %>% 
  summarise(gen=mean(generationEnergy),imps=mean(importedEnergy),cons=mean(consumptionEnergy),net=mean(net_to_grid)) %>%
  ungroup() %>% mutate(month=factor(month.abb[month], levels = month.abb),
                       date=ymd(paste(year,month,1,sep="-")))

p<-ggplot(df1)+
  geom_hline(aes(yintercept=0),color="black",size=rel(1.25))+
  #geom_line(data=hourly_data%>%mutate(month=factor(month.abb[month])),aes(he,generationEnergy,group=date),colour="black",size=rel(.025),alpha=.18)+
  geom_line(aes(he,gen,colour=factor(year),group=factor(year)),size=rel(1.25))+
  facet_wrap(~month,nrow = 2)+
  #scale_linetype_manual("",values=c("solid","31"),labels=c("Solar Generation","Net Consumption"))+
  scale_color_viridis("",discrete=TRUE,option = "D")+
  scale_x_continuous(expand=c(0,0),breaks=c(8,12,18,22))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(y=7)+
  theme_classic()+theme(text=element_text(size=21),
                        legend.key.width = unit(2,"cm"),legend.position = "bottom")+
  labs(x="Hour Ending",y="Hourly Generation (kWh)",
       title="Monthly Solar Generation Patterns",
       subtitle="August, 2017-present hourly average values",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
ggsave(filename = "hourly_gen_only.png",dpi=300, width = 16,height = 7,bg="white")


p<-ggplot(df1)+
  geom_hline(aes(yintercept=0),color="black",size=rel(1.25))+
  geom_line(data=hourly_data%>%mutate(month=factor(month.abb[month])),aes(he,generationEnergy,group=date),colour="black",size=rel(.0025),alpha=.18)+
  geom_line(aes(he,gen,colour=factor(year),group=factor(year)),size=rel(1.25))+
  facet_wrap(~month,nrow = 2)+
  #scale_linetype_manual("",values=c("solid","31"),labels=c("Solar Generation","Net Consumption"))+
  scale_color_viridis("",discrete=TRUE,option = "D")+
  scale_x_continuous(expand=c(0,0),breaks=c(8,12,18,22))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(y=7)+
  theme_classic()+theme(text=element_text(size=21),
                        legend.key.width = unit(2,"cm"),legend.position = "bottom")+
  labs(x="Hour Ending",y="Hourly Generation (kWh)",
       title="Monthly Solar Generation Patterns",
       subtitle="August, 2017-present hourly average values",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
ggsave(filename = "hourly_gen_spag.png",dpi=150, width = 16,height = 7)


p<-ggplot(df1)+
  geom_hline(aes(yintercept=0),color="black",size=rel(1.25))+
  geom_line(data=hourly_data%>%mutate(month=factor(month.abb[month])),aes(he,consumptionEnergy*(consumptionEnergy>0),group=date),colour="black",size=rel(.025),alpha=.18)+
  #geom_line(aes(he,gen,colour=factor(year),group=factor(year),linetype="A"),size=rel(1.25))+
  geom_line(aes(he,cons,colour=factor(year),group=factor(year),linetype="B"),size=rel(1.25))+
  facet_wrap(~month,nrow = 2)+
  scale_linetype_manual("",values=c("solid","31"),labels=c("Solar Generation","Net Consumption"))+
  scale_color_viridis("",discrete=TRUE,option = "D")+
  scale_x_continuous(expand=c(0,0),breaks=c(8,12,18,22))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(y=7)+
  theme_minimal()+theme(legend.key.width = unit(2,"cm"),legend.position = "bottom")+
  labs(x="Hour Ending",y="Hourly Household Consumption (kWh)",
       title="Monthly Household Consumption",
       subtitle="August, 2017-present hourly average values",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
ggsave(filename = "hourly_cons_spag.png",dpi=150, width = 16,height = 9)



p<-ggplot(df1)+
  geom_hline(aes(yintercept=0),color="black",size=rel(1.25))+
  geom_line(data=hourly_data%>%mutate(month=factor(month.abb[month])),aes(he,-net_to_grid,group=date),colour="black",size=rel(.025),alpha=.18)+
  geom_line(aes(he,-net,colour=factor(year),group=factor(year)),size=rel(1.25))+
  facet_wrap(~month,nrow = 2)+
  scale_color_viridis("",discrete=TRUE,option = "D")+
  scale_x_continuous(expand=c(0,0),breaks=c(8,12,18,22))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(y=c(-7,7))+
  theme_classic()+theme(text=element_text(size=21),
                        legend.key.width = unit(2,"cm"),legend.position = "bottom")+
  labs(x="Time",y="Hourly Net Exports (kWh)",
       title=paste("Daily Average Solar Net-to-Grid (",year(min(hourly_data$start)),"-",year(max(hourly_data$start)),")",sep=
                     ""),
       subtitle="August, 2017-present hourly average values",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
ggsave(filename = "hourly_net_spag.png",dpi=150, width = 16,height = 7)




p<-ggplot(df1)+
  geom_hline(aes(yintercept=0),color="black",size=rel(1.25))+
  geom_line(aes(he,-net,colour=factor(year),group=factor(year)),size=rel(1.25))+
  facet_wrap(~month,nrow = 2)+
  scale_color_viridis("",discrete=TRUE,option = "D")+
  scale_x_continuous(expand=c(0,0),breaks=c(8,12,18,22))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(y=c(-7,7))+
  theme_classic()+theme(text=element_text(size=21),
                        legend.key.width = unit(2,"cm"),legend.position = "bottom")+
  labs(x="Time",y="Hourly Net Exports (kWh)",
       title=paste("Daily Average Solar Net-to-Grid (",year(min(hourly_data$start)),"-",year(max(hourly_data$start)),")",sep=
                     ""),
       subtitle="August, 2017-present hourly average values",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
ggsave(filename = "hourly_net.png",dpi=150, width = 16,height = 7)


p<-ggplot(df1)+
  geom_hline(aes(yintercept=0),color="black",size=rel(1.25))+
  geom_line(aes(he,gen,colour=factor(year),group=factor(year),linetype="A"),size=rel(1.25))+
  geom_line(aes(he,net,colour=factor(year),group=factor(year),linetype="B"),size=rel(1.25))+
  facet_wrap(~month,nrow = 2)+
  scale_linetype_manual("",values=c("solid","31"),labels=c("Solar Generation","Net Consumption"))+
  scale_color_viridis("",discrete=TRUE,option = "D")+
  scale_x_continuous(expand=c(0,0),breaks=c(8,12,18,22))+
  scale_y_continuous(expand=c(0,0),limits = c(-5,5),breaks=pretty_breaks())+
  theme_minimal()+theme(legend.key.width = unit(2,"cm"),legend.position = "bottom")+
  labs(x="Hour Ending",y="Hourly Generation and Net Consumption (kWh)",
       title="Monthly Solar Generation and Net Consumption",
       subtitle="August, 2017-present hourly average values",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
ggsave(filename = "hourly_gen.png",dpi=150, width = 16,height = 9)

p<-ggplot(df1)+
  geom_hline(aes(yintercept=0),color="black",size=rel(1.25))+
  geom_line(aes(he,-net,colour=factor(year),group=factor(year)),size=rel(1.25))+
  facet_wrap(~month,nrow = 2)+
  scale_color_viridis("",discrete=TRUE,option = "D")+
  scale_x_continuous(expand=c(0,0),breaks=c(8,12,18,22))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  
  ajl_line()+
  labs(x="Time",y="Hourly Net Exports (kWh)",
       title=paste("Daily Average Solar Net-to-Grid (",year(min(hourly_data$start)),"-",year(max(hourly_data$start)),")",sep=
                     ""),
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
ggsave(filename = "hourly_net.png",dpi=150, width = 16,height = 9)


p<-ggplot(df1)+
  geom_line(aes(he,net,colour=factor(year),group=factor(year)),size=1.2)+
  facet_wrap(~month,nrow = 2)+
  scale_color_viridis("",discrete=TRUE,option = "D")+
  scale_x_continuous(expand=c(0,0),breaks=c(8,12,18,22))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  ajl_line()+
  labs(x="Time",y="Hourly Net Exports (kWh)",
       title=paste("Daily Average Solar Net-to-Grid (",year(min(hourly_data$start)),"-",year(max(hourly_data$start)),")",sep=
                     ""),
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
ggsave(filename = "hourly_duck.png",dpi=600, width = 16,height = 9)

p<-ggplot(df1)+
  geom_line(aes(he,cons,colour=factor(year),group=factor(year)),size=1.2)+
  facet_wrap(~month,nrow = 2)+
  scale_color_viridis("",discrete=TRUE,option = "D")+
  scale_x_continuous(expand=c(0,0),breaks=c(8,12,18,22))+
  scale_y_continuous(expand=c(0,0),breaks=pretty_breaks())+
  expand_limits(y=0)+ajl_line()+#theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(x="Hour Ending",y="Hourly Consumption (kWh)",
       title=paste("Average Electricity Consumption (",year(min(hourly_data$start)),"-",year(max(hourly_data$start)),")",sep=
                     ""),
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
ggsave(filename = "hourly_cons.png",dpi=600, width = 16,height = 9)




cumulative_graphs<-function(){
  min_time<-min(sys_data$start)+hours(1)
  #min_time<-trunc(min(sys_data$start),"days")+ 60*60*24
  max_time<-max(sys_data$end)
  #max_time<-min_time+years(1)
  min_time<-format(min_time,"%H:%M on %b %d, %Y")
  max_time<-format(max_time,"%H:%M on %b %d, %Y")
  df1 <- sys_data %>% mutate(month=month(start),date=as.POSIXct(date(start),tz="America/Denver")) %>%
                  group_by(date,month) %>% 
                  summarise(gen=sum(generationEnergy/12),imps=sum(importedEnergy/12),cons=sum(consumptionEnergy/12),net=sum(net_to_grid/12)) %>%
                  mutate(month=factor(month.abb[month], levels = month.abb))
  
  #df1<-df1 %>% filter(as.Date(date)>as.Date("2017-10-25"))
  df1$cum_net<-cumsum(df1$net)
  df1$cum_gen<-cumsum(df1$gen)
  lims <- c(as.POSIXct(min(df1$date)),as.POSIXct(max(df1$date)))
  
  breaks<-seq.POSIXt(min(lims), max(lims), by="4 weeks")
  
  
  cap_text<-paste("Source: Generation data via Neurio API, graph by Andrew Leach. From ",min_time," to ",max_time,", cumulative generation was ",
        round(sum(df1$gen)),"kWh and net deliveries ",ifelse(sum(df1$net)>0,"from","to")," the grid were ",
        abs(round(sum(df1$net))),"kWh."
        ,sep="")
  
  cap_text<-paste(strwrap(cap_text,width=116), collapse="\n")
  
  png<-1
  if(png==1)
    set_png("cumulative_net_all.png")
  p<-ggplot(df1)+
    #geom_smooth(aes(date,-net),method = "lm", formula = y ~ splines::bs(x, 3), se = TRUE)+
    geom_line(aes(date,-cum_net))+
    #  scale_color_viridis("",discrete=TRUE)+
    #  scale_fill_viridis(discrete=TRUE)+
    #scale_x_discrete()+
    #scale_x_discrete(limits = c(0,23))+
    #  scale_y_continuous(expand=c(.05,.05))+
    scale_x_datetime(limits = lims,date_breaks="3 months",labels = date_format("%b\n%Y", tz="America/Denver"))+
    theme_minimal()+theme(    
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 14, face = "bold"),
      plot.caption = element_text(size = 10, face = "italic",hjust=0),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 14, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 10,face = "bold"),
      axis.text = element_text(size = 10,face = "bold", colour="black"),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )+
    labs(x="",y="Cumulative Net Solar Generation Shortfall (kWh)",
         title="Cumulative Solar Generation Net of Consumption (August 2017-Current)",
         subtitle="7.6 kW DC south-facing solar array in Edmonton, AB",
         caption=cap_text)
  print(p)
  if(png==1)
    dev.off()
 
  min_time<-min(sys_data$start)+hours(1)
  #min_time<-trunc(min(sys_data$start),"days")+ 60*60*24
  #max_time<-max(sys_data$end)
  max_time<-min_time+years(1)
  min_time<-format(min_time,"%H:%M on %b %d, %Y")
  max_time<-format(max_time,"%H:%M on %b %d, %Y")
  
  lims <- c(as.POSIXct(min(df1$date)),as.POSIXct(min(df1$date))+years(1))
    if(png==1)
    set_png("cumulative_net_year1.png")
  p<-ggplot(df1)+
    #geom_smooth(aes(date,-net),method = "lm", formula = y ~ splines::bs(x, 3), se = TRUE)+
    geom_line(aes(date,-cum_net),size=2)+
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
    labs(x="Date",y="Cumulative Winter Generation Shortfall (kWh)",
         title="Cumulative Solar Generation Shortfall (Winter 2017-18)",
         subtitle="7.6 kW DC south-facing solar array in Edmonton, AB",
         caption=paste("Source: SolarPeople system data via Neurio API, graph by Andrew Leach\n",
                       "Maximum cumulative shortfall for 2017-2018 was ",round(max(df1$cum_net)),"kWh, or roughly ",round(max(df1$cum_net)/13.5/.93/10)*10," Tesla Powerwalls",
                       sep=""
         ))
  print(p)
  if(png==1)
    dev.off()  
  
  if(png==1)
    set_png("cumulative_gross_year1.png")
  p<-ggplot(df1)+
    #geom_smooth(aes(date,-net),method = "lm", formula = y ~ splines::bs(x, 3), se = TRUE)+
    geom_line(aes(date,cum_gen),size=2)+
    #  scale_color_viridis("",discrete=TRUE)+
    #  scale_fill_viridis(discrete=TRUE)+
    #scale_x_discrete()+
    #scale_x_discrete(limits = c(0,23))+
    #  scale_y_continuous(expand=c(.05,.05))+
    scale_x_datetime(limits = lims,date_breaks = "2 months",labels = date_format("%b %d\n%Y", tz="America/Denver"),expand = c(0,0))+
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
    labs(x="Date",y="Cumulative Annual Generation (kWh)",
         title="Cumulative Solar Generation (Year 1, 2017-18)",
         subtitle="7.6 kW DC south-facing solar array in Edmonton, AB",
         caption=paste("Source: SolarPeople system data via Neurio API, graph by Andrew Leach\n",
                       "Total cumulative year 1 generation was ",round(max(df1$cum_gen[df1$date<=lims[2]])),"kWh.",
                       sep=""
         ))
  print(p)
  if(png==1)
    dev.off()  
  
  
  
  #2020 cumulative
  
  system_start=date(min(hourly_data$start))
  
  df1 <- hourly_data %>% mutate(month=month(start),date=date(ymd_hms(start,tz="America/Denver")),
                                system_year=factor(1+trunc(time_length(interval(start = min(date),end = date),unit = "years")))
                                         
                                ) %>%
    group_by(date,month,year,system_year) %>% 
    summarise(gen=sum(generationEnergy),imps=sum(importedEnergy),cons=sum(consumptionEnergy),net=sum(net_to_grid)) %>%
    mutate(month=factor(month.abb[month], levels = month.abb)
           )%>%
    group_by(system_year)%>% mutate(day=row_number(),
                                    false_date=system_start+days(row_number()),
                                    cum_net=cumsum(net),
                             cum_gen=cumsum(gen))%>%ungroup()
  
  
  lims <- c(as.POSIXct(min(df1$date)),as.POSIXct(max(df1$date)))
  
  breaks<-seq.POSIXt(min(lims), max(lims), by="4 weeks")
  
  
  min_time<-min(df1$date)
  #min_time<-trunc(min(sys_data$start),"days")+ 60*60*24
  #max_time<-max(sys_data$end)
  max_time<-max(df1$date)
  min_time<-format(min_time,"%H:%M on %b %d, %Y")
  max_time<-format(max_time,"%H:%M on %b %d, %Y")
  
  cap_text<-paste("Source: Generation data via Neurio API, graph by Andrew Leach. From ",min_time," to ",max_time,", cumulative generation was ",
                  round(sum(df1$gen)),"kWh and net deliveries ",ifelse(sum(df1$net)>0,"from","to")," the grid were ",
                  abs(round(sum(df1$net))),"kWh."
                  ,sep="")
  
  cap_text<-paste(strwrap(cap_text,width=116), collapse="\n")
  
  p<-ggplot(df1)+
    #geom_smooth(aes(date,-net),method = "lm", formula = y ~ splines::bs(x, 3), se = TRUE)+
    geom_line(aes(false_date,-cum_net,group=system_year,color=system_year),size=2)+
    scale_color_viridis("System Year",discrete=TRUE)+
    #  scale_fill_viridis(discrete=TRUE)+
    #scale_x_discrete()+
    #scale_x_discrete(limits = c(0,23))+
    #  scale_y_continuous(expand=c(.05,.05))+
    scale_x_date(breaks=date_breaks(width = "1 month"),labels = date_format("%b"),expand = c(0,0))+
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
    labs(x="",y="Annual Generation Surplus (kWh)",
         title="Cumulative Solar Generation Surplus (2017-22)",
         subtitle="7.6 kW DC south-facing solar array in Edmonton, AB",
         caption=paste("Source: SolarPeople system data via Neurio API, graph by Andrew Leach\n",
                       "Maximum cumulative shortfall for 2017-2022 was ",round(max(df1$cum_net)),"kWh, or roughly ",round(max(df1$cum_net)/13.5/.93/10)*10," Tesla Powerwalls",
                       sep=""
         ))
  print(p)
  ggsave(file="annual_shortfall.png",dpi = 450,width = 13,height=6,bg="White")
  
  p<-ggplot(df1)+
    #geom_smooth(aes(date,-net),method = "lm", formula = y ~ splines::bs(x, 3), se = TRUE)+
    geom_line(aes(false_date,cum_gen,group=system_year,color=system_year),size=2)+
    scale_color_viridis("System Year",discrete=TRUE)+
    #  scale_fill_viridis(discrete=TRUE)+
    #scale_x_discrete()+
    #scale_x_discrete(limits = c(0,23))+
    #  scale_y_continuous(expand=c(.05,.05))+
    scale_x_date(breaks=date_breaks(width = "1 month"),labels = date_format("%b"),expand = c(0,0))+
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
    labs(x="",y="Annual Generation (kWh)",
         title="Cumulative Solar Generation (2017-22)",
         subtitle="7.6 kW DC south-facing solar array in Edmonton, AB",
         caption=paste("Source: SolarPeople system data via Neurio API, graph by Andrew Leach\n",
                       "Maximum cumulative generation for 2017-2022 was ",round(max(df1$cum_gen)),"kWh",
                       sep=""
         ))
  print(p)
  ggsave(file="annual_gen.png",dpi = 450,width = 13,height=6,bg="White")
  
  
  
  
}





cumulative_graphs()




df1<-sys_data %>% mutate(day_in_year=yday(start),year=factor(year),
                         billable_cons=net_to_grid*(net_to_grid>0),
                         gen_credit=net_to_grid*(net_to_grid<0),
                         power_bill=net_to_grid*(net_to_grid>0)) %>% 
  group_by(day_in_year,year) %>% 
  summarize(gen=sum(generationEnergy/12),
            net=sum(net_to_grid/12),
            cons=sum(consumptionEnergy/12),
            billed_power=sum(billable_cons/12),
            t_d_savings=sum(consumptionEnergy/12-billable_cons/12),
            gen_credit=sum(-1*gen_credit/12)
            ) %>% ungroup()

ggplot(df1)+
  geom_line(aes(day_in_year,gen_credit,group=year,color=year),size=1.25)+
  geom_line(aes(day_in_year,t_d_savings,group=year,color=year),size=1.25)
  

  df1<-sys_data %>% mutate(day_in_year=yday(start),year=factor(year),
                           billable_cons=net_to_grid*(net_to_grid>0),
                           gen_credit=net_to_grid*(net_to_grid<0),
                           power_bill=net_to_grid*(net_to_grid>0)) %>% 
    group_by(day_in_year,year) %>% 
    summarize(gen=sum(generationEnergy/12),
              net=sum(net_to_grid/12),
              cons=sum(consumptionEnergy/12),
              billed_power=sum(billable_cons/12),
              t_d_savings=sum(consumptionEnergy/12-billable_cons/12),
              gen_credit=sum(-1*gen_credit/12)
    ) %>% ungroup()
  






nrel_compare<-function(){

house_data <- read.xlsx(xlsxFile = "hourly-house.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
house_data$Date<-as.POSIXct(paste(house_data$Year,"-",house_data$Month,"-",house_data$Day," ",house_data$Hour,":00",sep=""),format='%Y-%m-%d %H:%M')
#View(head(house_data))
#house_data<-NULL
  
solar_data <- read.xlsx(xlsxFile = "solar_system.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
solar_data$Date<-as.POSIXct(paste(2016,"-",solar_data$Month,"-",solar_data$Day," ",solar_data$Hour+1,":00",sep=""),format='%Y-%m-%d %H:%M')
solar_data$Hour<-solar_data$Hour+1
solar_data<-solar_data %>% ungroup()

test<-hourly_data %>%
  filter(start>=ymd("2017-09-01"),start<ymd("2021-01-01"))%>%
  left_join(solar_data, by = c("month"="Month","he"="Hour","day"="Day"))
test$Date_NT<-as.Date(test$start)
test$`750_load_W`<-NULL
names(test)[grep("750_load",names(test))]<-"YEG_750_load"
test$YEG_pred_load<-test$YEG_750_load/750*663.2216
test<-arrange(test,month,day,hour)
df1<-test %>% group_by(year,month) %>% summarize(Pred_load=sum(YEG_pred_load),Actual_net=sum(net_to_grid,na.rm = T),Actual_load=sum(consumptionEnergy,na.rm = T),NREL=sum(`AC.System.Output.(W)`/1000,na.rm = T),Actual=sum(generationEnergy,na.rm = T)) %>% na.omit() %>% ungroup()
df1$month<-factor(month.abb[df1$month],levels=month.abb)
ggplot(df1) +
  #geom_col(aes(month,Actual,colour="A",fill="A"),size=.01,alpha=0.95,position="dodge")+
  geom_col(data=df1%>%filter(year==2018),aes(month,NREL,fill="B"),size=.01,alpha=0.25,position="dodge") +
  geom_line(aes(month,Actual,color=factor(year),group=factor(year)),size=1.4)+
  scale_fill_manual("",labels = c("NREL Predicted Generation"),values = "blue")+
  scale_color_viridis("Actual Generation",discrete = TRUE,option="A",direction = 1)+
  theme_minimal()+theme(    
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    legend.title = element_text(colour="black", size = 12, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic",hjust=0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Power Generation (kWh per month)",x="",
             title="Leach (7.6kW) vs NREL PVWatts Predicted Generation",
             caption=paste("Source: Generation data via Neurio API, graph by Andrew Leach.",sep=""))
ggsave(file="actual_gen_vs_nrel.png",dpi = 450,width=10)


png<-1
if(png==1)
  set_png(file="actual_load_vs_yeg.png")
ggplot(df1) +
  geom_col(aes(month,Actual_load,colour="A",fill="A"),size=.01,alpha=0.95,position="dodge")+
  geom_col(aes(month,Pred_load,colour="B",fill="B"),size=.01,alpha=0.25,position="dodge") +
  scale_fill_viridis("",labels = c("Actual Load","Pro-rated Edmonton Load"),discrete = TRUE,option="E",direction = -1)+
  scale_color_viridis("",labels = c("Actual Load","Pro-rated Edmonton Load"),discrete = TRUE,option="E",direction = -1)+
  theme_minimal()+theme(    
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic",hjust=0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Power Generation (kWh per month)",x="",
             title="Leach Household Load vs Pro-Rated Edmonton Load Curve",
             caption=paste("Source: Generation data via Neurio API, graph by Andrew Leach.\nCurrent to ",Sys.Date(),".",sep=""))
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()




ggplot(df1) +
  geom_col(data=df1%>%filter(year==2018),aes(month,Pred_load,fill="B"),size=.01,alpha=0.5,position="dodge")+
  geom_line(aes(month,Actual_load,colour=factor(year),linetype="A",group=factor(year)),size=1.25)+
  geom_line(aes(month,Actual_net,colour=factor(year),linetype="B", group=factor(year)),size=1.25)+
  scale_fill_manual("",labels = c("Pro-Rated Edmonton Residential Load Shape"),values = "blue")+
  scale_color_viridis("Actual Gross and Net Load",discrete = TRUE,option="A",direction = 1)+
  scale_linetype_manual("",values=c("solid","21"),labels=c("Gross","Net"))+
  guides(fill=guide_legend(order = 1),colour = guide_legend(order = 2,nrow=2), 
           linetype = guide_legend(order = 3,nrow=2))+
  theme_minimal()+theme(    
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.justification = c(0.5, .5),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 10, face = "bold"),
    legend.title = element_text(colour="black", size = 10, face = "bold"),
    plot.caption = element_text(size = 12, face = "italic",hjust=0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 10,face = "bold"),
    axis.text = element_text(size = 10,face = "bold", colour="black")
  )+    labs(y="Power Deliveries (net kWh per month)",x="",
             title="Leach Household Net-from-Grid vs Typical Edmonton Load Curve",
             caption=paste("Source: Household data via Neurio API, Edmonton data via EPCOR, graph by Andrew Leach.\nLoad is Edmonton residential load pro-rated to equal our 2018 annual household consumption.",sep=""))
ggsave(file="actual_vs_yeg.png",dpi = 450,width=10)



test<-NULL
test<-merge(hourly_data,solar_data,by.x = c("month","hour","day"),by.y = c("Month","Hour","Day"),all = T)
#test$`750_load` <- NULL
#test$`750_load_W` <- NULL
#test$`YEG_Res_Load` <- NULL
#res_data$Hour<-res_data$Hour+1
#test<-merge(test,res_data,by.x = c("month","hour","day"),by.y = c("Month","Hour","Day"),all = T)
yr_res_load<-sum(test$YEG_Res_Load)
avg_cons<-mean(test$consumptionEnergy,na.rm = T)
test<-test %>% mutate(load750=YEG_Res_Load/yr_res_load*avg_cons)
test$Date_NT<-as.Date(test$start)

  assignment_data<-merge(house_data,solar_data,by=c("Date"))
  asst_data<-data.frame(assignment_data$Date,assignment_data$`Use.(kWh)`*750/527,assignment_data$`AC.System.Output.(W)`/1000,assignment_data$`750_load`)
  names(asst_data)<-c("Date","Use_kWh","gen_kWh","avg_load")
  write.xlsx(asst_data, file = "assignment_data.xlsx", colNames = TRUE, borders = "columns") 
  
  lims <- c(as.POSIXct("2016-01-01 0:00:00 MDT"), as.POSIXct("2016-12-31 24:00:00 MDT"))
  
  breaks<-seq.POSIXt(min(lims)+hours(12), max(lims), by="1 month")
  
  png<-0
  if(png==1)
    png(file="actual_vs_avg.png", width = 1400, height = 750,res=130,type='cairo')
  ggplot(asst_data, aes(Date,Use_kWh,colour="A")) +
    geom_line(size=2) +
    geom_line(aes(Date,gen_kWh,colour="D"),size=2)+
    #geom_line(aes(Date,avg_load,colour="B"),size=2)+
    geom_line(aes(Date,Use_kWh-gen_kWh,colour="C"),size=2)+
    #geom_line(aes(start,net_imp,colour="Power/nDeliveries"),size=2)+
    #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
    #scale_color_viridis("",labels = c("Actual\nResidential Use","Average EPCOR\nResidential Load","Household Net\nDeliveries","Household Solar\nGeneration"),discrete = TRUE,option="C")+
    scale_color_viridis("",labels = c("Actual\nResidential Use","Household Net\nDeliveries","Household Solar\nGeneration"),discrete = TRUE,option="C")+
    scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a, %m/%d\n%Hh", tz="America/Denver"))+
    theme_minimal()+theme(    
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 16, face = "bold"),
      plot.caption = element_text(size = 14, face = "italic"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 16, face = "italic"),
      panel.grid.minor = element_blank(),
      text = element_text(size = 16,face = "bold"),
      axis.text = element_text(size = 16,face = "bold", colour="black")
    )+    labs(y="Energy Consumption or Generation (kWh)",x="\nTime",
               title="Household Load and Generation (750kWh/month average use)",
               caption="Source: Consumption data via Mustafa Gul and Hadia Awad\n Average load data via EPCOR,\nGraph by Andrew Leach")
  if(png==1)#set these to only turn on if you're making PNG graphs
    dev.off()
  
  #convert to same format as solar system data
  asst_data$imports<-ifelse(asst_data$Use_kWh-asst_data$gen_kWh>=0,asst_data$Use_kWh-asst_data$gen_kWh,0)
  asst_data$exports<-ifelse(asst_data$Use_kWh-asst_data$gen_kWh<=0,asst_data$gen_kWh-asst_data$Use_kWh,0)
  asst_data$end<-asst_data$Date+hours(1)
  names(asst_data)<-c("start","consumptionEnergy","genenerationEnergy","avg_load","importedEnergy","exportedEnergy","end")
  asst_data$net_to_grid<-asst_data$importedEnergy-asst_data$exportedEnergy




res_data <- read.xlsx(xlsxFile = "YEG_Res_Load.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
res_data$Date<-ymd_h(paste(2016,"-",res_data$Month,"-",res_data$Day," ",res_data$Hour,sep=""))+years(2)


#solar_data$importedEnergy<-(solar_data$`750_load`-solar_data$`AC.System.Output.(W)`)*(solar_data$`750_load`-solar_data$`AC.System.Output.(W)`>0)
#solar_data$exportedEnergy<-(solar_data$`AC.System.Output.(W)`-solar_data$`750_load`)*(solar_data$`AC.System.Output.(W)`-solar_data$`750_load`>0)
solar_data$date<-paste("2017-",solar_data$Month,"-",solar_data$Day," ",solar_data$Hour,":00",sep="")
solar_data$date<-as.POSIXct(solar_data$date,format='%Y-%m-%d %H:%M')

solar_data<-solar_data[(solar_data$date>=start_string&solar_data$date<=end_string),]
test_data<-merge(sys_data,solar_data,by.x="end",by.y="date")


test_10<-sys_data
test_10$start<-round_date(test_10$start, "10 minutes") 
#test_10<-test_10 %>% group_by(start) %>% summarise(Match = sum(Match))
test_10<-aggregate(. ~ start, test_10, mean)
test_10$end<-test_10$start+minutes(10)
test_10<-test_10 %>% mutate(month=month(start),hour=hour(start)+1,day=day(start),min=minute(start))

#bring in EV data
#Matteo Muratori, Impact of uncoordinated plug-in electric vehicle charging on residential power demand - supplementary data, 2017 [1]. Based on modeling reported in [2]."
#[1] M. Muratori, "Impact of uncoordinated plug-in electric vehicle charging on residential power demand." Forthcoming.
#[2] M. Muratori, M. J. Moran, E. Serra, and G. Rizzoni, "Highly-resolved modeling of personal transportation energy consumption in the United States," Energy, vol. 58, no. 0, pp. 168-177, 2013.


ev_data <- read.csv(file = "PEV-Profiles-L2.csv", skip = 2)
ev_data$Time<-mdy_hm(as.character(ev_data$Time))
ev_data <-ev_data %>% mutate(month=month(Time),hour=hour(Time)+1,day=day(Time),min=minute(Time))
ev_data$mean<-rowMeans(ev_data[,-1])
ev_data$max<-apply(ev_data[,-1],1,max)
ev_data$min<-apply(ev_data[,-1],1,min)

png<-1
if(png==1)
  set_png(file="indiv_vs_avg_ev.png")
ggplot(subset(ev_data,month(Time)==5 & day(Time)<15)) +
  #geom_line(aes(start,EV_Charge,colour="EV Charge"),size=2) +
  #geom_line(aes(start,generationEnergy,colour="Actual Generation"),size=2)
  geom_line(aes(Time,Vehicle.12/1000,colour="B"),size=2)+ 
  geom_line(aes(Time,mean/1000,colour="A"),size=2,alpha=1)+
  scale_color_viridis("",labels = c("Average EV Load","Individual Load from\n LV2 EV Charging"),discrete = TRUE,option="C")+
  scale_x_datetime(date_breaks = "2 day", labels = date_format("%a\n%b %d \n%Hh"))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic",hjust=0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 13, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="10 minute average load (kW)",x="",
             title="EV Individual Load vs Average Load, Muratori (2017)",
             #subtitle="Actual vs Predicted Net Household Load with 7.6kW solar array and a EV with Level 2 (30Amp) charger driven 25,000km/yr",
             caption="Predicted EV Load Data via Muratori (2017) (Vehicle 12 and overall mean)\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()
  

ev_dat<-melt(ev_data,id=c("Time","mean"),measure.vars = c("Vehicle.12"),variable.name = "Vehicle",value.name = "EV_Charge")
ev_dat$Time<-round_date(ev_dat$Time, "1 hour") 
ev_dat$Vehicle<-NULL
ev_dat<-aggregate(. ~ Time, ev_dat, mean)
names(ev_dat)[grep("mean",names(ev_dat))]<-"Mean_EV_load"
names(ev_dat)[grep("EV_Charge",names(ev_dat))]<-"Indiv_EV_load"
ev_dat <-ev_dat %>% mutate(month=month(Time),hour=hour(Time)+1,day=day(Time))
ev_dat$wday<-wday(ev_dat$Time)

#res_data$Date<-res_data$Date-years(3)
res_data$wday<-wday(res_data$Date)
res_sum<-res_data %>% group_by(Month,Hour) %>% summarize(Res_Load=mean(YEG_Res_Load))

ev_merge<-merge(ev_dat,res_sum,by.x=c("month","hour"),by.y=c("Month","Hour"))
ev_merge<-arrange(ev_merge,Time)
yr_res_load<-sum(ev_merge$Res_Load)
ev_merge<-ev_merge %>% mutate(load650=Res_Load/yr_res_load*650*12)
ev_merge<-merge(ev_merge,solar_data,by.x=c("month","day","hour"),by.y=c("Month","Day","Hour"))

ev_merge$hour<-factor(ev_merge$hour,levels=seq(1,24))
ev_merge$`AC.System.Output.(W)`<-ev_merge$`AC.System.Output.(W)`/787.9482*650

png<-1
if(png==1)
  set_png(file="may_avg_load_vs_ev.png")
ggplot(subset(ev_merge,month(Time)==5)) +
  geom_line(aes(hour,.2*Mean_EV_load/1000+load650,colour="B",group=day),size=1)+
  geom_line(aes(hour,load650-.2*`AC.System.Output.(W)`/1000,colour="C",group=day),size=1)+
  #geom_line(aes(hour,.2*Mean_EV_load/1000+load650-.2*`AC.System.Output.(W)`/1000,colour="D",group=day),size=1)+
geom_line(aes(hour,load650,colour="A",group=day),size=2)+
#scale_color_viridis("",labels=c("Current Average Load","20% with EVs","20% with Solar","20% with Solar and 20% with EVs"),discrete = TRUE,option="E")+
  scale_color_viridis("",labels=c("Current Average Load","20% with EVs","20% with Solar"),discrete = TRUE,option="E")+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic",hjust=0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 13, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Hourly Average Household Net Consumption (kWh)",x="Hour Ending",
             title="Impact of Adding Electric Vehicles to the Grid - May in Edmonton",
             subtitle="Actual vs predicted loads with 20% of offset by solar PV or with 20% of households with EVs driven 25,000km/yr",
             caption="Month-hour load data via Epcor, hourly predicted solar generation via NREL PVWatts\nHourly EV Load Data adapted from Muratori (2017).    Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

png<-1
if(png==1)
  set_png(file="dec_avg_load_vs_ev.png")
ggplot(subset(ev_merge,month(Time)==12)) +
  geom_line(aes(hour,.2*Mean_EV_load/1000+load650,colour="B",group=day),size=1)+
  geom_line(aes(hour,load650-.2*`AC.System.Output.(W)`/1000,colour="C",group=day),size=1)+
  #geom_line(aes(hour,.2*Mean_EV_load/1000+load650-.2*`AC.System.Output.(W)`/1000,colour="D",group=day),size=1)+
  geom_line(aes(hour,load650,colour="A",group=day),size=2)+
  #scale_color_viridis("",labels=c("Current Average Load","20% with EVs","20% with Solar","20% with Solar and 20% with EVs"),discrete = TRUE,option="E")+
  scale_color_viridis("",labels=c("Current Average Load","20% with EVs","20% with Solar"),discrete = TRUE,option="E")+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic",hjust=0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 13, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Hourly Average Household Net Consumption (kWh)",x="Hour Ending",
             title="Impact of Adding Electric Vehicles to the Grid - December in Edmonton",
             subtitle="Actual vs predicted loads with 20% of offset by solar PV or with 20% of households with EVs driven 25,000km/yr",
             caption="Month-hour load data via Epcor, hourly predicted solar generation via NREL PVWatts\nHourly EV Load Data adapted from Muratori (2017).    Graph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)
  set_png(file="may_ev_load.png")
ggplot(subset(ev_merge,month(Time)==5 & wday != 1 & wday!=7)) +
  geom_line(aes(hour,Mean_EV_load,colour="B",group=day),size=1)+
  #geom_line(aes(hour,load650-.2*`AC.System.Output.(W)`/1000,colour="C",group=day),size=1)+
  #geom_line(aes(hour,.2*Mean_EV_load/1000+load650-.2*`AC.System.Output.(W)`/1000,colour="D",group=day),size=1)+
  #geom_line(aes(hour,load650,colour="A",group=day),size=2)+
  #scale_color_viridis("",labels=c("Current Average Load","20% with EVs","20% with Solar","20% with Solar and 20% with EVs"),discrete = TRUE,option="E")+
  scale_color_viridis("",labels=c("Mean EV Load"),discrete = TRUE,option="E")+
  theme_minimal()+theme(    
    legend.position = "none",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic",hjust=0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 13, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Hourly Average EV load (kWh)",x="Hour Ending",
             title="Daily EV Average Hourly Load",
             subtitle="Hourly mean predicted loads for each weekday in May from Muratori (2017)",
             caption="Hourly EV Load Data from Muratori (2017) https://data.nrel.gov/submissions/69.\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


ev_merge$wday<-ifelse(ev_merge$wday %in% c(1,7),0,1) #weekday=1
df1<-ev_merge %>% group_by(hour,wday) %>% summarize(EV_load=mean(Mean_EV_load))%>% ungroup()
ev_merge$hour<-factor(ev_merge$hour,levels=seq(1,24))
df1$wday<-factor(df1$wday,levels = c(1,0),labels = c("Weekday","Weekend") )
png<-1
if(png==1)
  set_png(file="ev_all.png")
ggplot(df1) +
  geom_area(aes(hour,EV_load,group=wday),size=1,fill="blue")+
  facet_wrap(~wday,ncol = 2)+
  #geom_line(aes(hour,load650-.2*`AC.System.Output.(W)`/1000,colour="C",group=day),size=1)+
  #geom_line(aes(hour,.2*Mean_EV_load/1000+load650-.2*`AC.System.Output.(W)`/1000,colour="D",group=day),size=1)+
  #geom_line(aes(hour,load650,colour="A",group=day),size=2)+
  #scale_color_viridis("",labels=c("Current Average Load","20% with EVs","20% with Solar","20% with Solar and 20% with EVs"),discrete = TRUE,option="E")+
  scale_color_viridis("",labels=c("Mean EV Load"),discrete = TRUE,option="E")+
  scale_y_continuous(limits=c(0,1000))+
  theme_minimal()+theme(    
    legend.position = "none",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic",hjust=0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 13, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text = element_text(size = 12,face = "bold", colour="black")
  )+    labs(y="Hourly EV load (MW)",x="Hour Ending",
             title="Daily EV Average Hourly Load",
             subtitle="Hourly mean predicted loads for weekdays and weekends from Muratori (2017)",
             caption="Hourly EV Load Data adapted from Muratori (2017) https://data.nrel.gov/submissions/69.\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





#convert from Watts to kW energy rate
test_10$EV_Charge<-test_10$EV_Charge/1000
test_10$EV_Net<-test_10$net_to_grid+test_10$EV_Charge

test_10$start<-round_date(test_10$start, "1 hour") 




lims <- c(min(test_10$start),max(test_10$start))
#breaks<- as.POSIXct(seq.POSIXt(min(sys_data$start),max(sys_data$start), by = "1 month") )

breaks<-seq.POSIXt(min(test_10$start)+hours(12), max(test_10$start), by="1 month")

png<-1
if(png==1)
  set_png(file="actual_load_vs_ev.png")
ggplot(subset(test_10,month(start)==5 & day(start)<15)) +
  #geom_line(aes(start,EV_Charge,colour="EV Charge"),size=2) +
  #geom_line(aes(start,generationEnergy,colour="Actual Generation"),size=2)
  geom_line(aes(start,EV_Net,colour="B"),size=2)+ 
  geom_line(aes(start,net_to_grid,colour="A"),size=2,alpha=1)+
    scale_color_viridis("",labels = c("Actual Power Flows","Power Flows w LV2 EV Charging"),discrete = TRUE,option="C")+
  scale_x_datetime(date_breaks = "2 day", labels = date_format("%a\n%b %d \n%Hh", tz="America/Denver"))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic",hjust=0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 13, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="10 minute average load (kW)",x="\nTime",
             title="Impact of Adding An Electric Vehicle",
             subtitle="Actual vs Predicted Net Household Load with 7.6kW solar array and a EV with Level 2 (30Amp) charger driven 25,000km/yr",
             caption="Consumption and solar generation via Leach Household Neurio Monitor\nPredicted EV Load Data via Muratori (2017) (Vehicle 12)\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



}

sys_data<-sys_data %>%filter(year(end)!=2021)







hourly_patterns<-function(){

    
df1 <- sys_data %>% filter(year(start)>2018)%>% mutate(time=cut(start, breaks = "30 min")) %>%
     group_by(year,month,time) %>% summarise(gen=mean(generationEnergy),imps=mean(importedEnergy),cons=mean(consumptionEnergy),net=mean(net_to_grid))
df1$month = factor(month.abb[df1$month], levels=month.abb[])

df1$month_test<-paste(as.character(df1$month),df1$year,sep="-")
df1$month_test = factor(df1$month_test,levels=unique(df1$month_test))

print(levels(df1$month_test))

breaks=unique(df1$time)[seq(1, length(unique(df1$time)), 12) ]

png<-1
if(png==1)
  set_png("hourly_gen.png")

p<-ggplot(df1)+
  geom_line(aes(time,gen,colour=month_test,group=month_test),size=2)+
  scale_color_viridis("",discrete=TRUE)+
  scale_x_discrete(expand=c(0,0),breaks=breaks)+
  scale_y_continuous(expand=c(0,0))+
  ajl_line()+theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(x="Time",y="Generation (kW)",
       title="Monthly Solar Generation (2017-18 Avg)",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
if(png==1)
  dev.off()

png<-1
if(png==1)
  set_png("hourly_jan_may.png")
p<-ggplot(filter(df1,(month=="Jan" | month=="May"),year=="2018"))+
  geom_line(aes(time,gen,colour=month_test,group=month_test),size=2)+
  scale_color_viridis("",discrete=TRUE)+
  scale_x_discrete(expand=c(0,0),breaks=breaks)+
  scale_y_continuous(expand=c(0,0))+
  ajl_line()+theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(x="Time",y="Hourly Generation (kW)",
       title="Monthly Solar Generation (2017-18 Avg)",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
if(png==1)
  dev.off()



png<-1
if(png==1)
  set_png("cons_jan_may.png")
p<-ggplot(subset(df1,month=="Jan" | month=="May"))+
  geom_line(aes(time,cons,colour=month_test,group=month_test),size=2)+
  scale_color_viridis("",discrete=TRUE)+
  scale_x_discrete(expand=c(0,0),breaks=breaks)+
  scale_y_continuous(expand=c(0,0))+
  ajl_line()+theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(x="Time",y="Average Power Use (kW)",
       title="Electricity Use (Monthly Average)",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
if(png==1)
  dev.off()

png<-1
if(png==1)
  set_png("net_jan_may.png")
p<-ggplot(subset(df1,month=="Jan" | month=="May"))+
  geom_line(aes(time,net,colour=month_test,group=month_test),size=2)+
  geom_hline(aes(yintercept=0),size=1,colour="black")+
  scale_color_viridis("",discrete=TRUE)+
  scale_x_discrete(expand=c(0,0),breaks=breaks)+
  scale_y_continuous(expand=c(0,0))+
  ajl_line()+theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(x="Time",y="Power Use Net of Solar Generation (kW)",
       title="Hourly Net Electricity Use (Monthly Average)",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
if(png==1)
  dev.off()

png<-1
if(png==1)
  set_png("net_all.png")
p<-ggplot(df1)+
  geom_line(aes(time,net,colour=month_test,group=month_test),size=2)+
  geom_hline(aes(yintercept=0),size=1,colour="black")+
  scale_color_viridis("",discrete=TRUE)+
  scale_x_discrete(expand=c(0,0),breaks=breaks)+
  scale_y_continuous(expand=c(0,0))+
  ajl_line()+theme(axis.text.x=element_text(angle = -45, hjust = 0))+
  labs(x="Time",y="Power Use Net of Solar Generation (kW)",
       title="Hourly Net Electricity Use (Monthly Average)",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
if(png==1)
  dev.off()

}
#load_and_update()
hourly_patterns()



fun_graphs<-function(){
load_and_update()
day_list<-seq.Date(from=as.Date("2018-05-29"),to=as.Date("2018-05-30"),by="1 day")

df1 <- sys_data %>% filter(as.Date(date) %in% day_list) %>% group_by(date,hour,month,year) %>% summarise(gen=mean(generationEnergy),imps=mean(importedEnergy),cons=mean(consumptionEnergy),net=mean(net_to_grid))

df1$month = factor(month.abb[df1$month], levels=month.abb[])

df1$month_test<-paste(as.character(df1$month),df1$year,sep="-")
df1$month_test = factor(df1$month_test,levels=unique(df1$month_test))
df1$hour<-factor(df1$hour,levels = seq(0,23),labels=paste(seq(0,23),":00",sep=""))
df1$day<-as.factor(as.Date(df1$date))

png<-1
if(png==1)
  set_png("sunny_cloudy.png")
p<-ggplot(df1)+
  geom_line(aes(hour,gen,colour=day,group=day),size=2)+
  #geom_density(aes(fill="Wind Power Generation",colour=year(Time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  scale_color_viridis("",discrete=TRUE,labels=c("Sunny day (May 29, 2018)","Cloudy Day (May 30, 2018)"))+
  #scale_fill_viridis(discrete=TRUE)+
  #scale_x_discrete()+
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+theme(    
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(angle = -45, hjust = 0)
  )+
  labs(x="Hour",y="Average Hourly Generation (kWh)",
       title="Hourly Solar Generation (2017-18 Avg)",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
if(png==1)
  dev.off()


day_list<-c(ymd("2018-08-12"),ymd("2018-08-15"),ymd("2017-08-17"),ymd("2017-08-21"))
df1 <- sys_data %>% filter(as.Date(date) %in% day_list) %>% mutate(time=strftime(start, format="%H:%M")) %>%
  group_by(date,time) %>% summarise(gen=mean(generationEnergy),imps=mean(importedEnergy),cons=mean(consumptionEnergy),net=mean(net_to_grid))
df1$day<-as.factor(as.Date(df1$date))

breaks=unique(df1$time)[seq(1, length(unique(df1$time)), 12) ]

png<-1
if(png==1)
  set_png("smoky.png")
p<-ggplot(df1)+
  geom_line(aes(time,gen,colour=day,group=day),size=2)+
  #geom_density(aes(fill="Wind Power Generation",colour=year(Time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  scale_color_viridis("",discrete=TRUE,labels=c("Sunny day (August 17, 2017)","Solar eclipse (August 21, 2017)","Rainy day (August 12, 2018)","Smoky day (August 15, 2018)"))+
  #scale_fill_viridis(discrete=TRUE)+
  #scale_x_time()+
  scale_x_discrete(expand=c(0,0),breaks=breaks)+
  scale_y_continuous(expand=c(0,0))+
  guides(colour=guide_legend(nrow=2))+
  theme_classic()+theme(    
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"),
    axis.text.x=element_text(angle = -45, hjust = 0)
  )+
  labs(x="Time",y="5 minute average generation (kW)",
       title="How much does wildfire smoke affect solar generation?",
       caption="Source: SolarPeople system data via Neurio API for a 7.6kW south-facing solar array in Edmonton, AB\nGraph by Andrew Leach")
print(p)
if(png==1)
  dev.off()


lims <- c(as.POSIXct("2017-08-21 6:00:00 MDT"), as.POSIXct("2017-08-21 17:00:00 MDT"))
breaks <- make_breaks(min(sys_data$start), hour=0, interval='2 hour', length.out=length(sys_data))

png<-1
if(png==1)
  set_png(file="eclipse.png")
p<-ggplot(sys_data) +
  geom_line(aes(start,generationEnergy,colour="Solar Generation"),size=2) +
  #geom_line(aes(start,generationEnergy,colour="Generation"),size=2)+
  #geom_line(aes(start,net_to_grid,colour="Electricity from grid"),size=2)+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.POSIXct("2017-08-21 10:24:00 MDT"), xmax =min(as.POSIXct("2017-08-21 12:49:00 MDT")),
           ymin = -Inf, ymax = Inf) +
  annotate("text", x = as.POSIXct("2017-08-21 11:36:00 MDT"), y = 0.5, label = "Solar eclipse\n2017",size=6)+
  scale_color_viridis("",labels = c("Solar Generation"),discrete = TRUE)+
  scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  theme_minimal()+theme(    
    legend.position = "none",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Energy (kW)",x="",
             title="Residential rooftop solar generation during the 2017 solar eclipse",
             caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



lims <- c(as.POSIXct("2017-12-26 6:00:00 MDT"), as.POSIXct("2017-12-26 20:00:00 MDT"))
breaks <- make_breaks(min(lims), hour=1, interval='2 hour', length.out=12)

png<-1
if(png==1)
  set_png(file="snow_clearing.png")
p<-ggplot(subset(sys_data,start>= as.POSIXct("2017-12-26 6:00:00 MDT") & start <= as.POSIXct("2017-12-26 20:00:00 MDT")))+
  geom_line(aes(start,generationEnergy,colour="Solar Generation"),size=2) +
  #geom_line(aes(start,generationEnergy,colour="Generation"),size=2)+
  #geom_line(aes(start,net_to_grid,colour="Electricity from grid"),size=2)+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.POSIXct("2017-12-26 12:30:00 MDT"), xmax =min(as.POSIXct("2017-12-26 20:00:00 MDT")),
           ymin = 0, ymax = 2) +
  annotate("text", x = as.POSIXct("2017-12-26 17:00:00 MDT"), y = 1, label = "After brushing off\nmy solar panels",size=6)+
  annotate("text", x = as.POSIXct("2017-12-26 8:00:00 MDT"), y = 1, label = "Before brushing off\nmy solar panels",size=6)+
  scale_color_viridis("",labels = c("Solar Generation"),discrete = TRUE)+
  scale_x_datetime(limits=lims,breaks = breaks, labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  #scale_y_continuous(limits=c(0,2))+
  theme_minimal()+theme(    
    legend.position = "none",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Energy (kW)",x="",
             title="When you clean the snow off your solar panels",
             caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



}

#fun_graphs()




usage_graphs<-function(){
load_and_update()
day_list<-seq.Date(from=as.Date("2018-6-02"),to=as.Date("2018-06-2"),by="1 day")

df1 <- sys_data %>% filter(as.Date(date) %in% day_list) 

df1$month = factor(month.abb[df1$month], levels=month.abb[])

df1$month_test<-paste(as.character(df1$month),df1$year,sep="-")
df1$month_test = factor(df1$month_test,levels=unique(df1$month_test))
df1$hour<-factor(df1$hour,levels = seq(0,23),labels=paste(seq(0,23),":00",sep=""))
df1$day<-as.factor(as.Date(df1$date))

png<-1
if(png==1)
  set_png("power_use.png")
p<-ggplot(df1)+
  geom_line(aes(start,consumptionEnergy),size=2)+
  scale_x_datetime(labels = date_format("%b %d\n %H:%M", tz="America/Denver"))+
  scale_y_continuous(expand=c(0,0),limits=c(0,5))+
  theme_classic()+theme(    
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"))+
  labs(x="Hour",y="Electricity Use (kWh/h)",
       title="Power Use (5 minute data)",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
print(p)
if(png==1)
  dev.off()

day_list<-seq.Date(from=as.Date("2018-6-03"),to=as.Date("2018-06-3"),by="1 day")

df1 <- sys_data %>% filter(as.Date(start) %in% day_list) 

df1$month = factor(month.abb[df1$month], levels=month.abb[])

df1$month_test<-paste(as.character(df1$month),df1$year,sep="-")
df1$month_test = factor(df1$month_test,levels=unique(df1$month_test))
df1$hour<-factor(df1$hour,levels = seq(0,23),labels=paste(seq(0,23),":00",sep=""))
df1$day<-as.factor(as.Date(df1$start))

library(ggrepel)
p<-ggplot(df1)+
  geom_line(aes(start,consumptionEnergy),size=2)+
  scale_x_datetime(labels = date_format("%b %d\n %H:%M", tz="America/Denver"))+
  scale_y_continuous(expand=c(0,0),limits=c(0,6))+
  #geom_text_repel() +
  annotate("text", x = df1$start[1], y = 4.5, label = "Dryer",size=4)+
  annotate("text", x = df1$start[34], y = 4.5, label = "Dryer",size=4)+
  annotate("text", x = df1$start[150], y = 2.5, label = "Coffee",size=4)+
  annotate("text", x = df1$start[160], y = 4, label = "Breakfast",size=4)+
  annotate("text", x = df1$start[176], y = 5.65, label = "Dryer and\ntoaster",size=4)+
  annotate("text", x = df1$start[188], y = 3.75, label = "More\ncoffee",size=4)+
  annotate("text", x = df1$start[193], y = 4.75, label = "Snack Time",size=4)+
  annotate("text", x = df1$start[198], y = 5.75, label = "Dryer",size=4)+
  annotate("text", x = df1$start[217], y = 3, label = "Cooking\nLunch",size=4)+
  annotate("text", x = df1$start[237], y = 5, label = "Cooking\nStuff\nfor\nSupper",size=4)+
  annotate("text", x = df1$start[270], y = 5, label = "Cooking\nSupper",size=4)+
  theme_classic()+theme(    
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"))+
  labs(x="",y="Metered Consumption (kW)",
       title="My Household Electricity Use (5 minute data)",
       subtitle=paste("Days: 1. Energy: ",round(sum(df1$consumptionEnergy/12),2),"kWh. Demand: ",round(max(df1$consumptionEnergy),2),"kW",sep=""),
       caption="Source: SolarPeople system data via Neurio API, graph by Andrew Leach")
print(p)
ggsave("power_use_day.png",dpi=300,width=16,height=7,bg="white")


p<-ggplot(df1)+
  geom_line(aes(start,net_to_grid),size=2)+
  geom_hline(yintercept = 0,linewidth=.75,lty="11")+
  #geom_line(aes(start,generationEnergy),size=2,colour="yellow")+
  scale_x_datetime(labels = date_format("%b %d\n %H:%M", tz="America/Denver"))+
  scale_y_continuous(expand=c(0,0), breaks=pretty_breaks())+
  expand_limits(y=c(-5,6.5))+
  #geom_text_repel() +
  annotate("text", x = df1$start[1], y = 4.5, label = "Dryer",size=4)+
  annotate("text", x = df1$start[34], y = 4.5, label = "Dryer",size=4)+
  annotate("text", x = df1$start[150], y = 2.5, label = "Coffee",size=4)+
  annotate("text", x = df1$start[160], y = 4, label = "Breakfast",size=4)+
  annotate("text", x = df1$start[176], y = 5.65, label = "Dryer and\ntoaster",size=4)+
  annotate("text", x = df1$start[188], y = 3.75, label = "More\ncoffee",size=4)+
  annotate("text", x = df1$start[193], y = 4.75, label = "Snack Time",size=4)+
  annotate("text", x = df1$start[198], y = 5.75, label = "Dryer",size=4)+
  annotate("text", x = df1$start[217], y = 3, label = "Cooking\nLunch",size=4)+
  annotate("text", x = df1$start[237], y = 5, label = "Cooking\nStuff\nfor\nSupper",size=4)+
  annotate("text", x = df1$start[270], y = 5, label = "Cooking\nSupper",size=4)+
  theme_classic()+theme(    
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"))+
  labs(x="",y="Metered Consumption (kW)",
       title="My Household Net Electricity Use (5 minute data)",
       subtitle=paste("Days: 1. Energy: ",round(sum(df1$net_to_grid/12),2),"kWh. Demand: ",round(max(df1$net_to_grid),2),"kW",sep=""),
       caption="Source: SolarPeople system data via Neurio API, graph by Andrew Leach")
print(p)
ggsave("power_use_net.png",dpi=300,width=16,height=7,bg="white")





p<-ggplot(df1)+
  geom_line(aes(start,generationEnergy),size=2,color="yellow")+
  geom_line(aes(start,generationEnergy),size=2,lty="22")+
  #geom_line(aes(start,generationEnergy),size=2,colour="yellow")+
  scale_x_datetime(labels = date_format("%b %d\n %H:%M", tz="America/Denver"))+
  scale_y_continuous(expand=c(0,0), breaks=pretty_breaks())+
  expand_limits(y=c(0,6.5))+
  theme_classic()+theme(    
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"))+
  labs(x="",y="Metered Consumption (kW)",
       title="My Household Solar Generation (5 minute data)",
       subtitle=paste("Days: 1. Energy generation: ",round(sum(df1$generationEnergy/12),2),"kWh. Peak Generation: ",round(max(df1$generationEnergy),2),"kW",sep=""),
       caption="Source: SolarPeople system data via Neurio API, graph by Andrew Leach")
print(p)
ggsave("power_gen_dist.png",dpi=300,width=16,height=7,bg="white")




p<-ggplot(df1)+
  geom_line(aes(start,pmax(net_to_grid,0)),size=2)+
  #geom_line(aes(start,generationEnergy),size=2,colour="yellow")+
  scale_x_datetime(labels = date_format("%b %d\n %H:%M", tz="America/Denver"))+
  scale_y_continuous(expand=c(0,0), breaks=pretty_breaks())+
  expand_limits(y=c(0,6.5))+
  #geom_text_repel() +
  annotate("text", x = df1$start[1], y = 4.5, label = "Dryer",size=4)+
  annotate("text", x = df1$start[34], y = 4.5, label = "Dryer",size=4)+
  annotate("text", x = df1$start[150], y = 2.5, label = "Coffee",size=4)+
  annotate("text", x = df1$start[160], y = 4, label = "Breakfast",size=4)+
  annotate("text", x = df1$start[176], y = 5.65, label = "Dryer and\ntoaster",size=4)+
  annotate("text", x = df1$start[188], y = 3.75, label = "More\ncoffee",size=4)+
  annotate("text", x = df1$start[193], y = 4.75, label = "Snack Time",size=4)+
  annotate("text", x = df1$start[198], y = 5.75, label = "Dryer",size=4)+
  annotate("text", x = df1$start[217], y = 3, label = "Cooking\nLunch",size=4)+
  annotate("text", x = df1$start[237], y = 5, label = "Cooking\nStuff\nfor\nSupper",size=4)+
  annotate("text", x = df1$start[270], y = 5, label = "Cooking\nSupper",size=4)+
  theme_classic()+theme(    
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"))+
  labs(x="",y="Metered Consumption (kW)",
       title="My Household Distribution Energy Billing Determinant (5 minute data)",
       subtitle=paste("Days: 1. Energy: ",round(sum(df1$net_to_grid/12*pmax(df1$net_to_grid,0)),2),"kWh. Demand: ",round(max(df1$net_to_grid),2),"kW",sep=""),
       caption="Source: SolarPeople system data via Neurio API, graph by Andrew Leach")
print(p)
ggsave("power_use_dist.png",dpi=300,width=16,height=7,bg="white")



png<-1
if(png==1)
  set_png("power_gen_day.png")
library(ggrepel)
p<-ggplot(df1)+
  geom_line(aes(start,generationEnergy),size=2)+
  scale_x_datetime(labels = date_format("%b %d\n %H:%M", tz="America/Denver"))+
  #scale_y_continuous(expand=c(0,0),limits=c(0,6))+
  #geom_text_repel() +
#  annotate("text", x = df1$start[1], y = 4.5, label = "Dryer",size=4)+
#  annotate("text", x = df1$start[34], y = 4.5, label = "Dryer",size=4)+
#  annotate("text", x = df1$start[150], y = 2.5, label = "Coffee",size=4)+
#  annotate("text", x = df1$start[160], y = 4, label = "Breakfast",size=4)+
#  annotate("text", x = df1$start[176], y = 5.65, label = "Dryer and\ntoaster",size=4)+
#  annotate("text", x = df1$start[188], y = 3.75, label = "More\ncoffee",size=4)+
#  annotate("text", x = df1$start[193], y = 4.75, label = "Snack Time",size=4)+
#  annotate("text", x = df1$start[198], y = 5.75, label = "Dryer",size=4)+
#  annotate("text", x = df1$start[217], y = 3, label = "Cooking\nLunch",size=4)+
#  annotate("text", x = df1$start[237], y = 5, label = "Cooking\nStuff\nfor\nSupper",size=4)+
#  annotate("text", x = df1$start[270], y = 5, label = "Cooking\nSupper",size=4)+
  theme_classic()+theme(    
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black"))+
  labs(x="Hour",y="Electricity Generation (kWh/h)",
       title="Power Generation (5 minute data)",
       caption="Source: SolarPeople system data via Neurio API, graph by Andrew Leach")
print(p)
if(png==1)
  dev.off()

png<-1
if(png==1)
  set_png("power_net_day.png")
p<-ggplot(df1)+
  geom_col(data=filter(df1,net_to_grid>0),aes(start,net_to_grid,colour=
                    "Deliveries",fill="Deliveries"),size=1)+
  geom_col(data=filter(df1,net_to_grid<0),aes(start,-net_to_grid,colour=
                                                "Receipts",fill="Receipts"),size=1)+
  geom_line(aes(start,generationEnergy,colour="Solar Power Generation",fill="Solar Power Generation"),size=2)+
  scale_x_datetime(labels = date_format("%b %d\n %H:%M", tz="America/Denver"))+
  scale_fill_manual("",values=colors_ua10()[c(1,3,2)],labels=c("Deliveries (Purchases)","Receipts (Sales)","Generation (Solar PV)"))+
  scale_color_manual("",values=colors_ua10()[c(1,3,2)],labels=c("Deliveries (Purchases)","Receipts (Sales)","Generation (Solar PV)"))+
 theme_classic()+theme(    
  plot.margin = margin(1, 1, 1, 1, "cm"),
  legend.position = "bottom",
  legend.margin=margin(c(0,0,0,0),unit="cm"),
  legend.text = element_text(colour="black", size = 16, face = "bold"),
  plot.caption = element_text(size = 14, face = "italic"),
  plot.title = element_text(face = "bold"),
  plot.subtitle = element_text(size = 16, face = "italic"),
  panel.grid.minor = element_blank(),
  text = element_text(size = 16,face = "bold"),
  axis.text = element_text(size = 14,face = "bold", colour="black"))+
  labs(x="",y="Electricity Net-to/from-Grid (kWh/h)",
       title="Purchases and Sales of Power Through the Day",
       caption="Source: SolarPeople system data via Neurio API, graph by Andrew Leach")
print(p)
if(png==1)
  dev.off()

}


#usage_graphs()



peak_graphs<-function(){

df1 <- sys_data %>% group_by(month,hour) %>% summarise(gen=mean(generationEnergy),imps=mean(importedEnergy),cons=mean(consumptionEnergy),net=mean(net_to_grid),
                                                       max_net=max(abs(consumptionEnergy-generationEnergy)),max_cons=max(consumptionEnergy))
df1$month = factor(month.abb[df1$month], levels = month.abb)
png<-1
if(png==1)
  set_png(file="hourly_peak_cons.png", width = 1400, height = 900)
ggplot(df1)+
  geom_line(aes(hour,max_cons,colour=month,group=month),size=2)+
  geom_line(aes(hour,max_net,colour=month,group=month),size=2,linetype=3)+
  #geom_density(aes(fill="Wind Power Generation",colour=year(Time)),alpha=0.5)+
  #stat_density(geom="line",position="identity",aes(group=Year_ID,colour=Year_ID),size=1.5)+
  scale_color_viridis("",discrete=TRUE)+
  scale_fill_viridis(discrete=TRUE)+
  #scale_x_discrete()+
  #scale_x_discrete(limits = c(0,23))+
  scale_y_continuous(expand=c(.05,.05))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+
  labs(x="Hour",y="Peak load (kW)",
       title="Affect of solar on monthly peak loads (2017-2018)",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
if(png==1)
  dev.off()

}


#SOLAR FINANCIAL RETURNS


#build a lifetime system simulator

#grab 2018 sytem data
data_2018<-sys_data %>% filter(year(start)==2018) %>% assign_date_time_days(time_var = start) %>% 
  group_by(month,wday,hour) %>% summarize(generationEnergy=mean(generationEnergy),
                                          consumptionEnergy=mean(consumptionEnergy))

#create 25 year file

data_life<-data.frame(
  seq.POSIXt(from = min(sys_data$start)-minutes(35),to=min(sys_data$start)-minutes(35)+years(25),by="1 hour"),
  stringsAsFactors = F)
names(data_life)[1]<-"start"
loss_rate<-0.01 #1% annual degredation
data_life<-data_life%>% assign_date_time_days(time_var =start) %>% left_join(data_2018,by=c("month","wday","hour")) %>%
  mutate(generationEnergy=generationEnergy*(1-loss_rate)^(year(start)-2017),
         he_time=start+hours(1))
#now build prices


#load forecast data
load("../alberta_power/forecast_data.Rdata")
forecast_data$day_ahead_forecasted_ail<- gsub("\\,", "", forecast_data$day_ahead_forecasted_ail)
forecast_data$actual_ail<- gsub("\\,", "", forecast_data$actual_ail)
forecast_data<-forecast_data %>% mutate_at(names(forecast_data)[2:6],as.numeric)
forecast_data<-forecast_data %>% filter(!is.na(actual_posted_pool_price))


data_life<-data_life %>% left_join(forecast_data%>%select(time,actual_posted_pool_price,actual_ail), by=c("he_time"="time"))
names(data_life)[grep("actual_posted_pool_price",names(data_life))]<-"pool_price"

#now get NGX forward prices

proc_forwards<-function(file_sent,type){
  forwards<-read.csv(file=file_sent,header = TRUE, stringsAsFactors=FALSE,skip=1)
  formats<-unique(guess_formats(forwards[,1],"mdY"))
  forwards[,1]<-as.Date(forwards[,1],format=formats)
  forwards[,2]<-as.Date(forwards[,2],format=formats)
  forwards<-forwards[,-c(3,4,5)]
  colnames(forwards)<-c("Trade_Date","Inst_Date","Settle","Volume_MW","Open_Int_MW")
  forwards$Inst_Year<-year(forwards$Inst_Date)
  forwards$Inst_Month<-month(forwards$Inst_Date)
  forwards$Type<-type
  return(forwards)
}

peak_forwards<-proc_forwards("../alberta_power/peak_forwards.csv","PEAK")
off_peaks<-proc_forwards("../alberta_power/off_peak_forwards.csv","OFF_PEAK")
peak_forwards_daily<-proc_forwards("../alberta_power/peak_forwards_daily.csv","PEAK")
off_peaks_daily<-proc_forwards("../alberta_power/off_peak_forwards_daily.csv","OFF_PEAK")

#stack new ones
forwards<-rbind(peak_forwards,off_peaks)%>%filter(Trade_Date==max(Trade_Date)) %>%
  select(Inst_Date,Settle,Type) %>% assign_date_time_days(time_var =Inst_Date) %>%
  group_by(month,year,Type) %>% summarize(pool_price=mean(Settle)) 

#set up peak hours in data_life
data_life<-data_life%>% assign_peaks(time_var =start) %>%
  mutate(Type=case_when(
      on_peak ~ "PEAK",
      off_peak ~ "OFF_PEAK",
      TRUE ~ "x"
    )
  ) %>%
  select(-c(on_peak,off_peak,ext_peak,stat,ext_off_peak,super_peak))
  
data_life<-data_life %>% left_join(forwards%>%select(month,year,Type,pool_price),by=c("month","year","Type")) %>%
  mutate(price=case_when(
    is.na(pool_price.x) ~ pool_price.y,
    TRUE ~ pool_price.x
  )
  ) %>% arrange(Type,start) %>% mutate(price=na.locf(price)) %>%
  mutate(price=case_when(
    year>2026~ price*1.02^(year-2026),
    TRUE ~ price)
  )%>%
  select(-c(pool_price.x,pool_price.y)) %>% arrange(start)

#build a solar tariff
microgen_tariff<-150

data_life<-data_life %>%
  mutate(solar_tariff=case_when(
    year==2017 ~price,
    year==2018 ~microgen_tariff,
    year>2018~ microgen_tariff*1.02^(year-2018),
    TRUE ~ price)
  ) 




data_life$T_D<-0.00258-.00172+0.00170+0.03080+0.00861
data_life$T_D[(data_life$year>=2018)]<-(0.00358-.00172+0.00321+0.03025+0.00907)*1.02^(data_life$year[(data_life$year>=2018)]-2018)


data_life<-data_life %>% group_by(month,year) %>% 
  mutate(rro_test=sum(price*consumptionEnergy)/sum(consumptionEnergy)) %>%
  ungroup() %>%
  mutate(
  deliveries=pmax(0,consumptionEnergy-generationEnergy),
  net_power=consumptionEnergy-generationEnergy,
  my_bill=net_power*price/1000+deliveries*T_D,
  cons_bill=consumptionEnergy*(price/1000+T_D),
  net_meter_bill=net_power*(price/1000+T_D),
  solar_tariff_bill=(month %in% seq(3,9))*net_power*solar_tariff/1000+deliveries*T_D +(-month %in% seq(3,9))*my_bill,
  saved_td=(consumptionEnergy-deliveries)*T_D
  ) 




billing<-data_life %>% group_by(year,month) %>% summarize(
  price=
  deliveries=sum(deliveries),
  generation=sum(generationEnergy),
  consumption=sum(consumptionEnergy),
  net_power=sum(net_power),
  my_bill=sum(my_bill),
  cons_bill=sum(cons_bill),
  net_meter_bill=sum(net_meter_bill),
  solar_tariff_bill=sum(solar_tariff_bill),
  saved_td=sum(saved_td)
  ) %>% mutate(
    savings_standard=cons_bill-my_bill,
    savings_solar=cons_bill-solar_tariff_bill,
    savings_net_meter=cons_bill-net_meter_bill,
    savings_nerd_standard=savings_standard+days_in_month(month)*2.50,
    savings_nerd_solar=savings_solar+days_in_month(month)*2.50,
    )


#calculate NPV for a given rate of discount
npv_x <- function(x,ts) {
  sum(ts/(1+x)^seq(0,NROW(ts)-1))
}

# a solution is c(1,1)
irr<-function(ts,freq="annual"){
  xstart <- .005
  ret<-nleqslv(xstart, npv_x, jac=NULL, ts)
  ifelse(freq=="monthly",(1+ret$x)^12-1,ret$x)
}

billing$savings_solar[1]<-billing$savings_solar[1]-13000
irr(billing$savings_solar,freq="monthly")*100

billing$savings_standard[1]<-billing$savings_standard[1]-13000
irr(billing$savings_standard,freq="monthly")*100

billing$savings_nerd_standard[1]<-billing$savings_nerd_standard[1]-13000
irr(billing$savings_nerd_standard,freq="monthly")*100

billing$savings_nerd_solar[1]<-billing$savings_nerd_solar[1]-13000
irr(billing$savings_nerd_solar,freq="monthly")*100

billing$saved_td[1]<-billing$saved_td[1]-13000
irr(billing$saved_td,freq="monthly")*100


rate_history <- read.csv(file="Rates.csv",stringsAsFactors = F)
rate_history$Effective.Start.Date..mm.dd.yyyy.<-mdy_hm(rate_history$Effective.Start.Date..mm.dd.yyyy.)

rro_history<-rate_history %>% filter(grepl("Regulated Rate Option",rate_history$Product)|grepl("RRO",rate_history$Product)|grepl("rro",rate_history$Product),grepl("Edmonton",rate_history$Product),Customer.Class=="Home",Service.Area=="EPCOR")

ggplot(rro_history)+geom_line(aes(Effective.Start.Date..mm.dd.yyyy.,Electricity...kWh))




#load forecast data
load("../alberta_power/forecast_data.Rdata")

forecast_data$forecast_ail<- gsub("\\,", "", forecast_data$forecast_ail)
forecast_data$actual_ail<- gsub("\\,", "", forecast_data$actual_ail)
forecast_data<-forecast_data %>% mutate_at(names(forecast_data)[2:6],as.numeric)
forecast_data<-forecast_data %>% filter(!is.na(actual_posted_pool_price))

#make an he in the sys_data same as the time in forecast data
sys_data$he_full<-sys_data$start-minutes(minute(sys_data$start))+hours(1)
test_data<-sys_data %>% left_join(forecast_data%>%select(time,actual_posted_pool_price,actual_ail), by=c("he_full"="time"))
#test_data<-merge(sys_data,forecast_data,by.x =c("date","he_full"),by.y=c("date","he_full"),all.x = T)


#Transmission 2017 $0.03080
#Transmission 2018 $0.03025
#Transmission 2019 $0.03132
#Transmission 2020 $0.03536
#Transmission 2021 $0.03610
#Transmission 2022 $0.03462
#Transmission 2023 $0.03834

#Distribution 2017 $0.00861
#Distribution 2018 $0.00907
#Distribution 2019 $0.00951
#Distribution 2020 $0.00993
#Distribution 2021 $0.01036
#Distribution 2022 $0.01043
#Distribution 2023 $0.01643

#RIDER DJ
#2019 0.276 c/kWh
#2020 0.240 c/kWh
#done

#Rider L 2021
#2021 $0.00043/kwh
#2022 $0.00035/kwh
# done

#2017 Rider K 0.258 ?/kWh
#2018 Rider K 0.358 ?/kWh
#2019 Rider K 0.203 ?/kWh
#2020 Rider K 0.073 ¢/kWh
#2021 Rider K 0.00331 $/kWh
#2022 Rider K 0.614 ¢/kWh
#2023 Rider K 1.395 ¢/kWh

#2018 Rider J -0.172 c/kWh
#2019 Rider J +0.01 c/kWh
#2020 Rider J: $0.00168 /kWh.
#2021 Rider J: $0.00110 /kWh
#2022 Rider J: $0.00110 /kWh
#2023 Rider J: $0.00057 /kWh


#Rider G Balancing Pool

#2023 Rider G $0.00228 /kWh.
#2022 Rider G $0.00228 /kWh.
#2021 Rider G $0.00238/kWh.
#2020 Rider G $0.00259
#2019 Rider G $0.003/kWh
#2018 Rider G $0.00321/kWh
#2017 Rider G $0.00170/kWh

test_data$T_D<-0.00258-.00172+0.00170+0.03080+0.00861
test_data$T_D[(year(test_data$he_full)==2018)]<-0.00358-.00172+0.00321+0.03025+0.00907
test_data$T_D[(year(test_data$he_full)==2019)]<-0.00203+0.0001+0.003+0.03132+0.00951+0.00276
test_data$T_D[(year(test_data$he_full)==2020)]<-0.00073+0.00168+0.00259+0.03536+0.00993+0.00240
test_data$T_D[(year(test_data$he_full)==2021)]<-0.00331+0.00110+0.00238+0.03610+0.01036+0.00240
test_data$T_D[(year(test_data$he_full)==2022)]<-0.00614+0.00110+0.00228+0.03462+0.01043+0.00240
test_data$T_D[(year(test_data$he_full)==2023)]<-0.00614+0.00110+0.00228+0.03834+0.01643+0.00240


test_data %>% filter(month==6,year==2018)%>% summarize(cons=sum(consumptionEnergy/12))

test_data %>% filter(month==6,year==2018)%>% summarize(cons=sum(importedEnergy/12))


#load RRO prices

rro_data <- read.xlsx(xlsxFile = "RRO_hist.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
rro_data$`Effective.Start.Date.(mm/dd/yyyy)`<-as.Date(rro_data$`Effective.Start.Date.(mm/dd/yyyy)`,origin = "1899-12-30")
rro_data$`Effective.End.Date(mm/dd/yyyy)`<-as.Date(rro_data$`Effective.End.Date(mm/dd/yyyy)`,origin = "1899-12-30")
rro_data<-select(rro_data,c(4,7,9))
rro_data$month<-month(rro_data$`Effective.Start.Date.(mm/dd/yyyy)`)
rro_data$year<-year(rro_data$`Effective.Start.Date.(mm/dd/yyyy)`)
rro_data$RRO<-as.numeric(rro_data$RRO)
rro_data$My_price <-as.numeric(rro_data$My_price)


rro_data <- read.xlsx(xlsxFile = "RRO_hist.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
rro_data$`Effective.Start.Date.(mm/dd/yyyy)`<-as.Date(rro_data$`Effective.Start.Date.(mm/dd/yyyy)`,origin = "1899-12-30")
rro_data$`Effective.End.Date(mm/dd/yyyy)`<-as.Date(rro_data$`Effective.End.Date(mm/dd/yyyy)`,origin = "1899-12-30")
rro_data<-select(rro_data,c(4,7,9))

rro_data<-rro_data %>% clean_names() %>%
  rename(date=effective_start_date_mm_dd_yyyy)%>%
  mutate(month=month(date),
         year=year(date),
         rro=as.numeric(rro),
         my_price=as.numeric(my_price))

rro_data<-rro_data %>% mutate(
  transmission=case_when(
    #Transmission 
    (year==2017)~ 0.03080,
    (year== 2018)~ 0.03025,
    (year== 2019)~ 0.03132,
    (year== 2020)~ 0.03536,
    (year== 2021)~ 0.03610,
    (year== 2022)~ 0.03462,
    (year== 2023)~ 0.03834,
  ),
  distribution_var=case_when(
    (year==2017)~0.00861,
    (year==2018)~0.00907,
    (year==2019)~0.00951,
    (year==2020)~0.00993,
    (year==2021)~0.01036,
    (year==2022)~0.01043,
    (year==2023)~0.01643,
  ),
  
  rider_dj=case_when(
    #RIDER DJ
    (year==2017)~ - 0.00289,
    (year==2018)~ 0.00276,
  (year==2019)~ 0.00276,
  (year==2020)~ 0.00240,
  TRUE~0
),
    #Rider L 2021
    rider_l=case_when(
    (year==2021)~ 0.00043,
    (year==2022)~ 0.00035,
    TRUE~0
    ),
    
#Rider K
rider_k=case_when(
  (year==2017)~ 0.258/100,
  (year==2018)~ 0.358 /100,
  (year==2019)~ 0.203 /100,
  (year==2020)~ 0.073/100,
  (year==2021)~ 0.00331/100,
  (year==2022)~ 0.614/100,
  (year==2023)~ 1.395/100,
  TRUE~0
  ),

# rider J
rider_j=case_when(
  (year==2017)~0.00216,
  (year==2018)~ -0.00172,
  (year==2019)~ -0.0001,
  (year==2020)~ 0.00168,
  (year==2021)~ 0.00110,
  (year==2022)~ -0.00125,
  (year==2023) ~ 0.00057,
  TRUE~0
  ),
    #Rider G Balancing Pool checked
rider_g=case_when(
  (year==2017) ~ 0.00114,
  (year==2018) ~ 0.00321,
  (year==2019) ~ 0.003,
  (year==2020) ~ 0.00259,
  (year==2021) ~ 0.00238,
  (year==2022) ~ 0.00228,
  (year==2023) ~ 0.00228,
  TRUE~0
),
#local access fee
laf=case_when(
  (year==2017) ~ 0.0084,
  (year==2018) ~ 0.0081,
  (year==2019) ~ 0.0085,
  (year==2020) ~ 0.0090,
  (year==2021) ~ 0.0097,
  (year==2022) ~ 0.0099,
  (year==2023) ~ 0.0105,
  TRUE~0
),
dist_daily=case_when(
    (year==2017) ~ 0.5907,
    (year==2018) ~ 0.6210,
    (year==2019) ~ 0.6521,
    (year==2020) ~ 0.68128,
    (year==2021) ~ 0.71057,
    (year==2022) ~ 0.71445,
    (year==2023) ~ 0.66988,
    TRUE~0
  ),
bill_fixed=dist_daily*days_in_month(date),
bill_var=transmission+distribution_var+rider_dj+rider_g+rider_l+rider_k+rider_j+laf
)
         
write_csv(rro_data,file = "billing_info.csv")         
  
  



test_data<-merge(test_data,rro_data,by =c("year","month"),all.x = TRUE)
names(test_data)[grep("RRO_price",names(test_data))]<-"RRO"



test_data<-test_data[!is.na(test_data$actual_posted_pool_price),]

test_data$value=ifelse(test_data$net_to_grid<=0,-1*test_data$net_to_grid*test_data$RRO,test_data$net_to_grid*(test_data$RRO+test_data$T_D))
test_data$imp_price=ifelse(test_data$net_to_grid<=0,test_data$RRO,(test_data$RRO+test_data$T_D))

#bill with raw RRO Pricing
test_data$RRO_bill=ifelse(test_data$net_to_grid<=0,test_data$net_to_grid*test_data$RRO,test_data$net_to_grid*(test_data$RRO+test_data$T_D))
#bill at pool prices
test_data$pool_pricing=ifelse(test_data$net_to_grid<=0,test_data$net_to_grid*test_data$actual_posted_pool_price/1000,test_data$net_to_grid*(test_data$actual_posted_pool_price/1000+test_data$T_D))
#bill at my prices
test_data$my_bill=ifelse(test_data$net_to_grid<=0,test_data$net_to_grid*test_data$My_price,test_data$net_to_grid*(test_data$My_price+test_data$T_D))
#Consumption at RRO
test_data$my_cons_bill=test_data$consumptionEnergy*(test_data$RRO+test_data$T_D)
#Consumption at pool
test_data$pool_cons_bill=test_data$consumptionEnergy*(test_data$actual_posted_pool_price/1000+test_data$T_D)
#consumption at my prices
test_data$my_contract_bill=test_data$consumptionEnergy*(test_data$My_price+test_data$T_D)

sum(test_data$my_bill,na.rm = T)
sum(test_data$RRO_bill,na.rm = T)
sum(test_data$pool_pricing,na.rm = T)
sum(test_data$my_cons_bill,na.rm = T)
sum(test_data$pool_cons_bill,na.rm = T)

cons_weight_price=sum(test_data$consumptionEnergy*(test_data$actual_posted_pool_price/1000+test_data$T_D))/sum(test_data$consumptionEnergy)

#avg monthly
my_monthly<-sum(test_data$my_bill,na.rm = T)/NROW(unique(test_data$date))*365
monthly_RRO<-sum(test_data$RRO_bill,na.rm = T)/NROW(unique(test_data$date))*365
cons_RRO<-sum(test_data$my_cons_bill,na.rm = T)/NROW(unique(test_data$date))*365


#cons_weight_price
mean(test_data$T_D)
mean(test_data$actual_posted_pool_price/1000)




test_data$new_price<-.09
test_data$new_td<-.043
test_data$test_bill=ifelse(test_data$net_to_grid<=0,test_data$net_to_grid/12*test_data$new_price,test_data$net_to_grid/12*(test_data$new_price+test_data$new_td))
test_data$test_cons_bill=test_data$consumptionEnergy/12*(test_data$new_price+test_data$new_td)
w_solar<-sum(test_data$test_bill,na.rm = T)
wo_solar<-sum(test_data$test_cons,na.rm = T)


paste("With prices averaging $",mean(test_data$new_price),"/kWh and transmission and distribution at $",mean(test_data$new_td),"/kWh. Bill without solar would be $",round(wo_solar,2)," and with solar would be $",round(w_solar,2)," for savings of $",round(wo_solar-w_solar,2)," or $",round((wo_solar-w_solar)/NROW(unique(test_data$date)),2)," per day.",sep = "")



paired_tableau<-paste(t(cbind(colors_tableau10(),colors_tableau10_light())))

df1<-test_data %>%
  #filter(start<ymd(paste(year(max(start)),month(max(start)),1,sep = "-"))) %>%
  filter(start>=ymd(paste(year(min(start)),month(min(start))+1,1,sep = "-"))) %>%
  group_by(month,year) %>% summarize(RRO_bill=sum(RRO_bill),pool_prices=sum(pool_pricing),
                                     consumption_RRO=sum(my_cons_bill),
                                     consumption_pool_price=sum(pool_cons_bill),
                                     my_cons_bill=sum(my_contract_bill),
                                     net=sum(net_to_grid),my_bill=sum(my_bill)) %>%
  mutate(date=ymd(paste(year,month,15,sep="-")),
         savings=consumption_RRO-my_bill) %>% ungroup()

df1<-melt(df1,id=c("date","savings"),measure.vars = c("my_bill","my_cons_bill","RRO_bill","consumption_RRO","pool_prices","consumption_pool_price"),
                    value.name = "bill",variable.name = "contract")  %>%
     mutate(contract=factor(contract,labels = c("My Contract Prices w Solar","My Contract Prices w/o Solar","RRO Prices w Solar","RRO Prices w/o Solar",
                               "Pool Prices w Solar","Pool Prices w/o Solar")),
            contract=fct_relevel(contract, "My Contract Prices w Solar", after = Inf),
            contract=fct_relevel(contract, "My Contract Prices w/o Solar", after = Inf))

ggplot(subset(test_data,as.Date(start)>=as.Date("2017-09-1")))+
  geom_line(aes(start,T_D,colour="Transmission Charges"),size=2)



png<-1
if(png==1)
  set_png("solar_vcosts.png")
ggplot(df1)+
  #geom_line(aes(date,net,colour="Net"),size=2)+
  
  geom_line(aes(date,bill,group=contract,colour=contract),size=rel(1.25))+
  #geom_line(aes(date,consumption_RRO,colour="RRO Prices w/o Solar"),size=2)+
  #geom_line(aes(date,RRO_bill,colour="RRO Prices w Solar"),size=2)+
  #geom_line(aes(date,pool_prices,colour="Hourly Pool Prices w Solar"),size=2)+
  #geom_line(aes(date,consumption_pool_price,colour="Hourly Pool Prices w/o Solar"),size=2)+
  scale_x_date(date_breaks = "3 months",date_labels =  "%b\n%Y",expand=c(0,0))+
  scale_y_continuous(breaks=pretty_breaks())+
  scale_color_manual("",values=c(paired_tableau[1:4],"black","grey50"))+
  guides(colour = guide_legend(nrow = 2,byrow=FALSE))+
theme_minimal()+theme(    
  legend.position = "bottom",
  legend.margin=margin(c(0,0,0,0),unit="cm"),
  plot.margin=margin(c(1,1,1,1),unit="cm"),
  legend.text = element_text(colour="black", size = 12, face = "bold"),
  plot.caption = element_text(size = 10, face = "italic",hjust = .001),
  plot.title = element_text(face = "bold"),
  plot.subtitle = element_text(size = 12, face = "italic"),
  panel.grid.minor = element_blank(),
  text = element_text(size = 10,face = "bold"),
  axis.text = element_text(size = 10,face = "bold", colour="black")
)+    labs(y="Monthly Variable Costs ($)",x="\nDate",
           title="Variable costs of household electricity, with and without solar",
           subtitle="Includes energy charges, variable portion of transmission and distribution charges, as well as variable rate riders",
           caption=paste("Average monthly savings from solar and contract prices for the period shown vs consumption billed at regulated (RRO) rates: ", sprintf("$%.2f",mean(df1$savings[df1$contract=="My Contract Prices w Solar"])),"\nSources: Household power data via Neurio API, rate riders from EPCOR, RRO rates from Alberta's Utilities Consumer Advocate.\nCalculations and graph by Andrew Leach. RRO= Regulated Rate Option prices.",sep=""))
if(png==1)
 dev.off()


png<-1
if(png==1)
  set_png("solar_vcosts_contract.png")
ggplot(filter(df1,contract %in% c("My Contract Prices w Solar","RRO Prices w/o Solar")))+
  geom_line(aes(date,bill,group=contract,colour=contract),size=rel(1.25))+
  #geom_line(aes(date,consumption_RRO,colour="RRO Prices w/o Solar"),size=2)+
  #geom_line(aes(date,RRO_bill,colour="RRO Prices w Solar"),size=2)+
  #geom_line(aes(date,pool_prices,colour="Hourly Pool Prices w Solar"),size=2)+
  #geom_line(aes(date,consumption_pool_price,colour="Hourly Pool Prices w/o Solar"),size=2)+
  scale_x_date(date_breaks = "3 months",date_labels =  "%b\n%Y",expand=c(0,0))+
  scale_y_continuous(breaks=pretty_breaks())+
  scale_color_manual("",values=c(paired_tableau[1:4],"black","grey50"))+
  guides(colour = guide_legend(ncol = 4,byrow=FALSE))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    plot.margin=margin(c(1,1,1,1),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    plot.caption = element_text(size = 10, face = "italic",hjust = .001),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 10,face = "bold"),
    axis.text = element_text(size = 10,face = "bold", colour="black")
  )+    labs(y="Monthly Variable Costs ($)",x="",
             title="Variable costs of household electricity, with and without solar",
             subtitle="Includes energy charges, variable portion of transmission and distribution charges, as well as variable rate riders",
             caption=paste("Average monthly savings from solar and contract prices for the period shown vs consumption billed at regulated (RRO) rates: ", sprintf("$%.2f",mean(df1$savings[df1$contract=="My Contract Prices w Solar"])),"\nSources: Household power data via Neurio API, rate riders from EPCOR, RRO rates from Alberta's Utilities Consumer Advocate. Graph by Andrew Leach.",sep=""))
if(png==1)
  dev.off()




npv<-function(init= 13230,m_ret,rate=.1,years=25){
  m_rate<-(1+rate)^(1/12)-1
  -init+m_ret/m_rate-(m_ret/m_rate/(1+m_rate)^(years*12))
}

npv(init = 13230,m_ret=69.62,rate=.0251,years=25)


solar_irr<-function(rate_sent,m_ret_sent){
  npv(init = 13230,m_ret=m_ret_sent,rate=rate_sent,years=25)
}

solve(solar_irr())
uniroot(solar_irr, c(.0001,.5), tol = 0.000001,m_ret=69.62)

npv(rate = .0404,m_ret = 69.62,init = 13230)

png<-1
if(png==1)
  set_png("solar_vcosts_basic.png")
ggplot(filter(df1,contract %in% c("RRO Prices w Solar","RRO Prices w/o Solar")))+
  #geom_line(aes(date,net,colour="Net"),size=2)+
  geom_line(aes(date,bill,group=contract,colour=contract),size=2)+
  #geom_line(aes(date,consumption_RRO,colour="RRO Prices w/o Solar"),size=2)+
  #geom_line(aes(date,RRO_bill,colour="RRO Prices w Solar"),size=2)+
  #geom_line(aes(date,pool_prices,colour="Hourly Pool Prices w Solar"),size=2)+
  #geom_line(aes(date,consumption_pool_price,colour="Hourly Pool Prices w/o Solar"),size=2)+
  scale_x_date(date_breaks = "2 months",date_labels =  "%b\n%Y")+
  scale_color_manual("",values=c(paired_tableau[1:4],"black"))+
  guides(colour = guide_legend(nrow = 1,byrow=FALSE))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    plot.caption = element_text(size = 12, face = "italic",hjust = .001),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+    labs(y="Monthly Variable Costs ($)",x="\nDate",
             title="Household energy variable costs, with and without solar",
             subtitle="Includes energy charges, variable portion of transmission and distribution, as well as rate riders",
             caption=paste("Source: Household power data via Neurio API, graph by Andrew Leach. RRO= Regulated Rate Option.",sep=""))
if(png==1)
  dev.off()

png<-1
if(png==1)
  set_png("solar_vcosts_my_bill.png")
ggplot(filter(df1,contract %in% c("RRO Prices w Solar","RRO Prices w/o Solar","My Bill")))+
  #geom_line(aes(date,net,colour="Net"),size=2)+
  geom_line(aes(date,bill,group=contract,colour=contract),size=2)+
  #geom_line(aes(date,consumption_RRO,colour="RRO Prices w/o Solar"),size=2)+
  #geom_line(aes(date,RRO_bill,colour="RRO Prices w Solar"),size=2)+
  #geom_line(aes(date,pool_prices,colour="Hourly Pool Prices w Solar"),size=2)+
  #geom_line(aes(date,consumption_pool_price,colour="Hourly Pool Prices w/o Solar"),size=2)+
  scale_x_date(date_breaks = "2 months",date_labels =  "%b\n%Y")+
  scale_color_manual("",values=c(paired_tableau[1:2],"black"))+
  guides(colour = guide_legend(nrow = 1,byrow=FALSE))+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 12, face = "bold"),
    plot.caption = element_text(size = 12, face = "italic",hjust = .001),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+    labs(y="Monthly Variable Costs ($)",x="\nDate",
             title="Household energy variable costs, with and without solar",
             subtitle="Includes energy charges, variable portion of transmission and distribution, as well as rate riders",
             caption=paste("Source: Household power data via Neurio API, graph by Andrew Leach\nTotal savings from solar for period shown vs RRO priced consumption: $",round(sum(df1$savings[df1$contract=="My Bill"]),2),sep=""))
if(png==1)
  dev.off()




breaks<- as.POSIXct(seq.POSIXt(as.POSIXct("2017-06-1")+hours(12) ,as.POSIXct("2018-04-7")-hours(1), by = "1 day") )

png<-0
if(png==1)
  set_png("solar_price_signal.png")
ggplot(subset(test_data,as.Date(start)>=as.Date("2018-04-1")+hours(1) & as.Date(start)<=as.Date("2018-04-7")-hours(1)))+
  geom_line(aes(he_full,imp_price*100,colour="Effective Marginal Cost"),size=2)+
  geom_line(aes(he_full,actual_posted_pool_price/1000*100,colour="Market Price"),size=2)+
  scale_color_brewer("",labels = c("Marginal Opportunity Cost of Power","AESO Pool Price"),palette = "Set1")+
  #scale_color_viridis("",labels = c("Battery\nCharge","Consumption","Net Electricity\nPurchases","Solar\nGeneration"),discrete = TRUE,option="C")+
  scale_x_datetime(labels = date_format("%a\n%b %d\n%H:%M",tz="America/Denver"),breaks = breaks)+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Price or Opportunity Cost of Power (c/kWh)",x="\nTime",
             title="Household Energy Marginal Price vs System Marginal Price",
             caption="Source: Household power data via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


breaks<- as.POSIXct(seq.POSIXt(as.POSIXct("2018-05-10")+hours(12) ,as.POSIXct("2018-05-17")-hours(1), by = "1 day") )

png<-1
if(png==1)
  set_png("solar_price_signal_May.png")
ggplot(subset(test_data,as.Date(start)>=as.Date("2018-05-10")+hours(1) & as.Date(start)<=as.Date("2018-05-17")-hours(1)))+
  geom_line(aes(he_full,imp_price*100,colour="Effective Marginal Cost"),size=2)+
  geom_line(aes(he_full,actual_posted_pool_price/1000*100,colour="Market Price"),size=2)+
  scale_color_brewer("",labels = c("Marginal Opportunity Cost of Power","AESO Pool Price"),palette = "Set1")+
  #scale_color_viridis("",labels = c("Battery\nCharge","Consumption","Net Electricity\nPurchases","Solar\nGeneration"),discrete = TRUE,option="C")+
  scale_x_datetime(labels = date_format("%a\n%b %d\n%H:%M",tz="America/Denver"),breaks = breaks)+
  theme_minimal()+theme(    
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 14,face = "bold", colour="black")
  )+    labs(y="Price or Opportunity Cost of Power (c/kWh)",x="\nTime",
             title="Household Energy Marginal Price vs System Marginal Price",
             caption="Source: Household power data via Neurio API, pool price data via AESO\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()






#fix data hole from router error

fix_data<-function(){
  
load("solar_data.RData")
#neurio data router error from 2018-09-24 to 2018-10-09
max_date<-ymd("2018-10-09")
min_date<-ymd("2018-09-23")
df_test<-filter(sys_data,start<max_date,start>min_date) %>% assign_date_time_days(time_var ="start") 
# check - you can see where the holes are here
obs<-df_test %>% group_by(date,hour) %>% summarize(obs=n())

#we're going to use A/P systems data for generation, and previous year consumption data averages by month and day to correct it

#get previous year data for september and october
df_sub<-filter(sys_data,start<=ymd("2017-10-31"),start>=ymd("2017-09-1")) %>% assign_date_time_days(time_var ="start") %>%
  mutate(minute=minute(start))%>%
  group_by(month,hour,minute,wday)%>% summarize(consumptionEnergy=mean(consumptionEnergy))

#ggplot(df_sub)+
#  geom_line(aes(hour,cons,group=wday,colour=wday))+
#  facet_grid(~month)

#get the A/P systems data
#file names are Power Curve for 204000006111 in 2018-10-11.xls

A_P_name<-function(date_sent){
  paste("Power Curve for 204000006111 in ",format(date_sent, "%Y-%m-%d"),".xls",sep="")
}

data_store <- list()
counter<-1
dates<-seq.Date(min_date,max_date,by="1 day")
for(date_call in dates){
  print(as.Date(date_call))
  file_name<-A_P_name(as.Date(date_call))
  ap_data<-read_excel(file_name)
  #take out the commas
  ap_data$`Power(kW)`<-as.numeric(gsub(",", "",ap_data$`Power(W)`))/1000
  ap_data$date<-as.Date(date_call)
  data_store[[counter]]<-ap_data
  counter<-counter+1
}
ap_data<-do.call(rbind,data_store)

#align ap_data time_stamps with neurio time stamps 
ap_data$start<-ymd_hm(paste(ap_data$date,ap_data$time),tz="America/Denver")
#find closest 5 minutes
ap_data$start<-ap_data$start-minutes(mod(minute(ap_data$start),5))

ap_data$end<-ap_data$start+minutes(5)

#ap_data <-ap_data %>% left_join(df_test, by = c("start"))

new_data<-data.frame(
  seq.POSIXt(from = ymd_hms("2018-09-23 0:00:00",tz="America/Denver"),to=ymd_hms("2018-10-09 23:55:00",tz="America/Denver"),by="5 min"),
  stringsAsFactors = F)
names(new_data)[1]<-"start"
new_data$end<-new_data$start+minutes(5)

ap_data <-new_data %>% left_join(ap_data, by = c("start","end"))
ap_data$generationEnergy<-ap_data$`Power(kW)`
ap_data$generationEnergy[is.na(ap_data$generationEnergy)]<-0
ap_data<-ap_data %>% assign_date_time_days(time_var ="start") %>%
  mutate(minute=minute(start))

ap_data<-ap_data %>% left_join(df_sub,by = c("month", "wday", "hour", "minute"))
ap_data<-ap_data %>% 
  mutate(importedEnergy=pmax(consumptionEnergy-generationEnergy,0),
         exportedEnergy=pmax(generationEnergy-consumptionEnergy,0),
         net_to_grid=importedEnergy-exportedEnergy,
         year=year(start),month=month(start),hour=hour(start),he=hour(start)+1,day=day(start),date=date(start)) %>%
  select("importedEnergy","exportedEnergy","start","end",              
       "consumptionEnergy","generationEnergy","net_to_grid",       "year",             
       "month","hour","he","day","date")
#names in sys_data
#[1] "importedEnergy"    "exportedEnergy"    "start"             "end"              
#[5] "consumptionEnergy" "generationEnergy"  "net_to_grid"       "year"             
#[9] "month"             "hour"              "he"                "day"              
#[13] "date"             
 
#now replace it

sys_data<-sys_data %>% filter(!(start %in% ap_data$start)) %>% rbind(ap_data) %>% arrange(start)
save(sys_data, file= "solar_data.RData")
}
#fix_data()






