#CAISO Grid Status.io

source("power_paper_base.R")


# df = client.get_dataset(
#   dataset="caiso_standardized_5_min",
#   start=None,  # start from beginning of data
#   end=None,  # end at most recent data
#   tz="US/Pacific",  # return time stamps in Pacific time
# )

get_gsio<-function(dataset,start=NA,end=NA,tz="UTC",key_sent=grid_status_key)
{
  #start<-Sys.time()-days(5)
  #end<-Sys.time()-days(3)
  #dataset<-"caiso_standardized_5_min"
  #key_sent=grid_status_key
  if(!is.na(start))
      start<-paste(format(with_tz(as.POSIXct(start),tz = "UTC"),"%Y-%m-%dT%H:%M:%SZ"),sep="")
  if(!is.na(end))
    end<-paste(format(with_tz(as.POSIXct(end),tz = "UTC"),"%Y-%m-%dT%H:%M:%SZ"),sep="")
  api_url = paste("https://api.gridstatus.io/v1/datasets/",dataset,"/query?","start_time=",start,"&end_time=",end,"&api_key=",key_sent,sep="")
  
  
  if(is.na(start))
    api_url = paste("https://api.gridstatus.io/v1/datasets/",dataset,"/query?","&end_time=",end,"&api_key=",key_sent,sep="")
  if(is.na(end))
    api_url = paste("https://api.gridstatus.io/v1/datasets/",dataset,"/query?","&start_time=",start,"&api_key=",key_sent,sep="")
  if(is.na(start)&is.na(end))
    api_url = paste("https://api.gridstatus.io/v1/datasets/",dataset,"/query?","&api_key=",key_sent,sep="")
  
  data<-httr::VERB(verb = "GET", url = api_url, 
                   httr::add_headers(
                   #return_format = "csv",
                   #api_key = key_sent)
                     ))
  
  
  
  raise <- content(data, as="text")
  #convert to list using RJSONIO
  new<-fromJSON(raise)
  tibble(new$data)
}


df_list<-list()
i<-1
for(y_index in seq(2019,2023))
  {
  #y_index<-2023
  df_list[[i]]<-
    get_gsio(dataset="caiso_standardized_5_min",start=ymd_hm(paste(y_index,"-4-01 0:00",sep=""),tz="America/Los_Angeles"),
                         end=ymd_hm(paste(y_index,"-04-30 11:59",sep=""),tz="America/Los_Angeles"))
  i<-i+1
  df_list[[i]]<-
    get_gsio(dataset="caiso_standardized_5_min",start=ymd_hm(paste(y_index,"-1-01 0:00",sep=""),tz="America/Los_Angeles"),
             end=ymd_hm(paste(y_index,"-01-31 11:59",sep=""),tz="America/Los_Angeles"))
  i<-i+1
  df_list[[i]]<-get_gsio(dataset="caiso_standardized_5_min",start=ymd_hm(paste(y_index,"-10-01 0:00",sep=""),tz="America/Los_Angeles"),
                         end=ymd_hm(paste(y_index,"-10-31 11:59",sep=""),tz="America/Los_Angeles"))
  i<-i+1
  df_list[[i]]<-get_gsio(dataset="caiso_standardized_5_min",start=ymd_hm(paste(y_index,"-7-01 0:00",sep=""),tz="America/Los_Angeles"),
                         end=ymd_hm(paste(y_index,"-07-31 11:59",sep=""),tz="America/Los_Angeles"))
  i<-i+1
}

#test<-get_gsio(dataset="caiso_standardized_5_min",start=ymd_hm(paste("2023-5-01 0:00",sep=""),tz="America/Los_Angeles"),
#         end=ymd_hm(paste("2023-05-31 11:59",sep=""),tz="America/Los_Angeles"))

df_list[[i]]<-get_gsio(dataset="caiso_standardized_5_min",start=ymd_hm(paste(2024,"-3-01 0:00",sep=""),tz="America/Los_Angeles"),
                       end=ymd_hm(paste(2024,"-03-31 11:59",sep=""),tz="America/Los_Angeles"))



cali_data<-df_list %>% bind_rows()

library(hms)
duck<-
  cali_data%>%
  mutate(
  time=ymd_hms(interval_start_utc,tz="America/Los_Angeles"),
  clock=as_hms(time),year=year(time),month=month(time),
  month=factor(month.abb[month],levels=month.abb))%>%
  group_by(year,month,clock)%>%
  summarize(avg_net_load=mean(net_load,na.rm=T),
            min_net_load=min(net_load,na.rm=T),
            max_net_load=max(net_load,na.rm=T),
            )%>%
  #create a placeholder time entry for graphing on the same scale
  mutate(time_temp=ymd_hms(paste("2015-01-01",clock)),
         first=ymd(paste(year,month,"01",sep="-"))
         )


duck_spag<-
  cali_data%>%
  mutate(
    time=ymd_hms(interval_start_utc,tz="America/Los_Angeles"),
    day=date(time),
    clock=as_hms(time),year=year(time),month=month(time),
    month=factor(month.abb[month],levels=month.abb))%>%
    #filter(date(time)==ymd("2023-04-23"))%>%
  #group_by(year,month,clock)%>%
  #summarize(avg_net_load=mean(net_load),
  #          min_net_load=min(net_load),
  #          max_net_load=max(net_load),
  #)%>%
  mutate(time_temp=ymd_hms(paste("2015-01-01",clock)))



ggplot(duck%>%filter(year!=2024))+
  geom_line(aes(time_temp,avg_net_load,group=as.factor(year),color=as.factor(year)))+
  #geom_line(data=duck_min,aes(time_temp,avg_net_load,group=as.factor(year),color=as.factor("2023 (minimum net load day)")))+
  facet_wrap(~month,nrow = 1)+
  scale_y_continuous(expand=c(0,0))+
  expand_limits(y=0)+
  scale_color_manual("",values=colors_ua10())+
  scale_x_datetime(expand=c(0,0),date_labels = "%H",date_breaks = "3 hours")+
  guides(color=guide_legend(nrow = 1))+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 11),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10, r = 1, b = 1, l = 1)),
    #axis.text.x = element_blank(),
    axis.text = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
    axis.title = element_text(size = 14, colour="black"),
    axis.ticks = element_blank(),
    text = element_text(size = 20,family="Times New Roman MS")  )+
  labs(x="Hour",y="Average Net Load (MW)",
       #title="California (CAISO) Monthly Average Load Net of Renewable Supply",
       ##subtitle=paste("Metered natural gas, Neurio-monitored electricity consumpiton, generation, imports, and exports",sep=""),
       #caption=str_wrap("Data via gridstatus.io. Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("ducks.png",width=14.5,height=7,bg="white",dpi=200)



ggplot(duck%>%filter(month=="Apr"|year==2024)%>%mutate(year=paste(month,year)))+
  geom_line(aes(time_temp,avg_net_load,group=as.factor(year),color=as.factor(year)))+
  #geom_line(data=duck_min,aes(time_temp,avg_net_load,group=as.factor(year),color=as.factor("Apr 2023 (minimum net load day)")))+
  #facet_wrap(~month,nrow = 1)+
  scale_y_continuous(expand=c(0,0))+
  expand_limits(y=0)+
  scale_color_manual("",values=colors_ua10())+
  scale_x_datetime(expand=c(0,0),date_labels = "%H",date_breaks = "1 hours")+
  guides(color=guide_legend(nrow = 1))+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 11),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10, r = 1, b = 1, l = 1)),
    #axis.text.x = element_blank(),
    axis.text = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
    axis.title = element_text(size = 14, colour="black"),
    axis.ticks = element_blank(),
    text = element_text(size = 20,family="Times New Roman MS")  )+
  labs(x="Hour",y="Average Net Load (MW)",
       #title="California (CAISO) Monthly April Load Net of Renewable Supply",
       #subtitle=paste("Metered natural gas, Neurio-monitored electricity consumpiton, generation, imports, and exports",sep=""),
       #caption=str_wrap("Data via gridstatus.io. Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("apr_ducks.png",width=14.5,height=7,bg="white",dpi=200)




ggplot(duck%>%filter(first>as_date("2022-05-01"))%>%mutate(index=as_factor(paste(month,year))))+
  geom_line(data=duck_spag%>%filter(day>as_date("2022-05-01"))%>%mutate(index=as_factor(paste(month,year))),
           aes(time_temp,net_load,group=day),color="grey60",linewidth=0.1)+
  geom_line(aes(time_temp,avg_net_load,group=as.factor(year),color="Monthly Average Net Load"),linewidth=1.5)+
  facet_wrap(~index,nrow = 1)+
  scale_y_continuous(expand=c(0,0))+
  expand_limits(y=0)+
  scale_color_manual("",values=blakes_blue)+
  scale_x_datetime(expand=c(0,0),date_labels = "%H",date_breaks = "3 hours")+
  guides(color=guide_legend(nrow=1))+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 11),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10, r = 1, b = 1, l = 1)),
    #axis.text.x = element_blank(),
    axis.text = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
    axis.title = element_text(size = 14, colour="black"),
    axis.ticks = element_blank(),
    #text = element_text(size = 20,family="Times New Roman MS")  
    )+
  labs(x="Hour",y="Net Load (MW)",
       title="California (CAISO) Load Net of Renewable Supply",
       #subtitle=paste("Metered natural gas, Neurio-monitored electricity consumpiton, generation, imports, and exports",sep=""),
       caption=str_wrap("Data via gridstatus.io. Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("images/spag_ducks.png",width=14.5,height=7,bg="white",dpi=200)




ggplot(duck%>%filter(month=="Mar"|month=="Apr")%>%mutate(index=as_factor(paste(month,year))))+
  geom_line(data=duck_spag%>%filter(month=="Mar"|month=="Apr")%>%filter(as_date(interval_start_utc)!=ymd("2021-04-02"))%>%
              mutate(index=as_factor(paste(month,year))),
            aes(time_temp,net_load,group=day),color="grey60",linewidth=0.1)+
  geom_line(aes(time_temp,avg_net_load,group=as.factor(year),color="Monthly Average Net Load"),linewidth=1.5)+
  facet_wrap(~index,nrow = 1)+
  scale_y_continuous(expand=c(0,0))+
  expand_limits(y=0)+
  scale_color_manual("",values=blakes_blue)+
  scale_x_datetime(expand=c(0,0),date_labels = "%H",date_breaks = "3 hours")+
  guides(color=guide_legend(nrow=1))+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 11),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10, r = 1, b = 1, l = 1)),
    #axis.text.x = element_blank(),
    axis.text = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
    axis.title = element_text(size = 14, colour="black"),
    axis.ticks = element_blank(),
    #text = element_text(size = 20,family="Times New Roman MS")  
  )+
  labs(x="Hour",y="Net Load (MW)",
       title="California (CAISO) Load Net of Renewable Supply",
       #subtitle=paste("Metered natural gas, Neurio-monitored electricity consumpiton, generation, imports, and exports",sep=""),
       caption=str_wrap("Data via gridstatus.io. Graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("images/spag_ducks_progression.png",width=14.5,height=7,bg="white",dpi=200)




  #end<-Sys.time()-days(3)
  #dataset<-"caiso_standardized_5_min"
  #key_sent=grid_status_key)





https://api.gridstatus.io/v1/datasets/caiso_standardized_5_min&start_time=2023-06-02 10:03:30&end_time=2023-06-04 10:05:47&api_key=6cb5e350439cdec7f79c2dd592480e95