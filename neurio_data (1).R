#Mac
if(R.version$platform ==  "x86_64-apple-darwin15.6.0")
  setwd("/Users/aleach/Google Drive/Solar")
#PC
if(R.version$platform ==  "x86_64-w64-mingw32")
  setwd("C:/Users/aleach/Google Drive/Solar")
print(getwd())
source("../andrew_base.R")


#curl -i -X POST --data "grant_type=client_credentials&client_id=P8EEG3rvRxyqWd_beUBCJA&client_secret=ki6M5J_hTtiXuMAcHHPmtw" https://api.neur.io/v1/oauth2/token

sensor_id <- "0x0000C47F510354AE"

#
#req <- make_req(straighten())[[1]]

#status check, neurio API
neurio_api <- GET("https://api.neur.io/v1/status")
neurio_r <- fromJSON(rawToChar(neurio_api$content))


#command line for token
#curl -i -X POST -H "Content-Type:application/x-www-form-urlencoded" -d "grant_type=client_credentials&client_id=P8EEG3rvRxyqWd_beUBCJA&client_secret=HWnpmXfqSymeG8Iqjp46Dg" "https://api.neur.io/v1/oauth2/token"

my_client_id <- c("P8EEG3rvRxyqWd_beUBCJA")
my_client_secret <- c("HWnpmXfqSymeG8Iqjp46Dg")


#Simons system
simon_sensor_id<-"0x0000C47F5103486D"
simon_client_id<-"woE3Ank7Ty-zKp_QoT-1Yw"
simon_client_secret<-"w6dldtcOSIS0QShj_ItL6g"

#Peters system
solar_homes_sensor_id<-"0x0000C47F510356F3"
solar_homes_id<-"Kx-lCJ4uQqWkYAc4vI7K4Q"
solar_homes_client_secret<-"E58OQwQxRcWCDvJs0Pje2Q"




#backup
#backup_id<-"Ar99xfgNRdWYGn7ovp-VFw"
#backup_secret<-"g7vmO0uNT32Y6xZIOQdZpQ"
#token_id<- get_token(backup_id,backup_secret)




get_token<-function(client_id,client_secret){
  neurio.data <- data.frame(client_id,client_secret)
  # Use authorisation code to get (temporary) access token
  req <- POST(url = "https://api.neur.io/v1/oauth2/token", encode = "form",
              body = list(
                client_id = neurio.data$client_id,
                client_secret = neurio.data$client_secret,
                redirect_uri = "https://api.neur.io/v1/oauth2/token",
                grant_type = "client_credentials"))
  raise <- content(req, as="text")
  print(raise)
    #convert to list using RJSONIO
  fromJSON(raise) -> token_data
  
  return(token_data$access_token)
  }


#token_id<-"AIOO-2mxvJ42jWrVuwmFKPzHNmcxady7V7gACklOFPGLXm-D1f4hRR9ECcYRD7vIBZyDhHeYtqw6R2gJZ8F1Gh0ysVyu8q05Xf_y4LlJmoU9q21n_0PtE-z7i3NbGFfYlDYMa-6fLw7YX-ogc61JTG3Lpq3M82SUm_EJKIvsfHIB1g7WNMvDW00MV5Qn6rZaPeDqJnsRVf9_qpry0AznNU_bGXLg5XWrjKky_VaRtJo0Zu2k2fRzlgmq7ZkLPTS6K2S6Wqf_E6b_"




#call that works, stored in case I mess up the other ones.
#test<-httr::VERB(verb = "GET", url = "https://api.neur.io/v1/samples/stats?sensorId=0x0000C47F510354AE&start=2017-08-16T10:00:00Z&end=2017-08-17T03:00:00Z&granularity=minutes&frequency=5&timezone=America/Edmonton", 
#                       httr::add_headers(Authorization = "Bearer AIOO-2nAWDW89l_OJHK4wnuOKi_paoTLxKREWXZQuSLpefCSAhfxbeIslCQyQ8VXBiyvfRq3mlXOFLOu8clU31RN5l6B9YayQlP35VV12jtmrfueoWfabUfNtRl4HwNoNqeKxnxnlp7gl4zp-c4scD40OF-yZQgMZvf85Fzk7NXqycVBRC1EX7zjdKqjLyQsHRa2XhRzfgZSIxFMBcjZI-Rr_m6xq3dP3Tufp6rlaEVoViVyp24I3YCu9ndBSdg3fRUtyRN7nKgz"), 
#                       encode = "json")





get_neurio<-function(start_string,end_string,time_unit,time_freq,sensor_id,token_id)
  {
  if(time_unit=="minutes") #don't allow more than 24 hours of 5 minute data
    end_string<-min(as.POSIXct(end_string),as.POSIXct(start_string)+24*60*60)
  if(time_unit=="hours") #don't allow more than 31 days for hourly data
    end_string<-min(as.POSIXct(end_string),as.POSIXct(start_string)+31*24*60*60)
  if(time_unit=="days") #don't allow more than 92 days for daily data
    end_string<-min(as.POSIXct(end_string),as.POSIXct(start_string)+92*24*60*60)
  
  start_time<-paste(format(with_tz(as.POSIXct(start_string),tz = "UTC"),"%Y-%m-%dT%H:%M:%SZ"),sep="")
  end_time<-paste(format(with_tz(as.POSIXct(end_string),tz = "UTC"),"%Y-%m-%dT%H:%M:%SZ"),sep="")
  api_url = paste("https://api.neur.io/v1/samples/stats?","sensorId=",sensor_id,"&start=",start_time,"&end=",end_time,"&granularity=",time_unit,"&frequency=",time_freq,"&timezone=America/Edmonton",sep="")
  
  test<-httr::VERB(verb = "GET", url = api_url, 
                   httr::add_headers(Authorization = paste("Bearer ",token_id,sep="")), 
                   encode = "json")
  
  #test<-httr::VERB(verb = "GET", url = "https://api.neur.io/v1/samples/stats?sensorId=0x0000C47F510354AE&start=2017-08-16T19:35:00Z&end=2017-08-17T05:51:08Z&granularity=minutes&frequency=5&timezone=America/Edmonton", 
  #                 httr::add_headers(Authorization = "Bearer AIOO-2nAWDW89l_OJHK4wnuOKi_paoTLxKREWXZQuSLpefCSAhfxbeIslCQyQ8VXBiyvfRq3mlXOFLOu8clU31RN5l6B9YayQlP35VV12jtmrfueoWfabUfNtRl4HwNoNqeKxnxnlp7gl4zp-c4scD40OF-yZQgMZvf85Fzk7NXqycVBRC1EX7zjdKqjLyQsHRa2XhRzfgZSIxFMBcjZI-Rr_m6xq3dP3Tufp6rlaEVoViVyp24I3YCu9ndBSdg3fRUtyRN7nKgz"), 
  #                 encode = "json")
  #print(paste("Rate limit remaining is ",test$headers$`ratelimit-remaining`))
  raise <- content(test, as="text")
  #convert to list using RJSONIO
  fromJSON(raise) -> new
  testing<<-new
  #convert from watt-seconds to kWh
  if(time_unit=="minutes")
    {
    new$consumptionEnergy=new$consumptionEnergy/1000/60/time_freq
    new$generationEnergy=new$generationEnergy/1000/60/time_freq
    new$importedEnergy=new$importedEnergy/1000/60/time_freq
    new$exportedEnergy=new$exportedEnergy/1000/60/time_freq
    }
  # watt-second * 1kW/1000W * 1 min/60 seconds * 1 hour/60 mins
  if(time_unit=="hours")
    {
    new$consumptionEnergy=new$consumptionEnergy/1000/60/60/time_freq
    new$generationEnergy=new$generationEnergy/1000/60/60/time_freq
    new$importedEnergy=new$importedEnergy/1000/60/60/time_freq
    new$exportedEnergy=new$exportedEnergy/1000/60/60/time_freq
    }
  
  # watt-second * 1kW/1000W * 1 min/60 seconds * 1 hour/60 mins * 1 day/24 hours
  if(time_unit=="days")
    {
    new$consumptionEnergy=new$consumptionEnergy/1000/60/60/24/time_freq
    new$generationEnergy=new$generationEnergy/1000/60/60/24/time_freq
    new$importedEnergy=new$importedEnergy/1000/60/60/24/time_freq
    new$exportedEnergy=new$exportedEnergy/1000/60/60/24/time_freq
    }
  
  new$net_to_grid<-new$importedEnergy-new$exportedEnergy
  new$start<-with_tz(as.POSIXct(new$start,format="%Y-%m-%dT%H:%M:%S.000",tz = "UTC"),"America/Denver")
  new$end<-with_tz(as.POSIXct(new$end,format="%Y-%m-%dT%H:%M:%S.000",tz = "UTC"),"America/Denver")
  return(new)
} #end data routine



sample_neurio<-function(){
  sample_time<-paste(format(with_tz(as.POSIXct(Sys.time())-hours(1),tz = "UTC"),"%Y-%m-%dT%H:%M:%SZ"),sep="")
  api_url = paste("https://api.neur.io/v1/samples/live?","sensorId=",sensor_id,"&start=",sample_time,sep="")
print(api_url)
test<-httr::VERB(verb = "GET", url = api_url, 
                 httr::add_headers(Authorization = paste("Bearer ",token_id,sep="")), 
                 encode = "json")
print(paste("Rate limit remaining is ",test$headers$`ratelimit-remaining`))
raise <- content(test, as="text")
#convert to list using RJSONIO
fromJSON(raise) -> new
testing<<-new
return(testing)
}


make_breaks <- function(strt, hour, interval="day", length.out=31) {
  strt <- as.POSIXlt(strt - 60*60*24)  # start back one day
  strt <- ISOdatetime(strt$year+1900L, strt$mon+1L, strt$mday, hour=hour, min=0, sec=0, tz="America/Denver")
  seq.POSIXt(strt, strt+(1+length.out)*60*60*24, by=interval)
}




#get and stack all available data

update_data <- function(data_sent,token=token_id,sensor=sensor_id) {
  
  time_unit<-"minutes"
  time_freq<-5
  if(missing(data_sent)){
    start_string<-"2017-08-16 13:00:00 MDT"
    #start_string<- Sys.time()-24*60*60
    end_string<-Sys.time()
    #end_string<-"2017-08-20 12:00:00 MDT"
    ##end_string<- Sys.time()
    #get initiatial data
    start_point<-start_string
    end_point<-as.POSIXct(start_string)+hours(24)
    sys_data<-get_neurio(start_point,end_point,time_unit,time_freq,sensor,token)
  }
  if(!missing(data_sent)){
    sys_data<-data_sent}
  start_string<-max(sys_data$end)
  end_string<-Sys.time()
  start_point<-start_string  
  while(start_point<end_string)
  {
    #get data
    end_point<-as.POSIXct(start_point)+hours(24)
    new_data<-get_neurio(start_point,end_point,time_unit,time_freq,sensor,token)
    new_data<-new_data %>% mutate(year=year(start),month=month(start),hour=hour(start),he=hour(start)+1,day=day(start),date=date(start))
    sys_data<-rbind(sys_data,new_data)
    start_point<-end_point
    print(paste("Iteration done until ",end_point,sep = ""))
  }
  
  return(sys_data)  
}

load_and_update<-function(load_file="solar_data.RData"){
  load_file<-"solar_data.RData"
  load(load_file,.GlobalEnv) 
  if(max(sys_data$start)+minutes(10)<Sys.time())
     sys_data<-update_data(sys_data)
  sys_data<-sys_data %>% mutate(year=year(start),month=month(start),hour=hour(start),he=hour(start)+1,day=day(start),date=date(start))
  save(sys_data, file= "solar_data.RData")
  #write.xlsx(sys_data, file = "neurio_5_min.xlsx", colNames = TRUE, borders = "columns") 
}


hourly_data<-function(){
hourly_data<-sys_data %>% group_by(year,month,day,hour) %>% summarize(
            gen=sum(generationEnergy/12),
            net_to_grid=sum(-net_to_grid/12),
            cons=sum(consumptionEnergy/12),
            ) %>% ungroup()
  save(hourly_data, file= "hourly_data.RData")
  write.xlsx(hourly_data, file = "neurio_hourly.xlsx", colNames = TRUE, borders = "columns") 
}
hourly_data()


simon_update<-function(data_sent){
  simon_token_id<- get_token(simon_client_id,simon_client_secret)
  time_unit<-"minutes"
  time_freq<-5
  #load("simon_5min_data.RData",.GlobalEnv) 
  #simon_data<-update_data(simon_data,simon_token_id,simon_sensor_id)
  
  time_unit<-"minutes"
  time_freq<-5
  #load("simon_5min_data.RData",.GlobalEnv) 
  start_string<-max(data_sent$end)
  end_string<-Sys.time()
  start_point<-start_string  
  while(start_point<end_string)
  {
    #get data
    end_point<-as.POSIXct(start_point)+hours(24)
    new_data<-get_neurio(start_point,end_point,time_unit,time_freq,simon_sensor_id,simon_token_id)
    data_sent<-rbind(data_sent,new_data)
    start_point<-end_point
    print(paste("Iteration done until ",end_point,sep = ""))
  }
return(data_sent)
}


#load("simon_5min_data.RData",.GlobalEnv)
#simon_data<-simon_data[simon_data$start<as.POSIXct("2018-01-16"),]
#simon_data<-simon_update(simon_data)
#save(simon_data,file="simon_5min_data.RData")
#write.xlsx(simon_data, file = "simon_neurio_5min.xlsx", colNames = TRUE, borders = "columns") 


solar_homes_update<-function(data_sent){
  #simon_token_id<- get_token(simon_client_id,simon_client_secret)
  solar_homes_token_id<- get_token(solar_homes_id,solar_homes_client_secret)
  #data_sent<-solar_homes_data
  time_unit<-"minutes"
  time_freq<-5
  #load("simon_5min_data.RData",.GlobalEnv) 
  start_string<-max(data_sent$end)
  end_string<-Sys.time()
  start_point<-start_string  
  while(start_point<end_string)
  {
    #get data
    end_point<-as.POSIXct(start_point)+hours(24)
    new_data<-get_neurio(start_point,end_point,time_unit,time_freq,solar_homes_sensor_id,solar_homes_token_id)
    data_sent<-rbind(data_sent,new_data)
    start_point<-end_point
    print(paste("Iteration done until ",end_point,sep = ""))
  }
  #time_unit<-"minutes"
  #time_freq<-5
  #start_string<-as.Date("2018-01-11 0:00:00 MST")
  #end_string<-Sys.time()
  #start_point<-start_string
  #end_point<-as.POSIXct(start_point)+days(1)
  ##test_data<-get_neurio(start_point,end_point,time_unit,time_freq,solar_homes_sensor_id,solar_homes_token_id)
  #solar_homes_data<-test_data
  #solar_homes_data<-solar_homes_data[2,]
  #save(solar_homes_data, file= "solar_homes_5min_data.RData")
  #write.xlsx(solar_homes_data, file = "solar_homes_5min.xlsx", colNames = TRUE, borders = "columns")
  return(data_sent)
}
#load("solar_homes_5min_data.RData",.GlobalEnv)
#solar_homes_data<-solar_homes_update(solar_homes_data)
#save(solar_homes_data, file= "solar_homes_5min_data.RData")
#write.xlsx(solar_homes_data, file = "solar_homes_5min.xlsx", colNames = TRUE, borders = "columns")

solar_homes_graph<-function(){
png<-1
if(png==1)
  set_png("solar_homes_system_shapes.png")
ggplot(data=subset(solar_homes_data,start>Sys.Date()-days(7)), aes(start,consumptionEnergy,colour="Consumption")) +
  geom_line(size=2) +
  geom_line(aes(start,importedEnergy-exportedEnergy,colour="Power\nDeliveries"),size=2)+
  geom_line(aes(start,generationEnergy,colour="Solar\nGeneration"),size=2)+
  scale_color_viridis("",labels = c("Household\nConsumption","Electricity\nNet to (-) or\nfrom (+) Grid", "Solar\nGeneration"),discrete = TRUE)+
  #scale_color_viridis("",labels = c("Battery\nCharge","Consumption","Net Electricity\nPurchases","Solar\nGeneration"),discrete = TRUE,option="C")+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
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
  )+    labs(y="Energy Generation, Consumption, and Purchases (kW)",x="\nTime",
             title="Household Energy Consumption, Generation, and Imports",
             caption="Source: Solar Homes system data via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


png<-1
if(png==1)
  set_png("all_3.png")
ggplot(combined_new, aes(end,generationEnergy.simon,colour="Simon's Generation")) +
  geom_line(size=1,alpha=1) +
  geom_line(aes(end,generationEnergy.andrew,colour="Andrew's Generation"),size=1.7,alpha=1)+
  geom_line(aes(end,generationEnergy,colour="Solar Homes Generation"),size=1.7,alpha=1)+
  #geom_line(aes(start,net_imp,colour="Power/nDeliveries"),size=2)+
  #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
  scale_color_viridis("",labels = c("Andrew's\nGeneration","Simon's\n Generation","Solar Homes\n Generation"),discrete = TRUE,option="C")+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m/%d\n%Hh", tz="America/Denver"))+
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
  )+    labs(y="Power Generation (kWh per hour)",x="\nTime",
             title="Household Solar Generation",
             subtitle="Leach (7.6kW) vs Collier (8.37kW) vs Solar Homes (10.2kW)",
             caption="Source: Generation via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

}



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
    scale_x_datetime(limits = lims,date_breaks="2 months",labels = date_format("%b %d\n%Y", tz="America/Denver"))+
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
  
}

#cumulative_graphs()

compare_simon<-function(){
  load(file= "simon_5min_data.RData")
  if(last(simon_data$end)<Sys.time()-minutes(10)) #if there's room for an update, update
      simon_data<-simon_update(simon_data)
  save(simon_data, file= "simon_5min_data.RData")
  
  
  #token_id<- get_token(my_client_id,my_client_secret)
  #hourly_data()
  #load("hourly_data.RData",.GlobalEnv) 
  
  combined<-merge(simon_data, sys_data, by = c("start","end"),suffixes = c(".simon",".andrew"),all= F) # NA's match
  #sum, but adjust for time frequency of 5 min data
  combined$cum_gen.simon<-cumsum(combined$generationEnergy.simon/12)
  combined$cum_gen.andrew<-cumsum(combined$generationEnergy.andrew/12)
  
  #combined_new<-merge(combined,solar_homes_data, by = c("start","end"),suffixes = c(".combo",".slhms"),all=FALSE) # NA's match
  #combined_new$generationEnergy.simon[combined_new$generationEnergy.simon<=0]<-0
  
  lims <- c(min(combined$start),max(combined$start))
  #breaks<- as.POSIXct(seq.POSIXt(min(combined$start),max(combined$start), by = "1 month") )
  
  breaks<-seq.POSIXt(min(combined$start)+days(1), max(combined$start)-days(1), by="2 month")
  combined$day<-date(combined$end)
  df1<-combined %>% group_by(day)%>% summarise(generationEnergy.andrew=sum(generationEnergy.andrew/12),generationEnergy.simon=sum(generationEnergy.simon/12))
  
  png<-1
  if(png==1)
    set_png(file="simon_vs_andrew.png")
  p<-ggplot(df1, aes(day,generationEnergy.simon,colour="Simon's Generation",fill="Simon's Generation")) +
    geom_col(size=1,alpha=1,position="dodge") +
    geom_col(aes(day,generationEnergy.andrew,colour="Andrew's Generation",fill="Andrew's Generation"),size=1,alpha=0.5,position="dodge")+
    #geom_line(aes(start,net_imp,colour="Power/nDeliveries"),size=2)+
    #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
    scale_fill_viridis("",labels = c("Leach System\nGeneration","Collier System\nGeneration"),discrete = TRUE,option="E")+
    scale_color_viridis("",labels = c("Leach System\nGeneration","Collier System\nGeneration"),discrete = TRUE,option="E")+
    scale_x_date(breaks=as.Date(breaks),labels = date_format("%b %d\n%Y", tz="America/Denver"))+
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
    )+    labs(y="Power Generation (kWh per day)",x="\nTime",
               title="Leach (7.6kW) vs Collier (8.37kW) Household Solar Generation",
               caption=paste("Source: Generation data via Neurio API, graph by Andrew Leach\nAs of ",Sys.Date(),", cumulative generation is ",
                             round(sum(df1$generationEnergy.andrew)),"kWh for Leach and ",
                             round(sum(df1$generationEnergy.simon)),"kWh for Collier"
                             ,sep=""))
  print(p)
  if(png==1)#set these to only turn on if you're making PNG graphs
    dev.off()
  
}




#get a token if you need one
#sys_data<-sys_data[sys_data$start<as.POSIXct("2017-12-15 08:45:00"),]
#save(sys_data, file= "solar_data.RData")
token_id<- get_token(my_client_id,my_client_secret)
load("solar_data.RData",.GlobalEnv) 
#sys_data<-subset(sys_data,sys_data$start<ymd("2018-04-01"))
save(sys_data, file= "solar_data.RData")
load_and_update()
save(sys_data, file= "solar_data.RData")


cumulative_graphs()

compare_simon()



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
#get solar system data
load_and_update()
  
solar_data <- read.xlsx(xlsxFile = "solar_system.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
solar_data$Date<-as.POSIXct(paste(2016,"-",solar_data$Month,"-",solar_data$Day," ",solar_data$Hour+1,":00",sep=""),format='%Y-%m-%d %H:%M')
solar_data$Hour<-solar_data$Hour+1
solar_data<-solar_data %>% ungroup()

#token_id<- get_token(my_client_id,my_client_secret)
#hourly_data()

min_time<-trunc(min(sys_data$start),"days")+ 60*60*24
max_time<-min_time+years(1)

hourly_data<-sys_data %>% filter(start>=min_time & end<=max_time) %>% mutate(start=round_date(start, "1 hour"))
#test_10<-test_10 %>% group_by(start) %>% summarise(Match = sum(Match))
hourly_data<-aggregate(. ~ start, hourly_data, mean)
hourly_data$end<-hourly_data$start+minutes(60)
hourly_data <- hourly_data %>% mutate(hour=hour(start)+1,day=day(start),month=month(start),he=hour+1,date=as_date(start))



test<-merge(hourly_data,solar_data,by.x = c("month","hour","day"),by.y = c("Month","Hour","Day"),all = T)
test$Date_NT<-as.Date(test$start)
test$`750_load_W`<-NULL
names(test)[grep("750_load",names(test))]<-"YEG_750_load"
test$YEG_pred_load<-test$YEG_750_load/750*663.2216
test<-arrange(test,month,day,hour)
df1<-test %>% group_by(month) %>% summarize(Pred_load=sum(YEG_pred_load),Actual_net=sum(net_to_grid,na.rm = T),Actual_load=sum(consumptionEnergy,na.rm = T),NREL=sum(`AC.System.Output.(W)`/1000,na.rm = T),Actual=sum(generationEnergy,na.rm = T)) %>% na.omit() %>% ungroup()
df1$month<-factor(month.abb[df1$month],levels=month.abb)

png<-1
if(png==1)
  set_png(file="actual_gen_vs_nrel.png")
ggplot(df1) +
  geom_col(aes(month,Actual,colour="A",fill="A"),size=.01,alpha=0.95,position="dodge")+
  geom_col(aes(month,NREL,colour="B",fill="B"),size=.01,alpha=0.25,position="dodge") +
  scale_fill_viridis("",labels = c("Actual Generation","NREL Predicted Generation"),discrete = TRUE,option="E",direction = -1)+
  scale_color_viridis("",labels = c("Actual Generation","NREL Predicted Generation"),discrete = TRUE,option="E",,direction = -1)+
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
             title="Leach (7.6kW) vs NREL PVWatts Predicted Generation",
             caption=paste("Source: Generation data via Neurio API, graph by Andrew Leach.\nCurrent to ",Sys.Date(),".",sep=""))
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


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




png<-1
if(png==1)
  set_png(file="net_vs_yeg.png")
ggplot(df1) +
  geom_col(aes(month,Actual_net,colour="A",fill="A"),size=.01,alpha=0.95,position="dodge")+
  geom_col(aes(month,Pred_load,colour="B",fill="B"),size=.01,alpha=0.25,position="dodge") +
  scale_fill_viridis("",labels = c("Actual Net-from-grid","Pro-rated Typical Edmonton Load"),discrete = TRUE,option="E",direction = -1)+
  scale_color_viridis("",labels = c("Actual Net-from-grid","Pro-rated Typical Edmonton Load"),discrete = TRUE,option="E",direction = -1)+
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
  )+    labs(y="Power Deliveries (net kWh per month)",x="",
             title="Leach Household Net-from-Grid vs Typical Edmonton Load Curve",
             caption=paste("Source: Household data via Neurio API, Edmonton data via EPCOR, graph by Andrew Leach.\nLoad is Edmonton residential load pro-rated to equal our annual household consumption.",sep=""))
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



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



hourly_patterns<-function(){
  
df1 <- sys_data %>% mutate(time=strftime(start, format="%H:%M")) %>%
     group_by(year,month,time) %>% summarise(gen=mean(generationEnergy),imps=mean(importedEnergy),cons=mean(consumptionEnergy),net=mean(net_to_grid))
df1$month = factor(month.abb[df1$month], levels=month.abb[])

df1$month_test<-paste(as.character(df1$month),df1$year,sep="-")
df1$month_test = factor(df1$month_test,levels=unique(df1$month_test))


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
  labs(x="",y="Electricity Use (kW)",
       title="Power Use (5 minute data)",
       subtitle=paste("Total daily use: ",round(sum(df1$consumptionEnergy*5/60),2),"kWh. Peak demand: ",round(max(df1$consumptionEnergy*5/60),2),sep=""),
       caption="Source: SolarPeople system data via Neurio API, graph by Andrew Leach")
print(p)
ggsave("power_use_day.png",dpi=300,width=16,height=7,bg="white")



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
data_2018<-sys_data %>% filter(year(start)==2018) %>% assign_date_time_days(time_var ="start") %>% 
  group_by(month,wday,hour) %>% summarize(generationEnergy=mean(generationEnergy),
                                          consumptionEnergy=mean(consumptionEnergy))

#create 25 year file

data_life<-data.frame(
  seq.POSIXt(from = min(sys_data$start)-minutes(35),to=min(sys_data$start)-minutes(35)+years(25),by="1 hour"),
  stringsAsFactors = F)
names(data_life)[1]<-"start"
loss_rate<-0.01 #1% annual degredation
data_life<-data_life%>% assign_date_time_days(time_var ="start") %>% left_join(data_2018,by=c("month","wday","hour")) %>%
  mutate(generationEnergy=generationEnergy*(1-loss_rate)^(year(start)-2017),
         he_time=start+hours(1))
#now build prices


#load forecast data
load("../Alberta Power Data/forecast_data.Rdata")
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

peak_forwards<-proc_forwards("../Alberta Power Data/peak_forwards.csv","PEAK")
off_peaks<-proc_forwards("../Alberta Power Data/off_peak_forwards.csv","OFF_PEAK")
peak_forwards_daily<-proc_forwards("../Alberta Power Data/peak_forwards_daily.csv","PEAK")
off_peaks_daily<-proc_forwards("../Alberta Power Data/off_peak_forwards_daily.csv","OFF_PEAK")

#stack new ones
forwards<-rbind(peak_forwards_daily,peak_forwards,off_peaks,off_peaks_daily)%>%filter(Trade_Date==max(Trade_Date)) %>%
  select(Inst_Date,Settle,Type) %>% assign_date_time_days(time_var ="Inst_Date") %>%
  group_by(month,year,Type) %>% summarize(pool_price=mean(Settle)) 

#set up peak hours in data_life
data_life<-data_life%>% assign_peaks(time_var ="start") %>%
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
load("../Alberta Power Data/forecast_data.Rdata")
forecast_data$day_ahead_forecasted_ail<- gsub("\\,", "", forecast_data$day_ahead_forecasted_ail)
forecast_data$actual_ail<- gsub("\\,", "", forecast_data$actual_ail)
forecast_data<-forecast_data %>% mutate_at(names(forecast_data)[2:6],as.numeric)
forecast_data<-forecast_data %>% filter(!is.na(actual_posted_pool_price))

#make an he in the sys_data same as the time in forecast data
sys_data$he_full<-sys_data$start-minutes(minute(sys_data$start))+hours(1)
test_data<-sys_data %>% left_join(forecast_data%>%select(time,actual_posted_pool_price,actual_ail), by=c("he_full"="time"))
#test_data<-merge(sys_data,forecast_data,by.x =c("date","he_full"),by.y=c("date","he_full"),all.x = T)

#2017 Rider K 0.258 ?/kWh
#2018 Rider K 0.358 ?/kWh
#2019 Rider K 0.203 ?/kWh

#Rider J -0.172 c/kWh
#Rider J +0.01 c/kWh

#Rider G Balancing Pool

#2019 Rider G $0.003/kWh
#2018 Rider G $0.00321/kWh
#2017 Rider G $0.00170/kWh

#Transmission 2017 $0.03080
#Transmission 2018 $0.03025
#Transmission 2019 $0.03132

#Distribution 2017 $0.00861
#Distribution 2018 $0.00907
#Distribution 2018 $0.00951

#RIDER DJ
#2019 0.276 ?/kWh


test_data$T_D<-0.00258-.00172+0.00170+0.03080+0.00861
test_data$T_D[(year(test_data$he_full)==2018)]<-0.00358-.00172+0.00321+0.03025+0.00907
test_data$T_D[(year(test_data$he_full)==2019)]<-0.00203+0.0001+0.003+0.03132+0.00951+0.00276

#load RRO prices

rro_data <- read.xlsx(xlsxFile = "RRO_hist.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
rro_data$`Effective.Start.Date.(mm/dd/yyyy)`<-as.Date(rro_data$`Effective.Start.Date.(mm/dd/yyyy)`,origin = "1899-12-30")
rro_data$`Effective.End.Date(mm/dd/yyyy)`<-as.Date(rro_data$`Effective.End.Date(mm/dd/yyyy)`,origin = "1899-12-30")
rro_data<-select(rro_data,c(4,7,9))
rro_data$month<-month(rro_data$`Effective.Start.Date.(mm/dd/yyyy)`)
rro_data$year<-year(rro_data$`Effective.Start.Date.(mm/dd/yyyy)`)


test_data<-merge(test_data,rro_data,by =c("year","month"),all.x = TRUE)
names(test_data)[grep("RRO_price",names(test_data))]<-"RRO"

test_data$RRO<-as.numeric(test_data$RRO)
test_data$My_price <-as.numeric(test_data$My_price)


test_data<-test_data[!is.na(test_data$actual_posted_pool_price),]

test_data$value=ifelse(test_data$net_to_grid<=0,-1*test_data$net_to_grid/12*test_data$RRO,test_data$net_to_grid/12*(test_data$RRO+test_data$T_D))
test_data$imp_price=ifelse(test_data$net_to_grid<=0,test_data$RRO,(test_data$RRO+test_data$T_D))

test_data$RRO_bill=ifelse(test_data$net_to_grid<=0,test_data$net_to_grid/12*test_data$RRO,test_data$net_to_grid/12*(test_data$RRO+test_data$T_D))
test_data$pool_pricing=ifelse(test_data$net_to_grid<=0,test_data$net_to_grid/12*test_data$actual_posted_pool_price/1000,test_data$net_to_grid/12*(test_data$actual_posted_pool_price/1000+test_data$T_D))
test_data$my_bill=ifelse(test_data$net_to_grid<=0,test_data$net_to_grid/12*test_data$My_price,test_data$net_to_grid/12*(test_data$My_price+test_data$T_D))
test_data$my_cons_bill=test_data$consumptionEnergy/12*(test_data$RRO+test_data$T_D)
test_data$pool_cons_bill=test_data$consumptionEnergy/12*(test_data$actual_posted_pool_price/1000+test_data$T_D)

sum(test_data$my_bill,na.rm = T)
sum(test_data$RRO_bill,na.rm = T)
sum(test_data$pool_pricing,na.rm = T)
sum(test_data$my_cons_bill,na.rm = T)
sum(test_data$pool_cons_bill,na.rm = T)


test_data$new_price<-.09
test_data$new_td<-.043
test_data$test_bill=ifelse(test_data$net_to_grid<=0,test_data$net_to_grid/12*test_data$new_price,test_data$net_to_grid/12*(test_data$new_price+test_data$new_td))
test_data$test_cons_bill=test_data$consumptionEnergy/12*(test_data$new_price+test_data$new_td)
w_solar<-sum(test_data$test_bill,na.rm = T)
wo_solar<-sum(test_data$test_cons,na.rm = T)
paste("With prices averaging $",mean(test_data$new_price),"/kWh and transmission and distribution at $",mean(test_data$new_td),"/kWh. Bill without solar would be $",round(wo_solar,2)," and with solar would be $",round(w_solar,2)," for savings of $",round(wo_solar-w_solar,2),sep = "")


test_data$my_bill=ifelse(test_data$net_to_grid<=0,test_data$net_to_grid/12*test_data$My_price,test_data$net_to_grid/12*(test_data$My_price+test_data$T_D))
test_data$my_bill=ifelse(test_data$net_to_grid<=0,test_data$net_to_grid/12*test_data$My_price,test_data$net_to_grid/12*(test_data$My_price+test_data$T_D))









paired_tableau<-paste(t(cbind(colors_tableau10(),colors_tableau10_light())))

df1<-test_data %>% filter(start<ymd(paste(year(max(start)),month(max(start)),1,sep = "-"))) %>%
  filter(start>=ymd(paste(year(min(start)),month(min(start))+1,1,sep = "-"))) %>%
  group_by(month,year) %>% summarize(RRO_bill=sum(RRO_bill),pool_prices=sum(pool_pricing),
                                     consumption_RRO=sum(my_cons_bill),
                                     consumption_pool_price=sum(pool_cons_bill),
                                     net=sum(net_to_grid),my_bill=sum(my_bill)) %>%
  mutate(date=ymd(paste(year,month,round(days_in_month(month)/2),sep="-")),
         savings=consumption_RRO-my_bill) %>% ungroup()

df1<-melt(df1,id=c("date","savings"),measure.vars = c("my_bill","RRO_bill","consumption_RRO","pool_prices","consumption_pool_price"),
                    value.name = "bill",variable.name = "contract")  %>%
     mutate(contract=factor(contract,labels = c("My Bill","RRO Prices w Solar","RRO Prices w/o Solar",
                               "Pool Prices w Solar","Pool Prices w/o Solar")),
            contract=fct_relevel(contract, "My Bill", after = Inf))

ggplot(subset(test_data,as.Date(start)>=as.Date("2017-09-1")))+
  geom_line(aes(start,T_D,colour="Transmission Charges"),size=2)


png<-1
if(png==1)
  set_png("solar_vcosts.png")
ggplot(df1)+
  #geom_line(aes(date,net,colour="Net"),size=2)+
  
  geom_line(aes(date,bill,group=contract,colour=contract),size=2)+
  #geom_line(aes(date,consumption_RRO,colour="RRO Prices w/o Solar"),size=2)+
  #geom_line(aes(date,RRO_bill,colour="RRO Prices w Solar"),size=2)+
  #geom_line(aes(date,pool_prices,colour="Hourly Pool Prices w Solar"),size=2)+
  #geom_line(aes(date,consumption_pool_price,colour="Hourly Pool Prices w/o Solar"),size=2)+
  scale_x_date(date_breaks = "2 months",date_labels =  "%b\n%Y")+
  scale_color_manual("",values=c(paired_tableau[1:4],"black"))+
  guides(colour = guide_legend(nrow = 2,byrow=FALSE))+
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
             caption=paste("Source: Household power data via Neurio API, graph by Andrew Leach\n",sep=""))
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






