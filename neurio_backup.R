library(plyr)
require(dplyr)
require(tidyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(curlconverter)
library(curl)
library(lubridate)
library(viridis)
library(scales)
library(openxlsx)


#create long data file

setwd("C:/Users/aleach/Google Drive")

png<-0 #set to one to output to files, anything else to output to screen.


sensor_id <- "0x0000C47F510354AE"

#
#req <- make_req(straighten())[[1]]

#status check, neurio API
neurio_api <- GET("https://api.neur.io/v1/status")
neurio_r <- fromJSON(rawToChar(neurio_api$content))






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
    sys_data<-rbind(sys_data,new_data)
    start_point<-end_point
    print(paste("Iteration done until ",end_point,sep = ""))
  }
  return(sys_data)  
}


load_and_update<-function(load_file="solar_data.RData"){
  
  load(load_file,.GlobalEnv) 
  sys_data<<-update_data(sys_data)
  save(sys_data, file= "solar_data.RData")
  write.xlsx(sys_data, file = "neurio_5_min.xlsx", colNames = TRUE, borders = "columns") 
}



hourly_data<-function(){
  time_unit<-"hours"
  time_freq<-1
  start_string<-"2017-08-16 13:00:00 MDT"
  end_string<-Sys.time()
  start_point<-start_string
  end_point<-as.POSIXct(start_string)+hours(24)
  hourly_data<-get_neurio(start_point,end_point,time_unit,time_freq,sensor_id,token_id)
  start_string<-max(hourly_data$end)
  end_string<-Sys.time()
  start_point<-start_string  
  while(start_point<end_string)
  {
    #get data
    end_point<-as.POSIXct(start_point)+days(30)
    new_data<-get_neurio(start_point,end_point,time_unit,time_freq,sensor_id,token_id)
    hourly_data<-rbind(hourly_data,new_data)
    start_point<-end_point
    print(paste("Iteration done until ",end_point,sep = ""))
  }
  
  save(hourly_data, file= "hourly_data.RData")
  write.xlsx(hourly_data, file = "neurio_hourly.xlsx", colNames = TRUE, borders = "columns") 
}



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


load("simon_5min_data.RData",.GlobalEnv)
simon_data<-simon_data[simon_data$start<as.POSIXct("2018-01-16"),]
simon_data<-simon_update(simon_data)
save(simon_data,file="simon_5min_data.RData")
write.xlsx(simon_data, file = "simon_neurio_5min.xlsx", colNames = TRUE, borders = "columns") 


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
load("solar_homes_5min_data.RData",.GlobalEnv)
solar_homes_data<-solar_homes_update(solar_homes_data)
save(solar_homes_data, file= "solar_homes_5min_data.RData")
write.xlsx(solar_homes_data, file = "solar_homes_5min.xlsx", colNames = TRUE, borders = "columns")

png<-1
if(png==1)
  png(file="solar_homes_system_shapes.png", width = 1400, height = 900,res=130,type='cairo')
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
  png(file="all_3.png", width = 1400, height = 750,res=130,type='cairo')
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




#get a token if you need one
#sys_data<-sys_data[sys_data$start<as.POSIXct("2017-12-15 08:45:00"),]
#save(sys_data, file= "solar_data.RData")
token_id<- get_token(my_client_id,my_client_secret)
load("solar_data.RData",.GlobalEnv) 
load_and_update()

load(file= "simon_5min_data.RData")
simon_data<-simon_data[simon_data$start<as.POSIXct("2017-12-15 08:45:00"),]
#save(simon_data, file= "simon_5min_data.RData")
simon_update()


token_id<- get_token(my_client_id,my_client_secret)
hourly_data()
#load("hourly_data.RData",.GlobalEnv) 

combined<-merge(simon_data, sys_data, by = c("start","end"),suffixes = c(".simon",".andrew"),all=FALSE) # NA's match
#sum, but adjust for time frequency of 5 min data
combined$cum_gen.simon<-cumsum(combined$generationEnergy.simon/12)
combined$cum_gen.andrew<-cumsum(combined$generationEnergy.andrew/12)

#combined_new<-merge(combined,solar_homes_data, by = c("start","end"),suffixes = c(".combo",".slhms"),all=FALSE) # NA's match
#combined_new$generationEnergy.simon[combined_new$generationEnergy.simon<=0]<-0

lims <- c(min(combined$start),max(combined$start))
#breaks<- as.POSIXct(seq.POSIXt(min(combined$start),max(combined$start), by = "1 month") )

breaks<-seq.POSIXt(min(combined$start)+days(1), max(combined$start)-days(1), by="5 days")
combined$day<-date(combined$end)
df1<-combined %>% group_by(day)%>% summarise(generationEnergy.andrew=sum(generationEnergy.andrew/12),generationEnergy.simon=sum(generationEnergy.simon/12))

png<-1
if(png==1)
  png(file="simon_vs_andrew.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(df1, aes(day,generationEnergy.simon,colour="Simon's Generation",fill="Simon's Generation")) +
  geom_col(size=1,alpha=1,position="dodge") +
  geom_col(aes(day,generationEnergy.andrew,colour="Andrew's Generation",fill="Andrew's Generation"),size=1,alpha=0.5,position="dodge")+
  #geom_line(aes(start,net_imp,colour="Power/nDeliveries"),size=2)+
  #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
  scale_fill_viridis("",labels = c("Andrew's\nGeneration","Simon's\n Generation"),discrete = TRUE,option="C")+
  scale_color_viridis("",labels = c("Andrew's\nGeneration","Simon's\n Generation"),discrete = TRUE,option="C")+
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
  )+    labs(y="Power Generation (kWh per day)",x="\nTime",
             title="Leach (7.6kW) vs Collier (8.37kW) Household Solar Generation",
             caption="Source: Generation via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

breaks<-seq.POSIXt(min(combined$start)+days(1), max(combined$start)-days(1), by="2 weeks")


png<-1
if(png==1)
  png(file="simon_vs_andrew_cum.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(combined, aes(start,cum_gen.simon,colour="Simon's Generation")) +
  geom_line(size=2) +
  geom_line(aes(start,cum_gen.andrew,colour="Andrew's Generation"),size=2)+
  #geom_line(aes(start,net_imp,colour="Power/nDeliveries"),size=2)+
  #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
  scale_color_viridis("",labels = c("Andrew's\nGeneration","Simon's\n Generation"),discrete = TRUE,option="D")+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m/%d\n%Hh", tz="America/Denver"))+
  scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m/%d", tz="America/Denver"))+
  
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
  )+    labs(y="Cumulative Generation (kWh)",x="\nTime",
             title="Leach (7.6kW) vs Collier (8.37kW) Household Solar Generation",
             caption="Source: Generation via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

combined$cum_net_to_grid.andrew<-cumsum(combined$net_to_grid.andrew/12)
combined$cum_net_to_grid.simon<-cumsum(combined$net_to_grid.simon/12)
sys_data$cum_net_to_grid<-cumsum(sys_data$net_to_grid)
#plot(sys_data$end,sys_data$cum_net_to_grid)

png<-1
if(png==1)
  png(file="simon_vs_andrew_net.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(combined, aes(start,cum_net_to_grid.simon,colour="Simon's\nNet Electricity\nPurchases")) +
  geom_line(size=2) +
  geom_line(aes(start,cum_net_to_grid.andrew,colour="Andrew's\nNet Electricity\nPurchases"),size=2)+
  #geom_line(aes(start,net_imp,colour="Power/nDeliveries"),size=2)+
  #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
  scale_color_viridis("",labels = c("Andrew's\nNet Electricity\nPurchases","Simon's\nNet Electricity\nPurchases"),discrete = TRUE,option="D")+
  scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m/%d\n%Hh", tz="America/Denver"))+
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
  )+    labs(y="Cumulative Net Purchases (kWh)",x="\nTime",
             title="Leach (7.6kW) vs Collier (8.37kW) Household Electricity Net Purchases",
             caption="Source: Generation and Exports via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()






png<-0
if(png==1)
  png(file="actual_vs_avg.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(combined, aes(start,Simon_net_to_grid,colour="Simon's Net Electricty Purchases")) +
  geom_line(size=2) +
  geom_line(aes(start,Andrew_net_to_grid,colour="Andrew's Generation"),size=2)+
  #geom_line(aes(start,net_imp,colour="Power/nDeliveries"),size=2)+
  #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
  scale_color_viridis("",labels = c("Andrew's Net\n to (-) or\nfrom (+) Grid","Simon's Net\n Net to (-) or\nfrom (+) Grid"),discrete = TRUE,option="D")+
  #scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a, %m/%d\n%Hh", tz="America/Denver"))+
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
  )+    labs(y="Net Electricity Purchases (kW)",x="\nTime",
             title="Leach (7.6kW) vs Collier (8.37kW) Household Solar Generation",
             caption="Source: Generation via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()






house_data <- read.xlsx(xlsxFile = "hourly-house.xlsx", sheet = 1, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
house_data$Date<-as.POSIXct(paste(house_data$Year,"-",house_data$Month,"-",house_data$Day," ",house_data$Hour,":00",sep=""),format='%Y-%m-%d %H:%M')
View(head(house_data))

solar_data <- read.xlsx(xlsxFile = "solar_system.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
solar_data$Date<-as.POSIXct(paste(2016,"-",solar_data$Month,"-",solar_data$Day," ",solar_data$Hour+1,":00",sep=""),format='%Y-%m-%d %H:%M')
solar_data$Hour<-solar_data$Hour+1
res_data <- read.xlsx(xlsxFile = "YEG_Res_Load.xlsx", sheet = 2, startRow = 1,skipEmptyRows = TRUE,detectDates = TRUE)
res_data$Date<-as.POSIXct(paste(2016,"-",res_data$Month,"-",res_data$Day," ",res_data$Hour,":00",sep=""))

assignment_data<-merge(house_data,solar_data,by=c("Date"))
asst_data<-data.frame(assignment_data$Date,assignment_data$`Use.(kWh)`*750/527,assignment_data$`AC.System.Output.(W)`/1000,assignment_data$`750_load`)
names(asst_data)<-c("Date","Use_kWh","gen_kWh","avg_load")
#write.xlsx(asst_data, file = "assignment_data.xlsx", colNames = TRUE, borders = "columns") 

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

#solar_data$importedEnergy<-(solar_data$`750_load`-solar_data$`AC.System.Output.(W)`)*(solar_data$`750_load`-solar_data$`AC.System.Output.(W)`>0)
#solar_data$exportedEnergy<-(solar_data$`AC.System.Output.(W)`-solar_data$`750_load`)*(solar_data$`AC.System.Output.(W)`-solar_data$`750_load`>0)
solar_data$date<-paste("2017-",solar_data$Month,"-",solar_data$Day," ",solar_data$Hour,":00",sep="")
solar_data$date<-as.POSIXct(solar_data$date,format='%Y-%m-%d %H:%M')

solar_data<-solar_data[(solar_data$date>=start_string&solar_data$date<=end_string),]
test_data<-merge(sys_data,solar_data,by.x="end",by.y="date")




lims <- c(min(sys_data$start),max(sys_data$start))
#breaks<- as.POSIXct(seq.POSIXt(min(sys_data$start),max(sys_data$start), by = "1 month") )

breaks<-seq.POSIXt(min(sys_data$start)+hours(12), max(sys_data$start), by="1 day")

png<-1
if(png==1)
  png(file="actual_vs_avg.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(test_data, aes(end,`750_load`,colour="Predicted Consumption")) +
  geom_line(size=2) +
  geom_line(aes(end,consumptionEnergy,colour="Actual Consumption"),size=2)+
  #geom_line(aes(start,net_imp,colour="Power/nDeliveries"),size=2)+
  #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
  scale_color_viridis("",labels = c("Household\nConsumption","Average EPCOR\nResidential Load"),discrete = TRUE,option="C")+
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
  )+    labs(y="Energy Consumption (kWh)",x="\nTime",
             title="Actual vs Average Household Load (170kWh/week)",
             caption="Source: Consumption data via Neurio API, Average load data via EPCOR\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()


lims <- c(as.POSIXct(date(Sys.Date())-days(7)),as.POSIXct(Sys.Date()))

breaks<-seq.POSIXt(min(lims)+hours(19), max(lims), by="1 day")


png<-1
if(png==1)
  png(file="home_system_shapes.png", width = 1400, height = 900,res=130,type='cairo')
ggplot(data=subset(sys_data,start>Sys.Date()-days(7)), aes(start,consumptionEnergy,colour="Consumption")) +
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
             caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

sys_data$month<-month(sys_data$start)
sys_data$hour<-hour(sys_data$start)
#sys_data$month = factor(month.abb[sys_data$month], levels = month.abb)

df1 <- sys_data %>% group_by(month,hour) %>% summarise(gen=mean(generationEnergy),imps=mean(importedEnergy),cons=mean(consumptionEnergy),net=mean(net_to_grid))
df1$month = factor(month.abb[df1$month], levels = month.abb)


png<-1
if(png==1)
  png(file="hourly_gen.png", width = 1400, height = 900,res=130,type='cairo')
ggplot(df1)+
  geom_line(aes(hour,gen,colour=month,group=month),size=2)+
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
  labs(x="Hour",y="Average Hourly Generation (kWh)",
       title="Hourly Solar Generation (2017-18 Avg)",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
if(png==1)
  dev.off()

if(png==1)
  png(file="hourly_net.png", width = 1400, height = 900,res=130,type='cairo')
ggplot(df1)+
  geom_line(aes(hour,net,colour=month,group=month),size=2)+
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
  labs(x="Hour",y="Average Hourly Net Consumption (kWh)",
       title="Hourly Consumption Net of Solar Generation (2017-18 Avg)",
       caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
if(png==1)
  dev.off()


df1 <- sys_data %>% group_by(month,hour) %>% summarise(gen=mean(generationEnergy),imps=mean(importedEnergy),cons=mean(consumptionEnergy),net=mean(net_to_grid),max_net=max(consumptionEnergy-generationEnergy),max_cons=max(consumptionEnergy))
df1$month = factor(month.abb[df1$month], levels = month.abb)

if(png==1)
  png(file="hourly_peak_cons.png", width = 1400, height = 900,res=130,type='cairo')
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



load("hourly_data.RData")
#work_data<-subset(hourly_data,start>=as.Date("2017-08-18"))
work_data<-asst_data
names(work_data)[8]<-"net_from_grid"
#call on battery

batt_size<-13.5

for(batt_size in seq(0,13.5*5,by=13.5)){
batt_in<- - 5
batt_out<-5
init_charge<-0
batt_eff<-.9
elec_price<-.05
t_d_price<-.055



work_data$batt_call<-work_data$net_from_grid
work_data$batt_charge<-work_data$exportedEnergy*0+init_charge
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
 
 print(paste("Battery size is ",batt_size," and annual imports are ",round(sum(work_data$net_imp[work_data$net_imp>=0]), digits=2)," Total billable variable costs are ",round(sum(work_data$charges),digits=2)))
 write.xlsx(work_data, file = paste("battery_size_",batt_size,".xlsx",sep=""), colNames = TRUE, borders = "columns")
 
}
 


lims <- c(as.POSIXct("2017-10-18 0:00:00 MDT"), as.POSIXct("2017-10-24 23:00:00 MDT"))

breaks<-seq.POSIXt(min(lims)+hours(12), max(lims), by="1 day")

png<-1
if(png==1)
  png(file="home_battery.png", width = 1400, height = 900,res=130,type='cairo')
ggplot(work_data, aes(start,net_imp,colour="Net Consumption")) +
  geom_line(size=2) +
  geom_line(aes(start,net_from_grid,colour="Original Net"),linetype=6,size=2,alpha=.75)+
  geom_line(aes(start,batt_pwr,colour="Battery Flow"),size=2)+
  geom_line(aes(start,batt_charge,colour="Battery Charge"),size=2)+
  #geom_line(aes(start,net_imp ,colour="Power/nDeliveries"),size=2)+
  #geom_line(aes(start,generationEnergy,colour="Solar/nGeneration"),size=2)+
  scale_color_viridis("",option="C",labels = c("Battery\nCharge","Charge (+) or \n Discharging (-)\nFlows", "Purchases (+) or \n Sales (-) of\nElectricity","Purchases (+) or \n Sales (-) of\nElectricity\n w/o Battery"),discrete = TRUE)+
  #scale_color_viridis("",labels = c("Battery\nCharge","Consumption","Net Electricity\nPurchases","Solar\nGeneration"),discrete = TRUE,option="C")+
  scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
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
  )+    labs(y="Energy Generation and Purchases \nand Battery Charge (kWh)",x="\nTime",
             title="Household Energy Shapes with one 13.5 KW Tesla Powerwall",
             caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()





lims <- c(min(work_data$start),max(work_data$start))
#breaks<- as.POSIXct(seq.POSIXt(min(work_data$start),max(work_data$start), by = "1 month") )

breaks<-seq.POSIXt(min(work_data$start)+hours(12), max(work_data$start), by="1 day")


png<-0
if(png==1)
  png(file="delivery.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(work_data, aes(start,consumptionEnergy-generationEnergy,colour="Consumption"),colour="Consumption") +
  geom_line(size=2) +
  geom_line(aes(start,importedEnergy,colour="Delivered Energy"),size=2)+
  #geom_line(aes(start,net_from_grid,colour="Electricity from grid"),size=2)+
  #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
  scale_color_viridis("",labels = c("Net\nEnergy", "Delivered\nEnergy"),discrete = TRUE,option="D")+
  scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
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
  )+    labs(y="Energy (kW)",x="\nTime",
             title="Leach Household Energy Monitor",
             caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()







lims <- c(as.POSIXct("2017-08-21 6:00:00 MDT"), as.POSIXct("2017-08-21 17:00:00 MDT"))
breaks <- make_breaks(min(sys_data$start), hour=0, interval='2 hour', length.out=length(sys_data))

png<-0
if(png==1)
  png(file="eclipse.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(sys_data) +
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
    legend.position = "bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.text = element_text(colour="black", size = 16, face = "bold"),
    plot.caption = element_text(size = 14, face = "italic"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 16, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 16,face = "bold", colour="black")
  )+    labs(y="Energy (kW)",x="\nTime",
             title="Andrew Leach's residential rooftop solar generation\nduring the 2017 solar eclipse",
             caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



lims <- c(as.POSIXct("2017-12-26 6:00:00 MDT"), as.POSIXct("2017-12-26 20:00:00 MDT"))
breaks <- make_breaks(min(lims), hour=0, interval='2 hour', length.out=12)

png<-1
if(png==1)
  png(file="snow_clearing.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(subset(sys_data,start>= as.POSIXct("2017-12-26 6:00:00 MDT") & start <= as.POSIXct("2017-12-26 20:00:00 MDT")))+
  geom_line(aes(start,generationEnergy,colour="Solar Generation"),size=2) +
  #geom_line(aes(start,generationEnergy,colour="Generation"),size=2)+
  #geom_line(aes(start,net_to_grid,colour="Electricity from grid"),size=2)+
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.POSIXct("2017-12-26 12:30:00 MDT"), xmax =min(as.POSIXct("2017-12-26 23:00:00 MDT")),
           ymin = 0, ymax = 2) +
  annotate("text", x = as.POSIXct("2017-12-26 17:00:00 MDT"), y = 1, label = "After brushing off\nmy solar panels",size=6)+
  annotate("text", x = as.POSIXct("2017-12-26 8:00:00 MDT"), y = 1, label = "Before brushing off\nmy solar panels",size=6)+
  scale_color_viridis("",labels = c("Solar Generation"),discrete = TRUE)+
  scale_x_datetime(labels = date_format("%a\n%m-%d\n%H:00", tz="America/Denver"))+
  #scale_y_continuous(limits=c(0,2))+
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
  )+    labs(y="Energy (kW)",x="\nTime",
             title="When you clean the snow off your solar panels",
             caption="Source: SolarPeople system data via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()






library(dplyr)
library(ggplot2)
library(Hmisc)

supply <- Hmisc::bezier(x = c(1, 8, 9),
                        y = c(1, 5, 9)) %>%
  as_data_frame()

ggplot(supply, aes(x = x, y = y)) + 
  geom_path(color = "#0073D9", size = 1) + 
  theme_classic() + 
  coord_equal()+
  labs(y="Number of mistakes made by Andrew",x="Number of People Andrew Emails")



#compatison, mine vs Simon's          


simon_token_id<- get_token(simon_client_id,simon_client_secret)
my_token_id<- get_token(my_client_id,my_client_secret)


time_unit<-"minutes"
time_freq<-15


start_string<-"2017-10-12 0:00:00 MDT"
#start_string<- Sys.time()-24*60*60
end_string<-"2017-10-24 23:00:00 MDT"
##end_string<- Sys.time()
combined<-data.frame()
start_point<-start_string  
while(start_point<end_string)
{
  #get data
  end_point<-as.POSIXct(start_point)+hours(24)
  my_data<-get_neurio(start_point,end_point,time_unit,time_freq,sensor_id,token_id)
  my_data<-setNames(my_data,paste("Andrew_",colnames(my_data),sep=""))
  colnames(my_data)[5]<-"start"
  colnames(my_data)[6]<-"end"
  
  simons_data<-get_neurio(start_point,end_point,time_unit,time_freq,simon_sensor_id,simon_token_id)
  simons_data<-setNames(simons_data,paste("Simon_",colnames(simons_data),sep=""))
  colnames(simons_data)[5]<-"start"
  colnames(simons_data)[6]<-"end"
  
  new_data<-merge(my_data,simons_data,by="start")
  combined<-rbind(combined,new_data)
  start_point<-end_point
  print(paste("Iteration done until ",end_point,sep = ""))
}


combined$simon_avg_gen<-as.data.frame(rollapply(combined$Simon_generationEnergy,12,mean,fill=NA,align = c("right")))[,1]

combined$andrew_avg_gen<-as.data.frame(rollapply(combined$Andrew_generationEnergy,12,mean,fill=NA,align = c("right")))[,1]

combined$simon_net<-as.data.frame(rollapply(combined$Simon_net_to_grid,12,mean,fill=NA,align = c("right")))[,1]

combined$andrew_net<-as.data.frame(rollapply(combined$Andrew_net_to_grid,12,mean,fill=NA,align = c("right")))[,1]



df1<-melt(combined,id=c("start"),measure.vars = c("simon_avg_gen","andrew_avg_gen"))
df1<-na.omit(df1)

lims <- c(min(combined$start),max(combined$start))
breaks<-seq.POSIXt(min(combined$start)+hours(12), max(combined$start), by="1 day")

a_gen<-sum(na.omit(combined$andrew_avg_gen))
s_gen<-sum(na.omit(combined$simon_avg_gen))
a_lab<-paste("Leach\nGeneration\n",round(a_gen,0), "kWh Total")
s_lab<-paste("Collier\nGeneration\n",round(s_gen,0), "kWh Total")

png<-0
if(png==1)
  png(file="actual_vs_avg.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(combined, aes(start,simon_avg_gen,colour="Simon's Generation")) +
  geom_line(size=2) +
  geom_line(aes(start,andrew_avg_gen,colour="Andrew's Generation"),size=2)+
  #geom_line(aes(start,net_imp,colour="Power/nDeliveries"),size=2)+
  #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
  #scale_color_viridis("",labels = c("Andrew's\nGeneration","Simon's\n Generation"),discrete = TRUE,option="A",direction=1)+
  #scale_color_brewer("",labels = c("Andrew's\nGeneration","Simon's\n Generation"),palette = "Set1")+
  scale_color_brewer("",labels = c(a_lab,s_lab),palette = "Set1")+
  scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n %m/%d\n%Hh", tz="America/Denver"))+
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
  )+    labs(y="Power Generation (kW)",x="\nTime",
             title="Leach (7.6kW) vs Collier (8.37kW) Household Solar Generation",
             caption="Source: Generation via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()

a_net<-sum(na.omit(combined$andrew_net))/60*time_freq
s_net<-sum(na.omit(combined$simon_net))/60*time_freq
a_lab<-paste("Leach\nNet Purchases\n",round(a_net,0), "kWh Total")
s_lab<-paste("Collier\nNet Purchases\n",round(s_net,0), "kWh Total")


png<-0
if(png==1)
  png(file="actual_vs_avg.png", width = 1400, height = 750,res=130,type='cairo')
ggplot(combined, aes(start,simon_net,colour="Simon's Net Purchases")) +
  geom_line(size=2) +
  geom_line(aes(start,andrew_net,colour="Andrew's Net Purchases"),size=2)+
  #geom_line(aes(start,net_imp,colour="Power/nDeliveries"),size=2)+
  #scale_color_viridis("",labels = c("Household\nConsumption","Net Electricity\nPurchases", "Solar Generation"),discrete = TRUE)+
  #scale_color_viridis("",labels = c("Andrew's\nGeneration","Simon's\n Generation"),discrete = TRUE,option="A",direction=1)+
  #scale_color_brewer("",labels = c("Andrew's\nGeneration","Simon's\n Generation"),palette = "Set1")+
  scale_color_brewer("",labels = c(a_lab,s_lab),palette = "Set1")+
  scale_x_datetime(limits = lims,breaks=breaks,labels = date_format("%a\n %m/%d\n%Hh", tz="America/Denver"))+
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
  )+    labs(y="Net Purchases (kWh)",x="\nTime",
             title="Net Purchases After Leach (7.6kW) vs Collier (8.37kW) Household Solar Generation",
             caption="Source: Generation via Neurio API\nGraph by Andrew Leach")
if(png==1)#set these to only turn on if you're making PNG graphs
  dev.off()



write.xlsx(combined, file = "neurio_compare_5_min.xlsx", colNames = TRUE, borders = "columns") 



