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
  #if(as.numeric(test$headers$`ratelimit-remaining`<2))
  #    print("rate limit exceedance coming in")
  #    test$headers$`ratelimit-reset`
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
  #if rate limit is exceded, we should return a flag.
} #end data routine


neurio_raw<-function(start_string,end_string,time_unit,time_freq,sensor_id,token_id)
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
  #if(as.numeric(test$headers$`ratelimit-remaining`<2))
  #    print("rate limit exceedance coming in")
  #    test$headers$`ratelimit-reset`
  #test<-httr::VERB(verb = "GET", url = "https://api.neur.io/v1/samples/stats?sensorId=0x0000C47F510354AE&start=2017-08-16T19:35:00Z&end=2017-08-17T05:51:08Z&granularity=minutes&frequency=5&timezone=America/Edmonton", 
  #                 httr::add_headers(Authorization = "Bearer AIOO-2nAWDW89l_OJHK4wnuOKi_paoTLxKREWXZQuSLpefCSAhfxbeIslCQyQ8VXBiyvfRq3mlXOFLOu8clU31RN5l6B9YayQlP35VV12jtmrfueoWfabUfNtRl4HwNoNqeKxnxnlp7gl4zp-c4scD40OF-yZQgMZvf85Fzk7NXqycVBRC1EX7zjdKqjLyQsHRa2XhRzfgZSIxFMBcjZI-Rr_m6xq3dP3Tufp6rlaEVoViVyp24I3YCu9ndBSdg3fRUtyRN7nKgz"), 
  #                 encode = "json")
  #print(paste("Rate limit remaining is ",test$headers$`ratelimit-remaining`))
  raise <- content(test, as="text")
  #convert to list using RJSONIO
  fromJSON(raise) -> new
  testing<<-new
  #convert from watt-seconds to kWh
  return(new)
  #if rate limit is exceded, we should return a flag.
} #end data routine



sample_neurio<-function(){
  sample_time<-paste(format(with_tz(as.POSIXct(Sys.time())-hours(1),tz = "UTC"),"%Y-%m-%dT%H:%M:%SZ"),sep="")
  api_url = paste("https://api.neur.io/v1/samples/live?","sensorId=",sensor_id,"&start=",sample_time,sep="")
print(api_url)
test<-httr::VERB(verb = "GET", url = api_url, 
                 httr::add_headers(Authorization = paste("Bearer ",token_id,sep="")), 
                 encode = "json")
print(paste("Rate limit remaining is ",test$headers$`ratelimit-remaining`))
print(paste("Rate limit reset in ",as.numeric(test$headers$`ratelimit-reset`)/1000,"seconds"))
print(paste("Rate limit reset at ",format(Sys.time()+milliseconds(as.numeric(test$headers$`ratelimit-reset`)))))

#raise <- content(test, as="text")
#convert to list using RJSONIO
#fromJSON(raise) -> new
#testing<<-new
#return(testing)
}


get_hourly_data <- function(data_sent,token=token_id,sensor=sensor_id) {
  #testing
    #token<-token_id
    #sensor<-sensor_id
   #sys_data<-hourly_data
  #end testing
  
  time_unit<-"hours"
  time_freq<-1
  
  if(missing(data_sent)){
    start_string<-"2017-08-16 13:00:00 MDT"
    #start_string<- Sys.time()-24*60*60
    end_string<-round_date(Sys.time()-hours(1), unit = "hour")
    #end_string<-"2017-08-20 12:00:00 MDT"
    ##end_string<- Sys.time()
    #get initiatial data
    start_point<-start_string
    end_point<-as.POSIXct(start_string)+days(30)
    sys_data<-get_neurio(start_point,end_point,time_unit,time_freq,sensor,token)%>% 
      mutate(year=year(start),month=month(start),hour=hour(start),he=hour(start)+1,day=day(start),date=date(start))
  }
  if(!missing(data_sent)){
    sys_data<-data_sent}
  start_string<-max(sys_data$end)
  start_point<-start_string  
  end_string<-min(as.POSIXct(start_point)+days(120*30),round_date(Sys.time()-hours(2), unit = "hour"))
  while(start_point<end_string)
  {
    #get data
    end_point<-min(as.POSIXct(start_point)+days(30),end_string)
    new_data<-get_neurio(start_point,end_point,time_unit,time_freq,sensor,token)
    new_data<-new_data %>% mutate(year=year(start),month=month(start),hour=hour(start),he=hour(start)+1,day=day(start),date=date(start))
    sys_data<-rbind(sys_data,new_data)
    start_point<-end_point
    print(paste("Iteration done until ",end_point,sep = ""))
  }
sys_data
}






#get and stack all available data

#need to break it up into a maximum of 120 requests per hour.
#so, do this for a max of 100 days? Or build in a pause? 
#you can read the message in get_neurio and the pause for the number of seconds and go back


update_data <- function(data_sent,token=token_id,sensor=sensor_id) {
  #testing
  #data_sent<-sys_data
  
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
  start_point<-start_string  
  end_string<-min(as.POSIXct(start_point)+days(120),Sys.time())
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



