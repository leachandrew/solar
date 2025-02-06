#CAISO Grid Status.io

source("../alberta_power/power_paper_base.R")


download.file("https://www.aeso.ca/assets/Datafile_NetZero_Publication_V1.xlsx",destfile = "aeso_nz.xlsx", mode="wb")

aeso_gen<-read_excel("aeso_nz.xlsx",sheet = "3. Generation Forecast")%>%clean_names()

download.file("https://www.aeso.ca/assets/2021-Long-term-Outlook-data-file-updated-Aug-11.xlsx",destfile = "aeso_lto21.xlsx", mode="wb")
download.file("https://www.aeso.ca/assets/Uploads/2019-Long-term-Outlook-data-file.xlsx",destfile = "aeso_lto19.xlsx", mode="wb")

download.file("https://www.aeso.ca/assets/listedfiles/2017-LTO-data-file.xlsx",destfile = "aeso_lto17.xlsx", mode="wb")

aeso_lto_24<-read_excel("aeso_2024_lto.xlsx",sheet = "Capacity Forecast", range="A9:V24")%>%
  pivot_longer(-1,values_to = "capacity",names_to = "year")%>%clean_names()%>%
  rename("fuel_type"=1)%>%  mutate(year=as.numeric(year),scenario= paste("AESO 2024 LTO, 2050 Case"),
         scenario=as_factor(scenario))

aeso_lto_24b<-read_excel("aeso_2024_lto.xlsx",sheet = "Capacity Forecast", range="A27:V42")%>%
  pivot_longer(-1,values_to = "capacity",names_to = "year")%>%clean_names()%>%
  rename("fuel_type"=1)%>%  mutate(year=as.numeric(year),scenario= paste("AESO 2024 LTO, 2035 Case"),
                                   scenario=as_factor(scenario))


aeso_lto_gen<-read_excel("aeso_lto21.xlsx",sheet = "Generation data by fuel type")%>%clean_names()%>%
  mutate(scenario= paste("AESO 2021 LTO,", stringr::str_sub(scenario, 3)),
         scenario=as_factor(scenario))


aeso_lto_19<-read_excel("aeso_lto19.xlsx",sheet = "Generation Capacity by Type",range = "B4:L9")%>%clean_names()%>%
  mutate(scenario= paste("AESO 2019 LTO, Reference Case"),
         scenario=as_factor(scenario))

aeso_lto_17<-read_excel("aeso_lto17.xlsx",sheet = "Generation Capacity by Type",range = "B4:L9")%>%clean_names()%>%
  mutate(scenario= paste("AESO 2017 LTO, Reference Case"),
         year=as.numeric(gsub("[*]","",year)),
         scenario=as_factor(scenario))


ggplot(aeso_gen%>%filter(scenario %in%c("Reference Case","Renewables and Storage Rush"),fuel_type=="Solar",output=="Capacity Begin Year (MW)")%>%
         mutate(scenario = paste("AESO 2022 Net Zero Report,",scenario)))+
  geom_line(aes(calendar_year,value,group=scenario,color=scenario),linewidth=1.5)+
  geom_line(data=aeso_lto_24%>%filter(fuel_type=="Solar"),aes(year,capacity,color="AESO 2024 LTO, Both Cases"),linewidth=1.5)+
  geom_line(data=aeso_lto_gen %>% filter(scenario %in% c("AESO 2021 LTO, Reference Case","AESO 2021 LTO, Clean-Tech Scenario"),
                                         fuel_type=="Solar",
                                         category=="Capacity Begin Year (MW)",
                                         TRUE)%>%mutate(scenario=as_factor(scenario)),aes(calendar_year,value,group=scenario,color=scenario),linewidth=1.5)+
  geom_line(data=aeso_lto_19,aes(year,solar,color="AESO 2019 LTO, Reference Case"),linewidth=1.5)+
  geom_line(data=aeso_lto_17,aes(year,solar,color="AESO 2017 LTO, Reference Case"),linewidth=1.5)+
  
  annotate(geom="point",x=2024,y=1650,size=4)+
  annotate(geom="text",x=2024,y=1650,label="April 2024",hjust=-.2,vjust=0.5)+
  annotate(geom="point",x=2021,y=236,size=4)+
  annotate(geom="text",x=2021,y=236,label="April 2021",hjust=1.1,vjust=0.5)+
  scale_color_manual("",values = colors_tableau10())+
  guides(color=guide_legend(nrow = 2,byrow=TRUE))+
  theme_minimal()+
  theme(
    legend.position = "bottom",
    legend.margin=margin(c(.05,0,.05,0),unit="cm"),
    legend.text = element_text(colour="black", size = 11),
    plot.caption = element_text(size = 10, face = "italic",hjust=0),
    plot.title = element_text(size=16,face = "bold"),
    plot.subtitle = element_text(size = 10),
    #panel.grid.minor = element_blank(),
    #axis.text.x = element_blank(),
    #axis.text = element_text(size = 10, colour = "black", hjust=0.5,vjust=0.5),
    #axis.text.x = element_text(angle=-90),
    #axis.title = element_text(size = 14, colour="black"),
    #axis.title.x = element_text(margin = margin(t = 10, r = 1, b = 1, l = 1)),
    #axis.ticks = element_blank(),
    text = element_text(size = 20,family="TT Times New Roman")  
  )+
  labs(x="",y="Alberta Installed Solar Capacity (MW)",
       title="AESO's Solar Forecasts",
       caption=str_wrap("Data via AESO, graph by @andrew_leach.",width = 180),
       NULL
  )
ggsave("images/aeso_solar.png",width=14.5,height=7,bg="white",dpi=200)
  



  #end<-Sys.time()-days(3)
  #dataset<-"caiso_standardized_5_min"
  #key_sent=grid_status_key)





https://api.gridstatus.io/v1/datasets/caiso_standardized_5_min&start_time=2023-06-02 10:03:30&end_time=2023-06-04 10:05:47&api_key=6cb5e350439cdec7f79c2dd592480e95