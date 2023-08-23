library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(viridis)
library(scales)
library(openxlsx)
library(janitor)


munis<-read_excel("municip_potentiel-potential.xlsx")%>%
  clean_names()
  

names(munis)<-munis[2,]

munis<-munis %>%  clean_names()%>%rename(month=mois)%>%
  rename(two_axis_tracking=x2_axis_tracking)%>%
  slice(-c(1,2,3))%>%
  pivot_longer(-c(province,municipality,month))

munis<-munis%>% mutate(value=as.numeric(value))


regway<-read_csv("pvwatts_regway.csv",skip=31)%>%
  clean_names()%>%mutate(muni="Regway, SK")

nrel_data<-read_csv("pvwatts_arizona.csv",skip=31)%>%
  clean_names()%>%mutate(muni="Phoenix, AZ")%>%
  bind_rows(regway)



  

ggplot(nrel_data %>%
         mutate(dc_array_output_w=dc_array_output_w/1000/1000,
                month=factor(month.abb[month],levels=month.abb))%>%
         group_by(month,hour,muni)%>%
         summarize(
           
           median.val = median(dc_array_output_w, na.rm = TRUE)*100,
           mean.val = mean(dc_array_output_w, na.rm = TRUE)*100,
           #lower.ci = min(Value, na.rm = TRUE),
           upper.ci=quantile(dc_array_output_w, probs=c(.95))*100,
           lower.ci=quantile(dc_array_output_w, probs=c(.05))*100
         ))+
  #geom_line(aes(hour,mean.val,color=muni,group=muni))+
  facet_wrap(~month)+
  expand_limits(y=100)+
  scale_x_continuous(breaks=pretty_breaks(n=12), expand = c(0,0))+
  labs(y="Hourly Capacity Factor (%))", x="",)+
  #geom_ribbon(aes(x = hour, ymin = upper.ci, ymax = lower.ci,group=muni,fill=muni),alpha=.25, linetype = 0)+
  geom_line(aes(x=hour, y=mean.val, group=muni,color=muni),size=.75) +
  scale_y_continuous(breaks=pretty_breaks(),expand=c(0,0))+
  scale_colour_manual("",values=c("#BF0A30","#016a1b"))+
  scale_fill_manual("",values = c("#BF0A30","#016a1b"))+
  ##scale_color_manual(values = my_palette,
  #                  labels=label_func)+
  guides(linetype=guide_legend(nrow = 1,
                               keywidth=2)
  )+
  expand_limits(y=0)+
  theme_minimal()+
  #theme_bw()+
  theme(
    legend.title=element_blank(),
    legend.position="bottom",
    legend.margin=margin(c(0,0,0,0),unit="cm"),
    legend.spacing.x = unit(0.2, 'cm'),
    legend.spacing.y = unit(0.2, 'cm'),
    legend.text = element_text(colour="black", size = 14, face = "bold"),
    plot.caption = element_text(size = 8,face = "italic",hjust=0),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size =14, face = "italic"),
    panel.grid.minor = element_blank(),
    text = element_text(size = 12,face = "bold"),
    axis.text = element_text(size = 12,face = "bold", colour="black"),
    axis.title = element_text(size = 12,face = "bold", colour="black"),
    panel.spacing = unit(2, "lines")
  )+
  labs(y=expression(paste("Average Hourly Capacity Factor (%)")),x="Year",
       #subtitle=paste("Mean model predictions, SSP 2",sep=""),
       title="Mean hourly solar capacity factor: Phoenix, AZ vs. Regway, SK",
       caption="Two-axis tracking system estimate via NREL PVWatts, graph by Andrew Leach")

ggsave("regway.png",width=16, height=12,dpi=300,bg="White")





home_gas_data<-read_excel("gas_use.xlsx",skip=2)%>%
  clean_names()%>%mutate(date=ymd(usage_period_start),
                         kwh=usage*277.778)



home_gas_month <-home_gas_data%>%
  mutate(month=month(date),year=year(date))%>%
  group_by(month,year)%>%
  summarize(
    usage=sum(usage),kwh=sum(kwh)
  )%>%
  mutate(month=factor(month.abb[month],levels=month.abb),
         year=factor(year))

ggplot(home_gas_month%>%filter(as.character(year)>=2019))+
  geom_line(aes(month,kwh,group=year,color=year))
  