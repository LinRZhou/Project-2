library(stringr)

library(plyr)

library (tidyverse)

#setwd("C:/Users/linra/Documents/UNC Masters Degree/Fall 2018/BIOS 611/Project 2")

HIV_prev_df=read_csv("UN_HIV_Prevalence.csv")

#This code drops the "Value Footnotes" column because it does not contribute anything for my plotting.
#Filtered dataset using regex to just identify observations coming from either all countries or each UN AIDS Region.
#Remainder of filter functions are used to select the upper and lower modelled estimates for all adults aged 15-49.
#The mutate function is just to make the country or region name fit into the facets.

HIV_by_region=HIV_prev_df%>%
  rename(Country=`Country or Area`)%>%
  select(-c("Value Footnotes"))%>%
  filter(grepl(paste(c("countries","^UNAID"),collapse="|"),Country))%>%
  filter(grepl(paste(c("lower estimate modelled","upper estimate modelled"),collapse="|"),Subgroup))%>%
  filter(!str_detect(Subgroup,c("Young")))%>%
  mutate(Country_wrap=str_wrap(Country,width=15))


ggplot(data=HIV_by_region)+
  geom_point(mapping=aes(x=Year,y=Value))+
  geom_smooth(mapping=aes(x=Year,y=Value),color="red",se=FALSE)+
  facet_wrap(~Country_wrap,nrow=2)+
  theme(panel.spacing=unit(0.3,"lines"),axis.text.x=element_text(angle=90,hjust=1),plot.title=element_text(hjust=0.5))+
  labs(title=expression(paste("HIV Prevalence Among Adults (15-49) Shows Regional Trends")),
       x=expression(paste("Year (1990-2014)")),
       y=expression(paste("Prevalence (%)")),
       caption="Points plotted for each year represent the upper and lower modelled prevalence estimates from the UN data.")

ggsave('p2_HIV_Prevalence.png')

#This code drops the "Value Footnotes" column because it does not contribute anything for my plotting.
#Filtered dataset using regex to just identify observations coming from either all countries or each UN AIDS Region.
#Remainder of filter functions are used to select the upper and lower modelled estimates for adults aged 15-49, divided by sex.
#The mutate function is just to make the country or region name fit into the facets.

HIV_by_region_sex=HIV_prev_df%>%
  rename(Country=`Country or Area`)%>%
  select(-c("Value Footnotes"))%>%
  filter(grepl(paste(c("countries","^UNAID"),collapse="|"),Country))%>%
  filter(grepl(paste(c("^Females Adults","^Males Adults"),collapse="|"),Subgroup))%>%
  filter(!str_detect(Subgroup,c("Young")))%>%
  mutate(Country_wrap=str_wrap(Country,width=15))

#Used a for loop to create a Sex variable that is either Male or Female to make it easier to apply an aesthetic distinguishing the sexes.

HIV_by_region_sex$Sex<-vector("character",nrow(HIV_by_region_sex))
for (i in 1:nrow(HIV_by_region_sex)){
  HIV_by_region_sex$Sex[i]<-ifelse(str_detect(HIV_by_region_sex[i,2],c("Males")),"Male","Female")
}

ggplot(data=HIV_by_region_sex)+
  geom_point(mapping=aes(x=Year,y=Value,colour=Sex),alpha=0.2)+
  geom_smooth(mapping=aes(x=Year,y=Value,colour=Sex),alpha=0.7,se=FALSE)+
  facet_wrap(~Country_wrap,nrow=2)+
  theme(panel.spacing=unit(0.3,"lines"),axis.text.x=element_text(angle=90,hjust=1),plot.title=element_text(hjust=0.5))+
  labs(title=expression(paste("HIV Prevalence Among Adults (15-49) by Sex Shows Regional Trends")),
       x=expression(paste("Year (2000-2014)")),
       y=expression(paste("Prevalence (%)")),
       caption="Points plotted for each year represent the upper \n and lower modelled prevalence estimates from the UN data.")

ggsave('p2_HIV_Prevalence_Sex.png')

