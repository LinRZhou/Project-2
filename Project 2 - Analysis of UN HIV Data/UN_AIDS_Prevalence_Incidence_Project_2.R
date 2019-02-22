library(plyr)

library (tidyverse)

library(stringr)

#setwd("C:/Users/linra/Documents/UNC Masters Degree/Fall 2018/BIOS 611/Project 2")

HIV_prev_df=read_csv("UN_HIV_Prevalence.csv")

HIV_inc_df=read_csv("UN_HIV_Incidence.csv")

#This code drops the "Value Footnotes" column because it does not contribute anything for my plotting.
#Renaming of variables is done pre-emptively to prevent confusion on joining tables.
#Filtered dataset using regex to just identify observations coming from either all countries or each UN AIDS Region.
#Remainder of filter functions are used to select the upper and lower modelled estimates for all adults aged 15-49.
#The mutate function is just to make the country or region name fit into the facets.

HIV_prev_df=HIV_prev_df%>%
  rename(Country=`Country or Area`,Prevalence=Value,Prev_Unit=Unit)%>%
  select(-c("Value Footnotes"))%>%
  filter(grepl(paste(c("countries","^UNAID"),collapse="|"),Country))%>%
  filter(!grepl(paste(c("^Males","^Females"),collapse="|"),Subgroup))%>%
  mutate(Country_wrap=str_wrap(Country,width=15))

#Made a Key variable that contains the same string(s) as the Subgroup variable, but the "modelled" and its preceding whitespace are removed.
#This makes it much easier to join the two tables (Prevalence data and Incidence data).

HIV_prev_df$Key<-vector("character",nrow(HIV_prev_df))
for (i in 1:nrow(HIV_prev_df)){
  HIV_prev_df$Key[i]<-str_sub(HIV_prev_df$Subgroup[i],end=-10)
}

#This code drops the "Value Footnotes" column because it does not contribute anything for my plotting.
#Renaming of variables is done pre-emptively to prevent confusion on joining tables.
#Filtered dataset using regex to just identify observations coming from either all countries or each UN AIDS Region.
#Rename Subgroup variable to Key for easier joining.
#The mutate function is just to make the country or region name fit into the facets.

HIV_inc_df=HIV_inc_df%>%
  rename(Country=`Country or Area`,Incidence=Value,Inc_Unit=Unit)%>%
  select(-c("Value Footnotes"))%>%
  filter(grepl(paste(c("countries","^UNAID"),collapse="|"),Country))%>%
  rename(Key=Subgroup)%>%
  mutate(Country_wrap=str_wrap(Country,width=15))

#Perform a left join and then filtered for the upper and lower modelled estimates

HIV_prev_inc=HIV_inc_df%>%
  left_join(HIV_prev_df,by=c("Country_wrap","Year","Key"))%>%
  filter(grepl(paste(c("lower","upper"),collapse="|"),Key))

#Function to calculate transmission rates (incidence/prevalence)

Ratio_calc<-function(x,y){
  Ratio=x/y
  return(Ratio)
}

HIV_prev_inc$Transmission<-vector("numeric",nrow(HIV_prev_inc))
for (i in 1:nrow(HIV_prev_inc)){
  x=HIV_prev_inc$Incidence[i]
  y=HIV_prev_inc$Prevalence[i]
  HIV_prev_inc$Transmission[i]=Ratio_calc(x,y)
}

ggplot(data=HIV_prev_inc)+
  geom_point(mapping=aes(x=Year,y=Incidence),alpha=0.2)+
  geom_smooth(mapping=aes(x=Year,y=Incidence),alpha=0.7,se=FALSE)+
  facet_wrap(~Country_wrap,nrow=2)+
  theme(panel.spacing=unit(0.4,"lines"),axis.text.x=element_text(angle=90,hjust=1),plot.title=element_text(hjust=0.5))+
  labs(title=expression(paste("HIV Incidence Rates Among Adults (15-49) Are Decreasing Globally")),
       x=expression(paste("Year (1990-2014)")),
       y=expression(paste("Incidence Rate (%)")),
       caption="Points plotted for each year represent the upper and lower modelled incidence estimates from the UN data.")

ggsave('p2_HIV_Incidence.png')

ggplot(data=HIV_prev_inc)+
  geom_point(mapping=aes(x=Year,y=Transmission),alpha=0.2)+
  geom_smooth(mapping=aes(x=Year,y=Transmission),alpha=0.7,se=FALSE)+
  facet_wrap(~Country_wrap,nrow=2)+
  theme(panel.spacing=unit(0.4,"lines"),axis.text.x=element_text(angle=90,hjust=1),plot.title=element_text(hjust=0.5))+
  labs(title=expression(paste("HIV Transmission Rates Among Adults (15-49) Are Decreasing Globally")),
       x=expression(paste("Year (1990-2014)")),
       y=expression(paste("Transmission Rate (%)")),
       caption="Points plotted for each year represent the upper and lower modelled transmission estimates from the UN data.")

ggsave('p2_HIV_Transmission.png')
