library(plyr)

library (tidyverse)

library(stringr)

source("UN_AIDS_Prevalence_Incidence_Project_2.R")

#setwd("C:/Users/linra/Documents/UNC Masters Degree/Fall 2018/BIOS 611/Project 2")

HIV_prev_df=read_csv("UN_HIV_Prevalence.csv")

HIV_inc_df=read_csv("UN_HIV_Incidence.csv")

HIV_Circ_df=read_csv("UN_HIV_Circumcision.csv")

##This code drops the "Value Footnotes" column because it does not contribute anything for my plotting.
#Renaming of variables is done pre-emptively to prevent confusion on joining tables.
#The mutate function is just to make the country or region name fit into the facets.

HIV_Circ_df=HIV_Circ_df%>%
  rename(Country=`Country or Area`,Number=Value,Count=Unit)%>%
  select(-c("Value Footnotes"))%>%
  mutate(Country_wrap=str_wrap(Country,width=15))

#This code drops the "Value Footnotes" column because it does not contribute anything for my plotting.
#Renaming of variables is done pre-emptively to prevent confusion on joining tables.
#Filter functions are used to select modelled estimates for all adults aged 15-49.
#The mutate function is just to make the country or region name fit into the facets.

HIV_prev_df=HIV_prev_df%>%
  rename(Country=`Country or Area`,Prevalence=Value,Prev_Unit=Unit)%>%
  select(-c("Value Footnotes"))%>%
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
#Rename Subgroup variable to Key for easier joining.
#The mutate function is just to make the country or region name fit into the facets.

HIV_inc_df=HIV_inc_df%>%
  rename(Country=`Country or Area`,Incidence=Value,Inc_Unit=Unit)%>%
  select(-c("Value Footnotes"))%>%
  rename(Key=Subgroup)%>%
  mutate(Country_wrap=str_wrap(Country,width=15))

#Perform a left join and then filtered for the non-upper and non-lower modelled estimates

HIV_prev_inc=HIV_inc_df%>%
  left_join(HIV_prev_df,by=c("Country_wrap","Year","Key"))%>%
  filter(!grepl(paste(c("lower","upper"),collapse="|"),Key))

#Remove Ethiopia from dataset because it is not in the HIV prevalence or incidence data

HIV_Circ_Trans=HIV_Circ_df%>%
  left_join(HIV_prev_inc,by=c("Country_wrap","Year"))%>%
  filter(Country_wrap!="Ethiopia")

#Calculate transmission using incidence/prevalence

HIV_Circ_Trans$Transmission<-vector("numeric",nrow(HIV_Circ_Trans))
for (i in 1:nrow(HIV_Circ_Trans)){
  x=HIV_Circ_Trans$Incidence[i]
  y=HIV_Circ_Trans$Prevalence[i]
  HIV_Circ_Trans$Transmission[i]=Ratio_calc(x,y)
}

HIV_Circ_Trans$Country_wrap=as.factor(HIV_Circ_Trans$Country_wrap)

cor.test(HIV_Circ_Trans$Number,HIV_Circ_Trans$Transmission, alternative="t",method="spearman")->Circ_trans_spear

ggplot(data=HIV_Circ_Trans)+
  geom_point(mapping=aes(x=Number,y=Transmission,colour=as.factor(Year)))+
  facet_wrap(~Country_wrap,nrow=3)+
  theme(panel.spacing=unit(0.4,"lines"),axis.text.x=element_text(angle=90,hjust=1),plot.title=element_text(hjust=0.5))+
  labs(title=expression(paste("Circumcision Prevalence and HIV Transmission Rates Among Adults (15-49)")),
       x=expression(paste("Number of Circumcisions (Annual)")),
       y=expression(paste("Transmission Rate")),
       colour='Year',
       caption="The calculated value of the Spearman's rank correlation coefficient across all facets is -0.2637.")
 
ggsave('p2_HIV_Circum_Transmission.png')


cor.test(HIV_Circ_Trans$Number,HIV_Circ_Trans$Incidence, alternative="t",method="spearman")->Circ_inc_spear

ggplot(data=HIV_Circ_Trans)+
  geom_point(mapping=aes(x=Number,y=Incidence,colour=as.factor(Year)))+
  facet_wrap(~Country_wrap,nrow=3)+
  theme(panel.spacing=unit(0.4,"lines"),axis.text.x=element_text(angle=90,hjust=1),plot.title=element_text(hjust=0.5))+
  labs(title=expression(paste("Circumcision Prevalence and HIV Incidence Among Adults (15-49)")),
       x=expression(paste("Number of Circumcisions (Annual)")),
       y=expression(paste("Incidence Rate (%)")),
       colour="Year",
       caption="The calculated value of the Spearman's rank correlation coefficient across all facets is -0.3248.")

ggsave('p2_HIV_Circum_Incidence.png')
  



