library(plyr)

library (tidyverse)

library(stringr)

#setwd("C:/Users/linra/Documents/UNC Masters Degree/Fall 2018/BIOS 611/Project 2")

HIV_ART_df=read_csv("UN_ART_Coverage.csv")

HIV_PMTCT_df=read_csv("UN_PMTCT_Infection.csv")

##This code drops the "Value Footnotes" column because it does not contribute anything for my plotting.
#Renaming of variables is done pre-emptively to prevent confusion on joining tables.
#Filtered dataset using regex to just identify observations coming from either all countries or each UN AIDS Region.
#Further filtered to just get total estimates for each region
#The mutate function is just to make the country or region name fit into the facets.

HIV_ART_df=HIV_ART_df%>%
  rename(Country=`Country or Area`,ART_Percentage=Value,ART_Unit=Unit)%>%
  select(-c("Value Footnotes"))%>%
  filter(grepl(paste(c("countries","^UNAID"),collapse="|"),Country))%>%
  filter(grepl("^Total",Subgroup))%>%
  mutate(Country_wrap=str_wrap(Country,width=15))%>%
  rename(Key=Subgroup)

##This code drops the "Value Footnotes" column because it does not contribute anything for my plotting.
#Renaming of variables is done pre-emptively to prevent confusion on joining tables.
#Filtered dataset using regex to just identify observations coming from either all countries or each UN AIDS Region.
#The mutate function is just to make the country or region name fit into the facets.

HIV_PMTCT_df=HIV_PMTCT_df%>%
  rename(Country=`Country or Area`,PMTCT_Number=Value,Infections_Prevented=Unit)%>%
  select(-c("Value Footnotes"))%>%
  filter(grepl(paste(c("countries","^UNAID"),collapse="|"),Country))%>%
  mutate(Country_wrap=str_wrap(Country,width=20))

#Added a Key variable for easier table joins (same value as Subgroup variable, but with the word "Total" appended to the front)

HIV_PMTCT_df$Key<-vector("character",nrow(HIV_PMTCT_df))
for (i in 1:nrow(HIV_PMTCT_df)){
  HIV_PMTCT_df$Key[i]<-str_c("Total",HIV_PMTCT_df$Subgroup[i],sep=" ")
}

#Performed a inner join

HIV_PMTCT_Art=HIV_PMTCT_df%>%
  inner_join(HIV_ART_df,by=c("Key","Country_wrap","Year"))

ggplot(data=HIV_PMTCT_Art)+
  geom_point(mapping=aes(x=ART_Percentage,y=PMTCT_Number,colour=as.factor(Year)))+
  geom_smooth(mapping=aes(x=ART_Percentage,y=PMTCT_Number),colour="brown",se=FALSE)+
  facet_wrap(~Country_wrap,nrow=3)+
  theme(panel.spacing=unit(0.4,"lines"),axis.text.x=element_text(angle=90,hjust=1),plot.title=element_text(hjust=0.5))+
  labs(title=expression(paste("Increasing ART Coverage Has Reduced Mother-to-Child Transmission")),
       x=expression(paste("Antiretroviral Therapy (ART) Coverage Among People with HIV (%)")),
       y=expression(paste("Infections Averted by Prevention of Mother-to-Child Transmission (PMTCT)")),
       colour="Year")+
  scale_y_continuous(labels=scales::comma)

ggsave('p2_HIV_ART_PMTCT.png')
