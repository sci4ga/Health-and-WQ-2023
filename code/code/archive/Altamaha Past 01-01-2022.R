#Title: Lower Ocmulgee Past 01-01-2022
#Date: 1/24/2023
#Purpose: Transform data set and create visuals

#Part 1 - Load libraries
library(tidyverse)
library(readxl)
library(naniar)


Altamaha<-read_excel("data/Altamaha Water Data Past 01-01-2021.xlsx")


Altamaha<- subset(Altamaha, select = c('Project Name', 'Sampling Intent', 'Submedia', 
                                       'Monitoring Location ID', 'Monitoring Location', 
                                       'Date', 'Parameter', 'Result (Value)', 'Units', 
                                       'Detection Limit', 'Detection Limit Unit'))%>%
  drop_na('Result (Value)')%>%
  mutate(TABLEID=row_number())%>% 
  pivot_wider(names_from='Parameter', values_from ='Result (Value)', 
              id_cols=c('TABLEID', 'Monitoring Location ID', 'Date'))%>%
  subset(select=c('TABLEID', 'Monitoring Location ID', 'Date', 'Dissolved oxygen (DO)', 
                  'Dissolved oxygen saturation', 'Biochemical oxygen demand, standard conditions', 
                  'Phosphorus', 'Total suspended solids', 'Ammonia-nitrogen', 'Sulfate', 
                  'Inorganic nitrogen (nitrate and nitrite)', 'Escherichia coli'))%>%
  mutate(`Monitoring Location ID`=as.factor(`Monitoring Location ID`))%>%
  drop_na(`Escherichia coli`)
#CHANGE DROP_NA TO WHATEVER VARIABLE YOU WANT THEN PROCEED.



AltamahaECdata<-recode(Altamaha$`Escherichia coli`,"NA"="0")%>%as.numeric()%>%log10()


ggplot(Altamaha, x=`Monitoring Location ID`)+
  geom_point(aes(x=`Monitoring Location ID`, y=AltamahaECdata))+
  labs(y="Ecoli Levels Log10 Scale (MFN/100mL)")+
  geom_hline(yintercept=c(2.1,2.8), color=c("red", "blue"), alpha=0.25)


AltamahaBCdata<-recode(Altamaha$`Biochemical oxygen demand, standard conditions`,"NA"="0")%>%as.numeric()%>%log10()
Altamaha<-Altamaha%>%mutate(`Monitoring Location ID`=as.factor(`Monitoring Location ID`))

ggplot(Altamaha, x=`Monitoring Location ID`)+
  geom_point(aes(x=`Monitoring Location ID`, y=AltamahaBCdata))+
  labs(y="Biochemical oxygen demand")

#Look into doing Monitoring Location ID's. Will need to do average MFN/100 mL counts as there are multiple data points for single monitoring locations.
#This could show the worst polluters or polluted sites over time. To do monitoring locations will likely need to set MLID's to factor data type.






