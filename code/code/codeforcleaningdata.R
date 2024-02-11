


#This is the code I used to initially clean the full water quality portal dataset (wqpfinal2017-2021.csv), before creating a new separate file containing only the clean data (waterqualitydatafinal.csv)
#You can find the old data set in data/archive.

library(tidyverse)
library(tigris)
library(readxl)

setwd("C:/Users/dlwhi/Desktop/UGASpringInternship2023RProject") #<- change this as needed


#Downloading & cleaning water quality portal data.------------------------------------- 
wqp<-read_csv("data/wqpfinal2017-2021.csv")


wqpclean<- subset(wqp, select = c('OrganizationIdentifier', 'ActivityStartDate', 
                                  'ActivityLocation/LatitudeMeasure', 
                                  'ActivityLocation/LongitudeMeasure',
                                  'CharacteristicName', 'ResultMeasureValue',                    #<- all this can likely be saved to a single csv file, then just uploaded as needed
                                  'ResultMeasure/MeasureUnitCode'))%>%
  mutate(TABLEID=row_number())%>%rename("lat"='ActivityLocation/LatitudeMeasure',
                                        "long"='ActivityLocation/LongitudeMeasure',
                                        "Characteristic"='CharacteristicName', "Result"='ResultMeasureValue')%>%
  mutate(Result=as.numeric(Result))%>%
  drop_na()





wqpclean<-filter(wqpclean, `ResultMeasure/MeasureUnitCode` %in% c("deg C", "mm/Hg", "ft3/s", "count", "ft", "uS/cm @25C", "mg/l", "% saturatn",   "std units", "m", "m3/sec", "MPN/100 ml", "FNU",         #<- The E. coli measurements aren't uniform, and have a bunch of different reportable values. This filtered the Unit column so only 1 unit will be displayed... (loss of ~12k data, but better uniformity)
                                                                  "mg/l CaCO3",   "mg/l as N",    "mg/l as P",    "mg/l NO3",     "NTU",          "mg/l asPO4",   "mg/l NH4",    
                                                                  "None",         "%",            "ug/l",         "units/cm",     "L/mgDOC*m",    "NTRU",         "ng/l",        
                                                                  "tons/ac ft",   "mg/l asNO3",   "mg/l asNO2",   "tons/day",     "mph",          "g/m2",         "mg/m2",       
                                                                  "deg F",        "gal/min",      "pCi/L",        "cp/100 mL",    "g",            "uE/m2/sec",    "RFU",         
                                                                  "pct modern",   "cm3/g STP",    "ratio",        "per mil",      "cm3/g @STP",   "mg/kg",        "ug/kg",       
                                                                  "g/kg",         "seconds",      "mg/L",         "umho/cm",      "g/L",          "ug/L",         "NTMU",        
                                                                  "mg/l CaCO3**",         "um3/mL",       "uS/cm",        "mmHg",         NA,             "MPN/100mL",   
                                                                  "mg/m3",        "mg N/l******", "#/mL",         "ft3/sec",      "mg",           "years",       
                                                                  "PCU",          "ueq/L",        "cm",           "mm",           "MSC",          "L/sec",            
                                                                  "ppth",         "ft2",          "ppb",          "ug/cm2",       "cfs",          "tsc/100mL",    "geu/100mL",   
                                                                  "ng/L",         "ppm",          "ppt"))



wqpclean<-filter(wqpclean, `Characteristic` #<- IMPORTANT. IF YOU WANT TO ADD A NEW CHARACTERISTIC TO THE APP, YOU NEED TO ADD IT INTO THE LIST, THEN CREATE A NEW DATA SET USING THE CODE AT THE BOTTOM (REMOVE THE # FIRST).
                 %in% c('Escherichia coli','Fecal Coliform',
                        'Iron', 'Arsenic', 'Lead', 'Copper', 
                        'Biochemical oxygen demand, standard conditions', 'Dissolved oxygen (DO)',
                        'Total Kjeldahl nitrogen (Organic N & NH3)', 'Kjeldahl nitrogen', 'pH', 'Chloride', 'Phosphorus', 'Carbon', 'Nitrogen', 'Mercury', 'Cadmium', 'Diazinon', 'Acephate', 'Carbofuran', 'Carbaryl'
                 ))%>%drop_na(lat)%>%drop_na(long)%>%filter(Result != 30.800000)

#write_csv(wqpclean, "waterqualitydatafinal.csv") <- code used to create final filtered dataset



