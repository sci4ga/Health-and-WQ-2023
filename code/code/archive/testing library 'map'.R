install.packages("maps")
library(maps)

library(tidyverse)
library(readxl)
library(ggmap)
library('viridis')


GA<-map_data('state', 'georgia')
state<-map_data('state')

help(package='maps')



AAS<-read_excel("data/AASbact.xlsx")






Ecoli<-recode(AAS$ecoli_idexx,"NA"="0")%>%as.numeric()%>%log10()
lat<-AAS$site_latitude
long<-AAS$site_longitude

test<-data.frame(Ecoli, lat, long)%>%drop_na()


ggplot(data=test)+
  geom_polygon(data=GA, aes(x=long, y=lat))+
  stat_density2d(aes(fill=..level..,alpha=..level..), geom="polygon")

testdrop_na(test)



#First test#####
GA<-map_data("state", "georgia")


ggplot(data = test, aes(x = long, y = lat)) + 
  geom_point(data = test, aes(x = long, y = lat), 
                        color = "red", fill = "white") +
  stat_density2d(data=test,aes(fill = ..level.., alpha = ..level..), 
                          geom = "polygon") +
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(7, "Spectral")))+
  geom_polygon(data = state, aes(x = long, y = lat), color = "black", fill = "white") +
  theme(legend.position = "none")


#getting so close to this, just need the points to show up...

#Trying google maps approach####

#There is major setup to this approach, and requires using API keys
#Also appears to cost money... great

API<-'AIzaSyC3e4sGvj1_5l9Ghe3PUewpjp_BmHc89G0'
register_google(key=API)
GA<-get_map(c(left=-87, right=-80, bottom=30, top=35.5))
ggmap(GA)


Atl<-get_map("atlanta, georgia")

AASEcoliMapGA<-ggmap(GA)+
  geom_point(data=test, aes(x=long,y=lat, group=Ecoli, color=Ecoli), size=0.5)+
  scale_color_viridis(option="B")+
  stat_density2d(data=test, aes(fill=..level..,x=long, y=lat), geom="polygon", alpha=0.25)
#LOOK INTO GIS OUTPUTS

AASEcoliMapAtl<-ggmap(Atl)+
  geom_point(data=test, aes(x=long,y=lat, group=Ecoli, color=Ecoli), size=0.5)+
  scale_color_viridis(option="B")+
  stat_density2d(data=test, aes(fill=..level..,x=long, y=lat), geom="polygon", alpha=0.25)


#Testing to see if WQP is graphable...
wqp<-read.csv("data/wqpdump.csv")


wqp<- subset(wqp, select = c('OrganizationIdentifier', 'ActivityStartDate', 
                             'ActivityLocation.LatitudeMeasure', 
                             'ActivityLocation.LongitudeMeasure',
                             'CharacteristicName', 'ResultMeasureValue'))%>%
  mutate(TABLEID=row_number())%>%
  pivot_wider(names_from='CharacteristicName', 
              values_from ='ResultMeasureValue')

wqp<- subset(wqp, select = c('OrganizationIdentifier', 'ActivityStartDate', 
                             'ActivityLocation.LatitudeMeasure',
                             'ActivityLocation.LongitudeMeasure',
                             'Escherichia coli', 'Atrazine'))%>%
  rename("lat"="ActivityLocation.LatitudeMeasure", 
         "long"="ActivityLocation.LongitudeMeasure", 
         "Ecoli"="Escherichia coli")%>%
  mutate(Ecoli=log10(as.numeric(Ecoli)), Atrazine=as.numeric(Atrazine))


WQPEcoliMap<-ggmap(GA)+
  geom_point(data=wqp, aes(x=long,y=lat, group=Atrazine, color=Atrazine), size=0.5)+
  scale_color_viridis(option="B")+
  stat_density2d(data=wqp, aes(fill=..level..,x=long, y=lat), geom="polygon", alpha=0.25)

WQPEcoliMap

#something in the code is screwed up. Color isn't being applied to the points. Very cool stuff though
#You can see large amounts of Atrazine at the end of the Savannah River.


#Ecoli data should be heat mapped based on their current "level"... Reflecting density of higher levels in the US
