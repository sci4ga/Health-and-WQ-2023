
# Prior leg work  --------------------------------------------------------------------

#install.packages(c("tidyverse", "shiny", "leaflet", "RColorBrewer", "tigris", "readxl")) #<-code to download all the packages if needed (just remove the first # at the begining of the code)

library(tidyverse)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(tigris)
library(readxl)

setwd("C:/Users/dlwhi/Desktop/UGASpringInternship2023RProject") #<- change the working directory as needed. All read-ins of data follow the format "data/...", so you likely just need to change the parts before "/UGASpringInternshipRProject".


  #Downloading water quality portal data
wqp<-read_csv("data/waterqualitydatafinal.csv")             #<-precleaned dataset. To see how I cleaned the data, see the coforcleaningdata.R file.

outcomes<-c("All-Cause Age-Adjusted Death Rate (2012-2021)",         #<- putting outcome names into a list so it doesn't clog the UI code
            "Cancer Age-Adjusted Death Rate (2012-2021)",	
            "Respiratory Diseases Age-Adjusted Death Rate (2012-2021)",
            "Major Cardiovascular Diseases Age-Adjusted Death Rate (2012-2021)",
            "Nervous System Diseases Age-Adjusted Death Rate (2012-2021)",
            "Mental and Behavioral Disorders Age-Adjusted Death Rate (2012-2021)",	
            "Endocrine, Nutritonal, and Metabolic Diseases Age-Adjusted Death Rate (2012-2021)",
            "Digestive System Diseases (Non-Alcoholic Liver Disease) Age-Adjusted Death Rate (2012-2021)",	
            "Reproductive and Urinary System Diseases Age-Adjusted Death Rate (2012-2021)",
            "Alzheimer's Disease and Related Dementia Age-Adjusted Death Rate (2012-2021)",
            "Infant Mortality Rate (2012-2021)",
            "Fetal Mortality Rate (2012-2021)",
            "Birth Defect Age-Adjusted Death Rate (2012-2021)",
            "Age-Adjusted Prevelance (%) of Arthritis for Adults >=18 years (2020)",
            "Age-Adjusted Prevalence (%) of Current Asthma for Adults >=18 years (2020)",
            "Age-Adjusted Prevalence (%) of Stroke for Adults >=18 years (2020)",
            "Age-Adjusted Prevalence (%) of Physical health not good for >=14 days for Adults >=18 years (2020)",
            "Age-Adjusted Prevalence (%) of Depression for Adults >=18 years (2020)")


#Downloading healthoutcome data, rural map, and combining the two. 

healthdata <- read_excel("data/HealthOutcomeData.xlsx", na="*")       #<-downloading health outcome data

countymap<-counties(state="GA", class="sf")                                         #<- creating county map of georgia with tigris package

healthcountymap<-inner_join(countymap, healthdata, by="NAME")%>%                    #<- combining health outcome data and rural map
  sf::st_transform('+proj=longlat +datum=WGS84')%>%                        #<- making the map data the proper format
  pivot_longer(cols=outcomes, names_to="Outcome", values_to="Value")      #<- fixing map to be more usable (creating single columns for health outcomes and values)
 
#UI--------------------------------------------------------------------
ui <- fluidPage(
  #App title----
  titlePanel("Water Quality & Health Outcome Map (2012-2021)"),
  #sidebar layout w/ input & output----
  sidebarLayout(
    #Sidebar panel----
    sidebarPanel(
      #Inputs: 
      dateRangeInput(inputId="Date", "Select date range:", #<- This is a basic date range filter.
                     start=as.Date("2017-01-01"),
                     end=as.Date("2021-12-31"), format="yyyy-mm-dd"),
      selectInput(inputId="Variable",                                      #<- This is a basic input filter based on WQP characteristics
                  label="Water Quality Parameters",
                  choices=wqp$Characteristic),
      selectInput(inputId="MapLayer",                                      #<- This is a basic input filter based on Health data 
                  label="Health Outcome Maps",
                  choices=outcomes),
      h4("Note that the color of a circle marker does not indicate risk. Evaluate each marker individually."),
      h4("Water quality data is available through the Water Quality Portal, and health outcome data is available through the CDC's PLACES dataset and the Georgia Department of Public Health OASIS dataset.")
      ),
  mainPanel(
    h4("Please wait patiently while each map is loading."),
  leafletOutput("Map", width = "100%", height="600px")
  )))
#Server----------------------------------------------------------------

server <- function(input, output, session) {
  
  #Building the initial map of Georgia####
  output$Map <- renderLeaflet({
    leaflet() %>% 
      addTiles(urlTemplate='https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}{r}.png')%>%     #<- This changes what topographical map is used for the leaflet map
      fitBounds(-86, 30, -81, 35)%>%                                                                    #<- Fitbounds is used to make the map boundaries roughly around the state of GA.
      addMapPane("mapcircle", zIndex=440)%>% #<-custom map layers so the county polygon map doesn't overlay the circle markers and prevents them from being clicked
      addMapPane("mappolygon", zIndex=430) 
  })
 
  #reactive filter for dateInput and selectInputs####
  selectfilter<-reactive({                                           #<- This is a reactive filter for the input filter above. It basically creates a function that will update when an input is given.
        wqp%>%filter(`Characteristic` == input$Variable) 
                        })
  
  fullfilter<-reactive({                                             #<- Here I nested the first reactive filter (for characteristic input) into another filter for the date range input.
    selectfilter()%>%filter(`ActivityStartDate` >= input$Date[1] & ActivityStartDate <= input$Date[2])
  })
  
  healthfilter<-reactive({
    healthcountymap%>%filter(`Outcome` == input$MapLayer)          #<- Separate reactive filter for health outcome layer
  })
  
  #observer (used for reactive dataset)####
  observe({                                                         #<- This is an observer. It is necessary in order to use the reactive filters I created above.
    
    #Reactive color palette for water quality and health outcome data
    circlepal<-colorNumeric(palette=brewer.pal(8, "Set1"), domain=fullfilter()$Result)   #<- I used a diverging palette here. Each charactersitic can have a huge range, thus making a normal sequential palette useless.
    polypal<-colorNumeric(palette=brewer.pal(6,"YlOrBr"), domain=healthfilter()$Value)   #<- I used a sequential palette here, since the ranges of the data are not that large
    
    
    #creating popups
    circlecontent<-paste0("Water Quality Summary", "<br>",                    #<- This is the outline for a basic popup (used in leafletProxy()). It's written partially in HTML ("<br>"). Using the reactive filter above, each marker will be updated to display data once clicked.
                    "Date: ", fullfilter()$ActivityStartDate, "<br>",
                    "Characteristic: ", fullfilter()$Characteristic, "<br>",
                    "Result: ", fullfilter()$Result, " ", fullfilter()$`ResultMeasure/MeasureUnitCode`, "<br>",
                    "Latitude: ", fullfilter()$lat, "<br>",
                    "Longitude: ", fullfilter()$long, "<br>")
    
    polycontent<-paste0(healthfilter()$NAME, " County", "<br>", "<br>",     #<- popup for each county
                        "Outcome: ", healthfilter()$Outcome, "<br>",
                        "Rate: ", healthfilter()$Value)
    
    #Proxy map for data
    leafletProxy("Map", data=fullfilter())%>%                    #<- This is a proxy for the base leaflet map I created above. Using a proxy removes the need to rebuild the base map each time the characteristic and date inputs are changed.
      clearShapes()%>%                                             
      addCircles(lng=~long,lat=~lat,                              #<- This creates circle markers for each characteristic.
                 weight=5, 
                 color=~circlepal(Result), 
                 popup=circlecontent,
                 opacity=1,
                 options=pathOptions(pane="mapcircle"))%>%
      addPolygons(data=healthfilter(), options=pathOptions(pane="mappolygon"),        #<- This creates an outline of all the counties in GA.
                   opacity=0.5, weight= 1, color="Black", 
                   fillOpacity = .5, fill=T, 
                   fillColor=~polypal(Value),
                  popup = polycontent)%>%
      clearControls()%>%                                  #<-clearControls() resets the polygon map legend in between each input selection. If this wasn't here the legends would not be removed between selections and would fill the entire map...
      addLegend(pal=polypal, values=~healthfilter()$Value,    #<- Basic reactive legend
                position="bottomright", 
                title = "Health Outcome Legend")%>%
      addLegend(pal=circlepal, values=~fullfilter()$Result, 
                position="topright", title="Water Parameter Legend")
  })
  
}


  

#Shiny app--------------------------------------------------

shinyApp(ui = ui, server = server)
