library(rsconnect)
library(shiny)
library(rgdal)
library(leaflet)
library(DT)
library(raster)
library(rgeos)
library(ggplot2)
library(tidyverse)

#Load data sets
schoolsSpatial <- readOGR(dsn = "C:/Users/kost1/Documents/GitHub/Data-Viz-2018-Fall/FinalProject", layer = "School_Boundaries")

parksPoints <- read.csv("C:/Users/kost1/Documents/GitHub/Data-Viz-2018-Fall/FinalProject/Parks_Locations_and_Features.csv", stringsAsFactors = F)

parksSpatial <- SpatialPointsDataFrame(coords = parksPoints[,c("Lon","Lat")], data = parksPoints,
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))

AbandonedPropertyParcels <- readOGR(dsn = ".C:/Users/kost1/Documents/GitHub/Data-Viz-2018-Fall/FinalProject/data/Abandoned_Property_Parcels/", layer = "Abandoned_Property_Parcels", stringsAsFactors = FALSE)
CityCouncilDistricts <- readOGR(dsn = "C:/Users/kost1/Documents/GitHub/Data-Viz-2018-Fall/FinalProject/data/City_Council_Districts/", layer = "City_Council_Districts", stringsAsFactors = FALSE)

AbandonedPropertyParcels.center <- SpatialPointsDataFrame(gCentroid(AbandonedPropertyParcels, byid=TRUE), 
                                                          AbandonedPropertyParcels@data, match.ID=FALSE)

#Create pop ups
parksSpatial$popup <- paste("<b>",parksSpatial$Park_Name,"</b><br>",
                            "Type: ",parksSpatial$Park_Type,"<br>",
                            "Address: ",parksSpatial$Address,sep ="")

AbandonedPropertyParcels$popup <- paste("<b>",AbandonedPropertyParcels$Outcome_St,"</b><br>",
                            "Address: ", AbandonedPropertyParcels$Address_Nu, AbandonedPropertyParcels$Street_Nam,AbandonedPropertyParcels$Suffix, "<br>", 
                            "Structure Type: ", AbandonedPropertyParcels$Structures, "<br>", 
                            "District: ", AbandonedPropertyParcels$Council_Di)

CityCouncilDistricts$popupCouncils <- paste("<b>","District: ", CityCouncilDistricts$OBJECTID, "</b><br>",
                                            "Council Member: ", CityCouncilDistricts$Council_Me)

#Set color palettes
pal <- colorFactor(palette = 'Set1', domain = parksSpatial$Park_Type)
pal_A <- colorFactor(palette = "Accent", domain =AbandonedPropertyParcels$Outcome_St)


#Create UI
ui <- fluidPage(
  
  # Application title
  navbarPage("South Bend Facilities", id = "navFacilities",
             
             tabPanel("Schools and Parks",
                      
                      titlePanel("Map of Facilities"),
                      
                      # Show a plot of the generated distribution
                      br(),
                      leafletOutput("schoolsParksPlot", height = 600, width = "80%")
             ),
             tabPanel("Doug"
             ),
             
             #START ASHLEY
             tabPanel("Abandoned Properties",
                      titlePanel("Abandoned Properties"),
                      "This tab shows the abandoned properties in South Bend. The map shows the city of South Bend 
                      with each of the Districts plotted and highlighted in gray. When clicking on a gray area, a
                      popup will appear with the district number and the district's council member. Additionally, 
                      each abandoned property's property lines are mapped and are color-coded according to their
                      outcome status: Deconstructed, Demolished, Occupied & Not Repaired, Repaired, and 
                      Repaired & Occupied. When clicking on a property, a popup will appear with more information
                      about each property.",
                      br(),br(),
                      "The bar graph below the map shows the count of each property's status broken out by district. This
                      is a quick way to compare the number of abandoned properties in each district. This graph is also
                      color-coded the same way that the map is.",
                      br(),br(),
                      tabsetPanel(
                        #show a map of the abandoned properties
                        tabPanel("Map", h3("Map of Abandoned Properties in South Bend"), leafletOutput("AbandonedPropertyParcels", height = 600, width = "90%")), 
                        #show a bar graph of the abandoned properties by district
                        tabPanel("Bar Graph", h3("Bar Graph of Abandoned Properties by District"), plotOutput("plot_abandonedbydistrict", click = "plot_click", height = 600, width = "90%"))
                      )
             ),
             #END ASHLEY
             
             tabPanel("Udai"
             ),
             tabPanel("Sam"
             ),
             tabPanel("Contact Us",
                      textInput("firstContact", "First Name:"),
                      textInput("lastContact", "Last Name:"),
                      textInput("phoneContact", "Phone Number:"),
                      textInput("emailContact", "Email Address:"),
                      textInput("subjectContact", "Email Subject:"),
                      textAreaInput("messageContact", "Enter your message"),
                      actionButton("sendEmail", "Send Email")
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$schoolsParksPlot <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = parksSpatial, popup = ~popup, color=~pal(Park_Type), stroke = 0, fillOpacity=1, radius=4) %>%
      addLegend(pal = pal, values = parksSpatial$Park_Type) %>%
      addPolygons(data = schoolsSpatial, color = "blue", weight = 2, opacity = 0.6, popup = ~School)  
  })
  
  #create leaflet map - ASHLEY
  output$AbandonedPropertyParcels <-
      renderLeaflet({
      leaflet()  %>%
      addTiles()  %>%
      addProviderTiles(providers$OpenStreetMap) %>% #sets map design
      addPolygons(data = CityCouncilDistricts, popup = ~popupCouncils, fillColor = ~pal_A(Dist), #creates district lines
                  weight = 2,
                  opacity = 1,
                  color = "white") %>% 
      addPolylines(data = AbandonedPropertyParcels, popup = ~popup, color = ~pal_A(Outcome_St), fillOpacity = 1)
  })
  
  #create bar graph - ASHLEY
  output$plot_abandonedbydistrict <- renderPlot({
    ggplot(data = (AbandonedPropertyParcels@data), aes(fill = (AbandonedPropertyParcels@data)$Outcome_St, #color by outcome
                                                       x = (AbandonedPropertyParcels@data)$Council_Di)) + 
      geom_bar() + #make a bar graph
      labs(fill = "Outcome", x = "District", y = "Count") + #rename the axis and legend
      theme_bw() + #make a clean theme
      scale_fill_brewer(palette="Accent", na.value="grey") #same palette as palette for map
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
