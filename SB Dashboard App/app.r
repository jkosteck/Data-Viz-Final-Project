library(rsconnect)
library(shiny)
library(rgdal)
library(leaflet)
library(DT)
library(raster)
library(rgeos)
library(ggplot2)
library(tidyverse)
library(rJava)
library(mailR)
library(shinyAce)

########### Load data sets ###########
#School data
schoolsSpatial <- readOGR(dsn = "C:/Users/kost1/Documents/GitHub/data-Viz-Final-Project/data/School_Boundaries",
                          layer = "School_Boundaries")

#Park data
parksPoints <- read.csv("C:/Users/kost1/Documents/GitHub/data-Viz-Final-Project/data/Parks_Locations_and_Features.csv",
                        stringsAsFactors = F)

#Abandoned Property data
AbandonedPropertyParcels <- readOGR(dsn = "C:/Users/kost1/Documents/GitHub/data-Viz-Final-Project/data/Abandoned_Property_Parcels",
                                    layer = "Abandoned_Property_Parcels",
                                    stringsAsFactors = FALSE)

#City Council data
CityCouncilDistricts <- readOGR(dsn = "C:/Users/kost1/Documents/GitHub/Data-Viz-Final-Project/Data/City_Council_Districts",
                                layer = "City_Council_Districts",
                                stringsAsFactors = FALSE)

#Cenus data
census <- readOGR(dsn="C:/Users/kost1/Documents/GitHub/Data-Viz-Final-Project/Data/2010_Cenusdata",
                  layer ='2010_CensusData',
                  stringsAsFactors = FALSE)

parksSpatial <- SpatialPointsDataFrame(coords = parksPoints[,c("Lon","Lat")], data = parksPoints,
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))


AbandonedPropertyParcels.center <- SpatialPointsDataFrame(gCentroid(AbandonedPropertyParcels, byid=TRUE), 
                                                          AbandonedPropertyParcels@data, match.ID=FALSE)

public_facilities <- read.csv('C:/Users/kost1/Documents/GitHub/data-Viz-Final-Project/data/Public_Facilities.csv')
facilities.spatial <- SpatialPointsDataFrame(coords = public_facilities[,c("Lon","Lat")], data = public_facilities, proj4string = CRS("+proj=longlat +datum=WGS84"))


street_lights <- read.csv("C:/Users/kost1/Documents/GitHub/data-Viz-Final-Project/data/Street_Lights.csv", stringsAsFactors = F)
street_lights[street_lights$Pole_Type %in% c(""," "),]$Pole_Type <- "Unknown"
street_lights[street_lights$Service %in% c(""," "),]$Service <- "Unknown"
street_lights$Inspect_Date2 <- as.Date(street_lights$Inspect_Date)

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
palParks <- colorFactor(palette = 'Set1', domain = parksSpatial$Park_Type)
palProperties <- colorFactor(palette = "Accent", domain =AbandonedPropertyParcels$Outcome_St)
palCenus <- colorNumeric(palette = "RdYlBu", domain = census$SE_T002_01)

facilitiesCheckboxChoices <- unique(facilities.spatial@data$POPL_TYPE)
facilitiesCheckboxSelected <- unique(facilities.spatial@data$POPL_TYPE)

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
             
             tabPanel("Street Lights",
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput(inputId = "dates",
                                         label = "Date range",
                                         startview = "year",
                                         start="2010-01-01"),
                          selectInput(inputId = "variable",
                                      label = "Variable:",
                                      choices = c("Pole_Type","Service"))
                        ),#end sidebarPanel
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Map",
                                     leafletOutput(outputId = "map"))
                          )#end TabsetPanel          
                        )#end mainPanel
                      )#end sidebarLayout
             ),
             
             tabPanel("Map of Public Facilities and Census data", 
                      "This tab shows a map of public facilities and census data in the Notre Dame Area. The intent of this analysis is to enable one to formulate a strategy on selecting
             optimal locations for building public facilities in the future, as public facilities should be built in areas of high populations as the utilization rate should be
             higher. 
             When clicking on a colored area on the map, the name of the region will appear. Additionally, when clicking on a marker, the name of the type of facility will appear.
             The areas are colored by total population, therefore the area with the highest population will be colored blue, and the lowest population will be colored red (legend)
             shown to the bottom left. Lastly, one can select to only show specific facility types by selecting types at the top.",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("Type",
                                             "Choose Facility Type",
                                             choices = facilitiesCheckboxChoices,
                                             selected = facilitiesCheckboxSelected)
                        ),
                        mainPanel()
                      )
             ),
             tabPanel("Contact Us",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("firstContact", "First Name:"),
                          textInput("lastContact", "Last Name:"),
                          textInput("phoneContact", "Phone Number:"),
                          textInput("emailContact", "Email Address:"),
                          textInput("subjectContact", "Email Subject:"),
                          actionButton("sendEmail", "Send Email")
                        ),
                        mainPanel(
                          "Enter your message below",
                          aceEditor("messageContact")
                        )
                      )
             )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$schoolsParksPlot <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = parksSpatial, popup = ~popup, color=~palParks(Park_Type), stroke = 0, fillOpacity=1, radius=4) %>%
      addLegend(pal = palParks, values = parksSpatial$Park_Type) %>%
      addPolygons(data = schoolsSpatial, color = "blue", weight = 2, opacity = 0.6, popup = ~School)  
  })
  
  #create leaflet map - ASHLEY
  output$AbandonedPropertyParcels <- renderLeaflet({
    leaflet()  %>%
      addTiles()  %>%
      addProviderTiles(providers$OpenStreetMap) %>% #sets map design
      addPolygons(data = CityCouncilDistricts, popup = ~popupCouncils, fillColor = ~palProperties(Dist), #creates district lines
                  weight = 2,
                  opacity = 1,
                  color = "white") %>% 
      addPolylines(data = AbandonedPropertyParcels, popup = ~popup, color = ~palProperties(Outcome_St), fillOpacity = 1)
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
  
  output$Map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = public_facilities[public_facilities$POPL_TYPE %in% input$Type,], 
                 popup = paste("Facility Type:",public_facilities$POPL_TYPE)) %>%
      addPolygons(data = census,popup = paste("Area Name:",census$NAMELSAD), color = ~palCenus(SE_T002_01)) %>%
      addLegend(data = census, position = "bottomright", pal = palCenus, 
                values = ~SE_T002_01, title = "Total Population", opacity = 1)  
    
  })
  
  streetLights <- eventReactive(input$dates,{
    return(street_lights[street_lights$Inspect_Date2 >= input$dates[1]& street_lights$Inspect_Date2 <= input$dates[2],])
  })
  
  output$map <-  renderLeaflet({
    leaflet(data = streetLights())%>%
      addTiles()%>%
      addMarkers(~Lon, ~Lat, popup = ~Pole_Num_1)
  })
  output$texty <- renderText(paste(input$dates[1]))
  
  observeEvent(input$sendEmail, {
    
    emailBody <- HTML(paste(input$messageContact, "\n
The following message was sent by\n
                        Name:", input$firstContact, input$lastContact, "\n",
                        "Phone:", input$phoneContact, "\n",
                        "Email:", input$emailContact
    ))
    
    send.mail(from = "southbendcitymailbox@gmail.com",
              to = "southbendcitymailbox@gmail.com",
              subject = input$subjectContact,
              body = emailBody,
              html = TRUE,
              smtp = list(host.name = "smtp.gmail.com",
                          port = 465,
                          user.name="southbendcitymailbox@gmail.com",
                          passwd="0Z#cbf6NhQhr",
                          ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
