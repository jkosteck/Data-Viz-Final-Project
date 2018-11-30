library(leaflet)
library(rgeos)
library(rgdal)


schoolsSpatial <- readOGR(dsn = "C:/Users/kost1/Documents/GitHub/Data-Viz-2018-Fall/FinalProject", layer = "School_Boundaries")

parksPoints <- read.csv("C:/Users/kost1/Documents/GitHub/Data-Viz-2018-Fall/FinalProject/Parks_Locations_and_Features.csv", stringsAsFactors = F)

parksSpatial <- SpatialPointsDataFrame(coords = parksPoints[,c("Lon","Lat")], data = parksPoints,
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))

parksSpatial$popup <- paste("<b>",parksSpatial$Park_Name,"</b><br>",
                            "Type: ",parksSpatial$Park_Type,"<br>",
                            "Address: ",parksSpatial$Address,sep ="")

pal <- colorFactor(palette = 'Set1', domain = parksSpatial$Park_Type)

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
             tabPanel("Ashley"
             ),
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
}

# Run the application 
shinyApp(ui = ui, server = server)