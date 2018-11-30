library(rgdal)

setwd("~/GitHub/Data-Viz-Final-Project")

schoolsSpatial <- readOGR(dsn = "./Data", layer = "School_Boundaries")

parksPoints <- read.csv("./Data/Parks_Locations_and_Features.csv", stringsAsFactors = F)

parksSpatial <- SpatialPointsDataFrame(coords = parksPoints[,c("Lon","Lat")], data = parksPoints,
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))

parksSpatial$popup <- paste("<b>",parksSpatial$Park_Name,"</b><br>",
                            "Type: ",parksSpatial$Park_Type,"<br>",
                            "Address: ",parksSpatial$Address,sep ="")

pal <- colorFactor(palette = 'Set1', domain = parksSpatial$Park_Type)

save.image(file = "SB Dashboard App/appData.RData")
