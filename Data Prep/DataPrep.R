library(rgdal)
library(leaflet)
library(tidyverse)
library(rgeos)

setwd("~/GitHub/Data-Viz-Final-Project")

########### Load data sets ###########
#Abandoned Property data
AbandonedPropertyParcels <- readOGR(dsn = "./data/Abandoned_Property_Parcels",
                                    layer = "Abandoned_Property_Parcels",
                                    stringsAsFactors = FALSE)

#City Council data
CityCouncilDistricts <- readOGR(dsn = "./Data/City_Council_Districts",
                                layer = "City_Council_Districts",
                                stringsAsFactors = FALSE)

#Cenus data
census <- readOGR(dsn="./Data/2010_Cenusdata",
                  layer ='2010_CensusData',
                  stringsAsFactors = FALSE)


AbandonedPropertyParcels.center <- SpatialPointsDataFrame(gCentroid(AbandonedPropertyParcels, byid=TRUE), 
                                                          AbandonedPropertyParcels@data, match.ID=FALSE)

public_facilities <- read.csv('./data/Public_Facilities.csv')
facilities.spatial <- SpatialPointsDataFrame(coords = public_facilities[,c("Lon","Lat")], data = public_facilities, proj4string = CRS("+proj=longlat +datum=WGS84"))


street_lights <- read.csv("./data/Street_Lights.csv", stringsAsFactors = F)
street_lights[street_lights$Pole_Type %in% c(""," "),]$Pole_Type <- "Unknown"
street_lights[street_lights$Service %in% c(""," "),]$Service <- "Unknown"
street_lights$Inspect_Date2 <- as.Date(street_lights$Inspect_Date)

AbandonedPropertyParcels$popup <- paste("<b>",AbandonedPropertyParcels$Outcome_St,"</b><br>",
                                        "Address: ", AbandonedPropertyParcels$Address_Nu, AbandonedPropertyParcels$Street_Nam,AbandonedPropertyParcels$Suffix, "<br>", 
                                        "Structure Type: ", AbandonedPropertyParcels$Structures, "<br>", 
                                        "District: ", AbandonedPropertyParcels$Council_Di)

CityCouncilDistricts$popupCouncils <- paste("<b>","District: ", CityCouncilDistricts$OBJECTID, "</b><br>",
                                            "Council Member: ", CityCouncilDistricts$Council_Me)

facilitiesCheckboxChoices <- unique(facilities.spatial@data$POPL_TYPE)
facilitiesCheckboxSelected <- unique(facilities.spatial@data$POPL_TYPE)

#School data
schoolsSpatial <- readOGR(dsn = "./data/School_Boundaries",
                          layer = "School_Boundaries")

#Park data
parksPoints <- read.csv("./data/Parks_Locations_and_Features.csv",
                        stringsAsFactors = F)

naReplaceCols <- c('Aqua_Feat__Pool', 'Aqua_Feat__Spray', 'Backstop__Practice', 'Ballfield',
                   'Basketball', 'Blueway', 'Complex__Ballfield', 'Complex__Tennis',
                   'Concessions', 'Disk_Golf', 'Driving_Range', 'Educational_Experience',
                   'Event_Space', 'Fitness_Course', 'Garden__Community', 'Garden__Display',
                   'Golf', 'Hockey__Ice', 'Loop_Walk', 'MP_Field__Large',
                   'MP_Field__Multiple', 'MP_Field__Small', 'Multiuse_Court',
                   'Natural_Area', 'Open_Turf', 'Open_Water', 'Other___Active',
                   'Other_Passive', 'Passive_Node', 'Picnic_Grounds',
                   'Playground__Destination', 'Playground__Local', 'Public_Art', 'Shelter',
                   'Shelter__Group', 'Skate_Park', 'Sledding_Hill', 'Structure', 'Tennis',
                   'Trail__Primitive', 'Volleyball', 'Water_Access__Developed',
                   'Water_Access__General', 'Water_Feature')

naReplaceZeros <- function(x){
  x <- ifelse(is.na(x), 0, x)
}

stringFeatures <- parksPoints %>%
  mutate_at(.vars = naReplaceCols, .funs = naReplaceZeros) %>%
  gather(key = "Feature", value = "Amount", naReplaceCols) %>%
  filter(Amount > 0) %>%
  group_by(Park_Name, Address) %>%
  mutate(stringFeatures = paste0(paste0(Feature, ": ", Amount), collapse = "<br>")) %>%
  mutate(stringFeatures = gsub(pattern = "_", replacement = " ", stringFeatures)) %>%
  mutate(stringFeatures = gsub(pattern = "  ", replacement = " ", stringFeatures)) %>%
  group_by(Park_Name, Park_Type, stringFeatures) %>%
  summarise()

parksPoints <- parksPoints %>%
  left_join(stringFeatures) %>%
  mutate(stringFeatures = ifelse(is.na(stringFeatures), "Not Available", stringFeatures))

parksSpatial <- SpatialPointsDataFrame(coords = parksPoints[,c("Lon","Lat")], data = parksPoints,
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))

#Create pop ups
parksSpatial$popup <- paste("<b>",parksSpatial$Park_Name,"</b><br>",
                            "Type: ",parksSpatial$Park_Type,"<br>",
                            "Address: ",parksSpatial$Address, "<br>",
                            "<b>Features</b><br>",
                            parksSpatial$stringFeatures,
                            sep ="")

#Set color palettes
palParks <- colorFactor(palette = 'RdYlBu', domain = parksSpatial$Park_Type)
palProperties <- colorFactor(palette = "RdYlBu", domain =AbandonedPropertyParcels$Outcome_St)
palCenus <- colorNumeric(palette = "RdYlBu", domain = census$SE_T002_01)
palSchools <- colorFactor(palette = "Dark2", domain = schoolsSpatial$SchoolType)
  
save.image(file = "SB Dashboard App/appData.RData")
