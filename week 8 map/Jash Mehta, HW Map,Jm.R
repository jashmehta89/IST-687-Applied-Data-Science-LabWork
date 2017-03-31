#Jash Mehta
#Homework Maps

install.packages("gdata")
library("gdata")

install.packages("ggplot2")
library("ggplot2")
install.packages("maps")
library("maps")
install.packages("mapproj")
library("mapproj")
install.packages("ggmap")
library("ggmap")
install.packages("openintro")
library("openintro")

install.packages("xlsx")
library("xlsx")

#Loading the data into R
dataCrime <- read.csv("C:\\soaps\\crimeInSYR.csv")
dataCrime

#Changing name of columns for convenience
colnames(dataCrime) <- c("TypeCrime", "Address", "City", "Date")

#creating a state column so that it does not give error later in the code
dataCrime$state <- "?" #feka hai state ka col

#Taking dummyDF from in class code and creating a simple map of USA
dummyDF <- data.frame(state.name, stringsAsFactors=FALSE)
dummyDF$state <- tolower(dummyDF$state.name)
map.simple <- ggplot(dummyDF, aes(map_id = state))  
us <- map_data("state")

map.simple <- map.simple+  geom_map(map = us, fill="white", color="black") 
map.simple <- map.simple + expand_limits(x = us$long, y = us$lat)
map.simple  
map.simple <- map.simple + coord_map() + ggtitle("basic map of USA")
map.simple

#Zooming into Syracuse
zoomGeo <- geocode("Syracuse, ny")
zoomAmount <- 3

centerx <- zoomGeo$lon
centery <- zoomGeo$lat
ylimit <- c(centery-zoomAmount, centery+zoomAmount)
xlimit <- c(centerx-zoomAmount, centerx+zoomAmount)

#Zooming into the map
SyrMap<- map.simple +coord_map()+ coord_cartesian(x = xlimit, y = ylimit)
SyrMap

#downloading all geo codes
dataCrime$city <- tolower(dataCrime$City)
geos <- geocode(dataCrime$city)
dataCrime$geos <- geos
#SyrMap <- SyrMap + geom_point(aes(x = geos$lon, y = geos$lat),color=TypeCrime, size = 3)
#SyrMap

#STEP1
map.simple1 <- SyrMap + geom_point(data=dataCrime, aes(x = geos$lon, y = geos$lat))
map.simple1

#STEP2
map.simple2 <- map.simple1 + stat_density2d(aes(x = geos$lon, y = geos$lat),data=dataCrime, geom = "polygon")
map.simple2
