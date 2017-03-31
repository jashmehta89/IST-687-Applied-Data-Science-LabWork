
#install.packages("RCurl")
install.packages("gdata")
#library("RCurl")
library("gdata")


install.packages("xlsx")
library(xlsx)

#Reading the data from the sheet from local machine
dataAn <- read.xls("C:\\soaps\\MedianZIP.xlsx", perl="C:\\Perl\\bin\\perl.exe")
dataA <- dataAn[-1,]


#changing column names
colnames(dataA) <- c("zip", "median", "mean", "population")

install.packages("zipcode")
library("zipcode")

data("zipcode")

#Merging data
Merge1<-merge(dataA, zipcode, by = intersect(names(dataA), names(zipcode)))
#removing alaska and hawaii
e<-Merge1[which(Merge1$state!="AK"),]
e<-e[-which(e$state=="HI"),]


#install.packages("tm")
#library("tm")
#Lowercase
e$state <- tolower(e$state)
#e$population <- gsub(",","",e$population)
#e$median <- gsub(",","",e$median)
#converting to numeric
e$median <- as.numeric(e$median)
e$population <- as.numeric(e$population)

#aggregating according to median and populaiton average
x<-aggregate(e$median ~ e$state, data=e, mean)
y<-aggregate(e$population ~ e$state, data=e, mean)

#merging x and y
#merge2 has 43 observations
Merge2<-merge(x, y, by = intersect(names(x), names(y)))

#or <- order(Merge1$population)
#sortedStates <- Merge1 [or,]
install.packages("ggplot2")
install.packages("maps")
install.packages("mapproj")
install.packages("ggmap")
library("maps")
install.packages("openintro")

#changing column names for my ease
colnames(Merge2) <- c("state", "median","population")

#coverting abbr to full names
Merge2$statename <- abbr2state(Merge2$state)

#code from PPT taught in class for plotting simple map
us <- map_data("state") 
#Merge2$statename <- tolower(Merge2$statename)
map.simple <- ggplot(Merge2, aes(map_id =statename))
map.simple <- map.simple +geom_map(map = us, fill="white",  color="black") 
map.simple <- map.simple +expand_limits(x = us$long, y = us$lat)
map.simple <- map.simple +coord_map() + ggtitle("basic map of USA")
map.simple

#STEP 2
#map for plotting states with median incomes
map.popColor <- ggplot(Merge2, aes(map_id = statename))  
map.popColor <- map.popColor + geom_map(map = us, aes(fill=median)) 
map.popColor <- map.popColor + expand_limits(x = us$long, y = us$lat)
map.popColor <- map.popColor + coord_map() + ggtitle("median income")
map.popColor

#STEP 2
#map for plotting states with population
map.popColor <- ggplot(Merge2, aes(map_id = statename))  
map.popColor <- map.popColor + geom_map(map = us, aes(fill=population)) 
map.popColor <- map.popColor + expand_limits(x = us$long, y = us$lat)
map.popColor <- map.popColor + coord_map() + ggtitle("population")
map.popColor

#latlon <- geocode("syracuse university, syracuse, ny")
#latlon

#STEP 3
#just copying data into merge3
Merge3 <- Merge2
#using geocode to get lat and lon of states
geos <- geocode(Merge3$statename)
map.popColor <- ggplot(Merge3, aes(map_id = statename))  
map.popColor <- map.popColor + geom_map(map = us, aes(fill=population)) 
map.popColor <- map.popColor + expand_limits(x = us$long, y = us$lat)
map.popColor <- map.popColor + coord_map() + ggtitle("population")
map.popColor <- map.popColor+ geom_point(aes(x = geos$lon, y = geos$lat),color="darkred", size = 3)
map.popColor

#STEP 4 - using stat_density2d function to see which state has more zipcodes and less zip
Merge3$state <- Merge3$statename
map.popColor1 <- ggplot(Merge3, aes(map_id = state))  
map.popColor1 <- map.popColor1 + geom_map(map = us, fill="white", color="black") 
map.popColor1 <- map.popColor1 + expand_limits(x = us$long, y = us$lat)
map.popColor1 <- map.popColor1 + coord_map()
map.popColor1 <- map.popColor1+ geom_point(aes(x = longitude, y = latitude),data=Merge1)
map.popColor1 <- map.popColor1+ stat_density2d(aes(x = longitude, y = latitude),data=Merge1, geom = "polygon")
map.popColor1

#STEP 5 - ZOOMING
# Code from inclass.R

zoomGeo <- geocode("New York, ny")
zoomAmount <- 3

centerx <- zoomGeo$lon
centery <- zoomGeo$lat
ylimit <- c(centery-zoomAmount, centery+zoomAmount)
xlimit <- c(centerx-zoomAmount, centerx+zoomAmount)

# Zooming in using coord_cartesian:

map.popColor1 + coord_cartesian(x = xlimit, y = ylimit)

map.popColor + coord_cartesian(x = xlimit, y = ylimit)

