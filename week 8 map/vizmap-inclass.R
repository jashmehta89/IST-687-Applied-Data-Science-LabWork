#inclass discussion
install.packages("ggplot2")
install.packages("ggmap")

library(ggplot2)
library(ggmap)

############################
#show a simple map
#########################

#get the us states map
us <- map_data("state")

#show map using ggplot
dummyDF <- data.frame(state.name, stringsAsFactors=FALSE)
dummyDF$state <- tolower(dummyDF$state.name)
map.simple <- ggplot(dummyDF, aes(map_id = state))  

map.simple <- map.simple+  geom_map(map = us, fill="white", color="black") 
map.simple <- map.simple + expand_limits(x = us$long, y = us$lat)
map.simple  
map.simple <- map.simple + coord_map() + ggtitle("basic map of USA")
map.simple





######################
#show population per state
#######################

#read in the data - use the function we previously created
dfStates <- readStates()

#remove 'dot' from state name and make sure everything is lowercase
dfStates$state <- gsub("\\.","", dfStates$state) 
dfStates$state <- tolower(dfStates$state)

map.popColor <- ggplot(dfStates, aes(map_id = state))  
map.popColor <- map.popColor+  geom_map(map = us, aes(fill=base2010)) 
map.popColor <- map.popColor + expand_limits(x = us$long, y = us$lat)
map.popColor <- map.popColor+ coord_map() + ggtitle("state population")
map.popColor




#####################
#show a point on the map
######################
map.simple + geom_point(aes(x = -100, y = 30))
map.simple + geom_point(aes(x = -100, y = 30), color="darkred", shape=1)

map.popColor + geom_point(aes(x = -100, y = 30), color="darkred", shape=1)





##################
#show a logical location
####################
latlon <- geocode("syracuse university, syracuse, ny")
latlon
map.popColor + geom_point(aes(x = latlon$lon, y = latlon$lat),color="darkred", size = 3)

#show a second point
l <- data.frame(latlon)
latlon <- geocode("colorado")
l[2,] <- latlon
l[3,] <- geocode("denver, colorado")
map.simple + geom_point(data=l,aes(x = lon, y = lat))
l$state <- "?"
map.simple + geom_point(data=l,aes(x = lon, y = lat))
map.popColor + geom_point(data=l,aes(x = lon, y = lat),alpha = .5, color="darkred", size = 3)


##############
# get and display a bunch of points
############
urlFile <- "http://www.opendata500.com/us/download/us_companies.csv"
od.companies <- read.csv(url(urlFile))

#"make sure we have good data"
od.companies <- od.companies[od.companies$city != "",] 

#clean up the state abbreviations, and remove DC
od.companies$state <- as.character(od.companies$state)
od.companies <- od.companies[od.companies$state != "DC",]
od.companies$state <- ifelse(od.companies$state=="KA", "KS", od.companies$state)

od.companies$cityState <- paste(od.companies$city, od.companies$state)
od.companies$geoCode <- geocode(od.companies$cityState)

#show the locations of all the companies
map.simple + geom_point(data=od.companies, aes(x = geoCode$lon, y = geoCode$lat), shape=1)       

#what is the bad datapoint
bad <- od.companies[od.companies$geoCode$lon > 0, ]

#remove the bad datapoint
od.companies <- od.companies[od.companies$geoCode$lon < 0, ]
map.simple + geom_point(data=od.companies, aes(x = geoCode$lon, y = geoCode$lat), shape=1)       


#show each point based on the size of the company
od.companies$sizes <- factor(od.companies$full_time_employees,
                    levels = c("1-10", "11-50", "51-200", "201-500", "501-1,000", "1,001-5,000", "5,001-10,000", "10,001+"))
map.simple + geom_point(data=od.companies, aes(x = geoCode$lon, y = geoCode$lat, size=sizes), shape=1)       

##############
#have the color of each point also be based on the size of the company
#############
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
myColors <- brewer.pal(length(levels(od.companies$sizes)),"Reds")
names(myColors) <- levels(od.companies$sizes)
map.simple + geom_point(data=od.companies, aes(x = geoCode$lon, y = geoCode$lat, color=sizes, size=sizes)) +
   scale_colour_manual(name = "sizeOfCompany",values = myColors)

map.popColor + geom_point(data=od.companies, 
  aes(x = geoCode$lon, y = geoCode$lat, color=sizes, size=sizes)) + 
  scale_colour_manual(name = "sizeOfCompany",values = myColors) + 
  ggtitle("Open Data Company Analysis")


#############
#have the color of each state represent the number of companies in that state
###############

#create a dataframe with all states having 0 count
allCnt <- data.frame(state.name, 0, stringsAsFactors=FALSE)
colnames(allCnt) <- c("state", "count")
allCnt$state <- tolower(allCnt$state)

#sum all the companies in each state - store in a dataframe
counts <- tapply(od.companies$state, od.companies$state, length)
stateCnt <- data.frame(rownames(counts), counts)
colnames(stateCnt) <- c("abb", "count")

#upate counts (for states that we have counts)
allCnt$count[match(stateCnt$abb,state.abb)] <- stateCnt$count


map.color <- ggplot(allCnt, aes(map_id = state))  
map.color <- map.color+  geom_map(map = us, aes(fill=count)) 
map.color <- map.color + expand_limits(x = us$long, y = us$lat)
map.color <- map.color + coord_map()
map.color

map.color + geom_point(data=od.companies, aes(x = geoCode$lon, y = geoCode$lat), shape=1, color="white") 

map.color + geom_point(data=od.companies, aes(x = geoCode$lon, y = geoCode$lat, color=sizes, size=sizes), shape=1) + colScale





###################
# zoom around new york city
###################
zoomGeo <- geocode("New York, ny")
zoomAmount <- 3

centerx <- zoomGeo$lon
centery <- zoomGeo$lat
ylimit <- c(centery-zoomAmount, centery+zoomAmount)
xlimit <- c(centerx-zoomAmount, centerx+zoomAmount)

map.zoom <- ggplot(allCnt, aes(map_id = state))  
map.zoom <- map.zoom+  geom_map(map = us, aes(fill=count)) 
map.zoom <- map.zoom + expand_limits(x = xlimit, y = ylimit)
map.zoom <- map.zoom + coord_map()
map.zoom

zoom.companies <- od.companies
zoom.companies <- zoom.companies[zoom.companies$geoCode$lon > xlimit[1], ]
zoom.companies <- zoom.companies[zoom.companies$geoCode$lon < xlimit[2], ]
zoom.companies <- zoom.companies[zoom.companies$geoCode$lat > ylimit[1], ]
zoom.companies <- zoom.companies[zoom.companies$geoCode$lat < ylimit[2], ]

map.zoom + geom_point(data=zoom.companies, aes(x = geoCode$lon, y = geoCode$lat), shape=1, color="white") + expand_limits(x = xlimit, y = ylimit)

map.zoom + geom_point(data=zoom.companies, aes(x = geoCode$lon, y = geoCode$lat, color=sizes, size=sizes), shape=1) + colScale














#####################
#how does geocode work?
#######################
address <- ("syracuse university, syracuse, ny")

#create the URL - using JSON!!!
root <- "http://maps.google.com/maps/api/geocode/" 
url <- paste(root, "json?address=",address, "&sensor=false", sep = "")

#now get the results 0 parse a JSON (Java Script Object Notation)
require("RJSONIO")
apiResult <- getURL(URLencode(url))
geoStruct <- fromJSON(apiResult, simplify = FALSE)

#parse the reslts
lat <- geoStruct$results[[1]]$geometry$location$lat
lng <- geoStruct$results[[1]]$geometry$location$lng
l <- data.frame(lng,lat)
l

#compare to geocode
latlon <- geocode("syracuse university, syracuse, ny")
latlon


#myLat <- Addr2latlng
Addr2latlng <- function(address) 
{
  #create the URL - using JSON!!!
  root <- "http://maps.google.com/maps/api/geocode/" 
  url <- paste(root, "json?address=",address, "&sensor=false", sep = "")

  #now get the results
  apiResult <- getURL(URLencode(url))
  geoStruct <- fromJSON(apiResult, simplify = FALSE)
  
  #parse the reslts
  lat <- geoStruct$results[[1]]$geometry$location$lat
  lng <- geoStruct$results[[1]]$geometry$location$lng
  l <- data.frame(lat, lng)
  return(l)
}









####################
# plot percent change for each state 
# as a cirlce on the map, one for each state
######################

#first get the lat/long for each state
latlon <- geocode(dfStates$state)
dfStates$latlon <- latlon

#now plot the points, on top of the map
gg + geom_point(aes(x = dfStates$latlon$lon, y = dfStates$latlon$lat),alpha = .5, color="darkred", size = dfStates$PercentChange)


#range isn't "enough" to see the difference
range(dfStates$PercentChange)
gg + geom_point(aes(x = dfStates$latlon$lon, y = dfStates$latlon$lat),alpha = .5, color="darkred", size = dfStates$PercentChange*5)

#remove HI and AK
df <- dfStates[dfStates$state != "alaska",]
df <- df[df$state != "hawaii",]
gg + geom_point(aes(x = df$latlon$lon, y = df$latlon$lat),alpha = .5, color="darkred", size = df$PercentChange*5)

#need to have the correct / equal number of states
gg <- ggplot(df, aes(map_id = state)) + geom_map(aes(fill = base2010Num), map = us) + expand_limits(x = us$long, y = us$lat)
gg <- gg + coord_map()
gg + geom_point(aes(x = df$latlon$lon, y = df$latlon$lat),alpha = .5, color="darkred", size = df$PercentChange*5)

###################################
# put all the code required for reading the states csv file into this function
#######################################
readStates <- function() {
  #read a cvs file into a dataframe
  urlToRead <- "http://www.census.gov/popest/data/state/totals/2011/tables/NST-EST2011-01.csv"
  dfStates <- read.csv(url(urlToRead))
  
  #now have to fix up the mess
  #dfStates[1,]
  
  #str(dfStates)
  
  #remove the last 5 columns (all NAs)
  #dfStates[1,]
  #length(dfStates[1,])
  #dfStates[1,6]
  #dfStates[1,7]
  #dfStates[1,8]
  #dfStates[1,9]
  #dfStates[1,10]
  dfStates <- dfStates[,1:5]
  #dfStates
  #str(dfStates)
  
  
  #remove the header stuff in front
  head(dfStates,5)
  
  #dfStates[5,]
  #see that the 9'th row is the first state
  #dfStates[9,]
  
  dfStates <- dfStates[-1:-8,]
  #dfStates
  
  #update header names
  colnames(dfStates) <- c("state", "Jul2010", "Jul2011", "base2010", "base2011" )
  #dfStates[1,]
  #head(dfStates)
  
  #how about the end 
  #tail(dfStates)
  #nrow(dfStates)
  dfStates <- dfStates[1:51,]
  
  #tail(dfStates)
  #dfStates
  #dfStates$base2011
  #why does this line cause an error?
  #mean(dfStates$Jul2011)
  
  #str(dfStates)
  dfStates$base2011 <- as.numeric(gsub(",","", dfStates$base2011))
  dfStates$base2010 <- as.numeric(gsub(",","", dfStates$base2010))
  head(dfStates,5)
  return (dfStates)
}


#read in the census data set
readCensus <- function() {
  urlToRead <-    
    "http://www.census.gov/popest/data/state/totals/2011/tables/NST-EST2011-01.csv"
  
  #read the data from the web
  testFrame <- read.csv(url(urlToRead))
  
  #remove the first 8 rows (‘header information’)
  testFrame<-testFrame[-1:-8,]
  
  #only keep the first 5 columns
  testFrame<-testFrame[,1:5]
  
  #rename the first column
  testFrame$region <- testFrame[,1]
  testFrame<-testFrame[,-1]
  
  #remove the last rows (tail info)
  testFrame<-testFrame[-52:-58,]
  
  #remove the ‘dot’ from the state name
  testFrame$region <- gsub("\\.","", testFrame$region)
  
  #convert the columns to actual numbers and rename columns
  testFrame$april10census <-gsub(",", "", testFrame$X)
  testFrame$april10base <-gsub(",", "", testFrame$X.1)
  testFrame$july10pop <-gsub(",", "", testFrame$X.2)
  testFrame$july11pop <-gsub(",", "", testFrame$X.3)
  
  testFrame$april10census <-as.numeric(gsub(" ", "", testFrame$april10census))
  testFrame$april10base <-as.numeric(gsub(" ", "", testFrame$april10base))
  testFrame$july10pop <-as.numeric(gsub(" ", "", testFrame$july10pop))
  testFrame$july11pop <-as.numeric(gsub(" ", "", testFrame$july11pop))
  
  testFrame$april10census <-Numberize(testFrame$X)
  testFrame$april10base <-Numberize(testFrame$X.1)
  testFrame$july10pop <-Numberize(testFrame$X.2)
  testFrame$july11pop <-Numberize(testFrame$X.3)
  testFrame <- testFrame[,-1:-4]
  
  #remove the old rownames, which are now confusing
  rownames(testFrame) <- NULL
  
  return(testFrame)
}


# Numberize() - Gets rid of commas and other junk and 
# converts to numbers
# Assumes that the inputVector is a list of data that 
# can be treated as character strings
Numberize <- function(inputVector)
{
  # Get rid of commas
  inputVector<-gsub(",","", inputVector)
  # Get rid of spaces
  inputVector<-gsub(" ","", inputVector)
  
  return(as.numeric(inputVector))
}

test <- readCensus()
