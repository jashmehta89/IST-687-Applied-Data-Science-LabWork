#JASH MEHTA
#lab 5
install.packages("RJSONIO")
install.packages("RCurl")
install.packages("sqldf")
library("sqldf")

#Storing link in a variable
Mary <- "https://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD"
#Storing URL
url = getURL(Mary)
#Fetching Json data using fromJSON
json <- fromJSON(url)
#finding the number of rows in second column of JSON data.
#First column is always metadata
numRows <- length(json[[2]])
numRows

#converting the dataset into dataframe
JSONdata<-data.frame(matrix(unlist(json), nrow=numRows,byrow=T), stringsAsFactors=FALSE)

#deleting first 8 columns
JSONdata <- JSONdata[,-1:-8]


str(JSONdata)
#Changing the name of the columns
names(JSONdata) <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2")

#Rmoving all null values and replacing with NA
for(i in 1:numRows ) { 
  
  record <- JSONdata[[1]][[i]] 
  
  record[sapply(record, is.null)] <- NA 
  
  JSONdata[[1]][[i]] <- record
  
}


JSONdata$DAY_OF_WEEK
#SQL query to find how many accidents occur on sunday
shr <- sqldf("select Count(*) from JSONdata where DAY_OF_WEEK = 'SUNDAY   '")
shr

JSONdata$INJURY
#SQL query to find how many injuries occur in accidents
shru <- sqldf("select Count(*) from JSONdata where INJURY = 'YES' ")
shru

#SQL query to list injuries by day 
shru1 <- sqldf("select INJURY,DAY_OF_WEEK from JSONdata where INJURY = 'YES' order by DAY_OF_WEEK ")
shru1

#using tapply to find how many accidents occur on sunday
aye <- tapply(JSONdata$DAY_OF_WEEK=="SUNDAY   ",JSONdata$DAY_OF_WEEK, sum)
aye

#using tapply to find find how many injuries occur in accidents
char<- tapply(JSONdata$INJURY=="YES",JSONdata$INJURY, sum)
char

#using tapply to list injuries according to the day
jay <- tapply(JSONdata$INJURY=="YES",JSONdata$DAY_OF_WEEK, sum)
jay
