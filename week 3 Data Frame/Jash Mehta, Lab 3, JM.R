# Reading file from the URL

#Creating a function to read a csv file

readStates <- function(myVector)
{
  x <- "http://www.census.gov/popest/data/state/totals/2011/tables/NST-EST2011-01.csv"
  dfStates1 <- read.csv(x)
}
dfStates1

dfStates2 <- dfStates1

#ignore
dfStates1 <- dfStates1[,-6:-10]

#dfStates1 <- dfStates1[-1,]
#dfStates1 <- dfStates1[-1:-2,]
#dfStates1 <- dfStates1[-1:-5,]
#dfStates1$table.with.row.headers.in.column.A.and.column.headers.in.rows.3.through.4...leading.dots.indicate.sub.parts.
#dfStates1 <- dfStates1[-52:-58,]

#colnames(dfStates1) <- c("state", "Jul2010", "Jul2011", "base2010", "base2011")
#dfStates1[1,]




#dfStates1$Jul2010
#dfStates1$Jul2010 <- gsub(",","",dfStates1$Jul2010)
#as.numeric(dfStates$Jul2010)
#mean(dfStates$Jul2010)


dfStates2
#Just copying into dfStates3 for safety
dfStates3 <- dfStates2

#cleaning data
#deleting last 5 columns as they are not neccessary 
dfStates2 <- dfStates2[,-6:-10]

#changing column names
colnames(dfStates2) <- c("state", "Jul2010", "Jul2011", "base2010", "base2011")
dfStates2[1,]

#Cleaning all rows
dfStates2 <- dfStates2[-1:-8,]
dfStates2$state
dfStates2 <- dfStates2[-52:-58,]

#This gsub has to be done to remove commas from Jul2010 column 
dfStates2$Jul2010 <- gsub(",","",dfStates2$Jul2010)
#This converts into numeric 
dfStates2$Jul2010 <- as.numeric(dfStates2$Jul2010)
dfStates2$Jul2010
mean(dfStates2$Jul2010)

#This gsub has to be done to remove commas from Jul2011 column 
dfStates2$Jul2011 <- gsub(",","",dfStates2$Jul2011)
#This converts into numeric
dfStates2$Jul2011 <- as.numeric(dfStates2$Jul2011)
dfStates2$Jul2011

#This gsub has to be done to remove commas from base2010
dfStates2$base2010 <- gsub(",","",dfStates2$base2010)
# This converts into numeric
dfStates2$base2010 <- as.numeric(dfStates2$base2010)
dfStates2$base2010

#This gsub has to be done to remove commas from base2011
dfStates2$base2011 <- gsub(",","",dfStates2$base2011)
# This converts into numeric
dfStates2$base2011 <- as.numeric(dfStates2$base2011)
dfStates2$base2011

#Highest populiton in Jul2011
highpop <- which.max(dfStates2$Jul2011)
dfStates2[highpop,]

#Arranging in ascending order
or <- order(dfStates2$Jul2011)
sortedStates <- dfStates2 [or,]
dfStates2

#Cufunction <- function(myVector,num)
#{
  
#}

#13.	Test the function with the vector 'dfStates$Jul2011Num', and the mean of dfStates$Jul2011Num'.
myNewFunction<-function(myVector,a)
{
  b<-length(myVector)
  myVector<-which(myVector<a)
  length(myVector)*100/b
  return(length(myVector)*100/b)
}
myNewFunction(dfStates2$Jul2011,mean(dfStates2$Jul2011))


