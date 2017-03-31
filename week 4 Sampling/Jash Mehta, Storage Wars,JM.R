#Jash Mehta
#HomeWork-5
install.packages("sqldf")
library(sqldf)
#Loading dataset airquality
airquality
#Copying airquality dataset into air
air<-airquality
#Just checking the dataset with head() function
head(air)
#CLeaning the dataset by removing NAs
airq<-na.omit(air)
#Calculating the mean of Ozone
mean(airq$Ozone)

fg<-sqldf("SELECT AVG(Ozone) FROM airq")
fg

#Storing it into newAQ| SQL query using sqldf to find out the rows in Ozone which have more than average
newAQ<-sqldf("select * from airq where airq.Ozone>(SELECT AVG(Ozone) FROM airq)")
#displaying the structure of newAQ
str(newAQ)

head(newAQ)
x <- mean(airq$Ozone)
x
#Using tapply
tapply(airq$Ozone>x, airq$Ozone, sum)

