#Jash Mehta
#Lab Info Viz

#Installing package ggplot2
install.packages("ggplot2")
library(ggplot2)

#Step 1
#Loading airquality  data into airq
airq <- airquality
airq

#Step 2
#Running a for loop to replace NAs with the means of that column
for (i in 1:ncol(airq) ) {
  airq[is.na(airq[,i]),i] <- mean(airq[,i], na.rm=TRUE)
}

#Finding the structure of airq
str(airq)

#Step 3 A
#Creating Histograms for each Column
ggplot(airq, aes(x=Ozone)) +geom_histogram(binwidth=20)
ggplot(airq, aes(x=Solar.R)) +geom_histogram(binwidth=20)
ggplot(airq, aes(x=Wind)) +geom_histogram()
ggplot(airq, aes(x=Temp)) +geom_histogram()
ggplot(airq, aes(x=Month)) +geom_histogram()
ggplot(airq, aes(x=Day)) +geom_histogram()

#Creating a boxplot for Ozone
ggplot(airq,aes(x=factor(0),Ozone)) +geom_boxplot()

#Rounding off Wind 
airq$Wind <- round(airq$Wind)

#Creating a boxplot for Wind
ggplot(airq,aes(x=factor(0),Wind)) +geom_boxplot()


#Step 3B
#Storing the date so that we can work with this in the next step
d <- as.Date(paste(1973,airq$Month,airq$Day, sep="."), format="%Y.%m.%d")

#Step 3B
#Plotting 4 different lines on different Charts
a <- ggplot(data=airq, aes(x = Ozone, y=d)) + geom_line()
a
b <- ggplot(data=airq, aes(x = Wind, y=d)) + geom_line()
b
c <- ggplot(data=airq, aes(x = Temp, y=d)) + geom_line()
c 
ff <- ggplot(data=airq, aes(x = Solar.R, y=d)) + geom_line()
ff

# Plotting all 4 lines in one chart with different colors
g1<- ggplot(data=airq, aes(y=d)) + geom_line(aes(x = Ozone, colour= "Ozone")) + geom_line(aes(x = Wind, colour ="Wind")) + geom_line(aes(x = Solar.R, colour ="Solar.R")) + geom_line(aes(x = Temp, colour = "Temp"))
g1

#Step 5
#Plotting Scatter Plot 
#with the x-axis representing the wind, the y-axis representing the temperature, 
#the size of each dot representing the ozone 
#and the color representing the solar.R
g <- ggplot(airq, aes(x=Wind, y=Temp)) + geom_point(aes(size = Ozone, color=Solar.R)) 
g

#Step 4

install.packages("reshapes2")
library(reshape2)

#Convert data frame into matrix
aqmatrix <- data.matrix(airq)
aqmatrixmelt<-melt(aqmatrix)

#Creating the heatmap
heat<-ggplot(aqmatrixmelt, aes(y=Var2,x=Var1)) + geom_tile(aes(fill=value))+scale_fill_gradient(low="white",high="orange")+xlab("")+ylab("days")
heat


#Step 6 Final Analysis
#6.1 Yes, I see patterns after exploring the data.
#6.2 I found line chart the most useful and easy to read.