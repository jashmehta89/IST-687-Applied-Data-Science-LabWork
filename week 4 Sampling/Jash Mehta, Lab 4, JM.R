#Jash Mehta
#Lab 4
#Task 1

#Creating a function for mean, median, min, max, sd and skew
printVecInfo <- function(myVector)
{
  
  print(paste("mean:",mean(myVector)))
  
  print(paste("median:",median(myVector)))
  
  print(paste("min:",min(myVector), "max:", max(myVector)))
  
  print(paste("sd:",sd(myVector))) 
  
  #sd(myVector)
  #quantile(myVector)
  b <- quantile(myVector,c(0.05, 0.95))
  print(paste("quantile(0.05 -0.95):", b[1],"--", b[2] ) ) 
  
  print(paste("skewness:",skewness(myVector)))
  
}

a <- c(1,2,3,4,5,6,7,8,9,10,50)
printVecInfo(a)


#Task 2
#Creatng a variable jar having 50 red and blue balls
jar <- replicate(50,c("red","blue"))
jar

#COnfirming that there are 50 red balls
sum(str_count(jar,"red"))

#Sampling 10 redballs
sam <- sample(jar, size=10, replace=TRUE)
sam

#Counting the red balls and then calculating the percentage
percent <- sum(str_count(sam,"red"))/length(sam)*100
percent

#Sampling the data 10 times and replicate the mean 20 times
sample10<- replicate(20,mean(sum(str_count(sample(jar,size=10,replace=TRUE),"red"))))
hist(sample10)
printVecInfo(sample10)

#Sampling the data 100 times and replicate the mean 20 times
sample100<- replicate(20,mean(sum(str_count(sample(jar,size=100,replace=TRUE),"red"))))
hist(sample100)
printVecInfo(sample100)

#Sampling the data 100 times and replicate the mean 100 times
sample1000<- replicate(100,mean(sum(str_count(sample(jar,size=100,replace=TRUE),"red"))))
hist(sample1000)
printVecInfo(sample1000)


#TASK 3
airquality1 <- airquality
#Cleaning the data and removing all NAs
airquality1 <- na.omit(airquality1)
airquality1
fix(airquality1)

#Ozone's histogram
printVecInfo(airquality1$Ozone)
hist(airquality1$Ozone)

#Winds's histogram
printVecInfo(airquality1$Wind)
hist(airquality1$Wind)

#Time's histogram
printVecInfo(airquality1$Temp)
hist(airquality1$Temp)
