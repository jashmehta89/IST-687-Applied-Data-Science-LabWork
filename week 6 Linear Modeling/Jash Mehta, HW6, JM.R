#Jash Mehta
#IST687 - HW 6 Linear Modeling
#Installing and Libraring required packages
#Checking if package is already installed

  install.packages("RCurl")
  install.packages("gdata")
library("RCurl")
library("gdata")




#Step 1-3:
#Reading the data from the sheet from local machine
dataAn <- read.xls("C:\\Users\\jashm\\Downloads\\mlr01.xls", perl="C:\\Perl\\bin\\perl.exe")


#Checking the structure of dataset
str(dataAn)


#Changing column names to make it more meaningful as per the assignment
names(dataAn)[]<-c('Fawns','AdultAntelope','Precipitation','Winter')
str(dataAn)


#Step 4:
#Creating bivariate plot for baby fawns versus adult antelope population
plot(dataAn$AdultAntelope,dataAn$Fawns, xlab = 'Population of adult Antelope', ylab = 'Number of Fawns')

#Creating bivariate plot for baby fawns versus precipitation that year
plot(dataAn$Precipitation,dataAn$Fawns, xlab = 'Annual Precipitation', ylab = 'Number of Fawns')

#Creating bivariate plot for baby fawns versus the severity of the winter
plot(dataAn$Winter,dataAn$Fawns, xlab = 'Winter Severity', ylab = 'Number of Fawns')


#Step 5:
#Creating 3 different regression models and predicting 
#Will perform two predictions for each model, based on conmbination of low and high values of independent variables)

#Model 1 - predicting the number of fawns from the severity of the winter
model1<-lm(formula=Fawns~Winter, data=dataAn)

summary(model1)


predict(model1,data.frame(Winter=1),type = "response")
predict(model1,data.frame(Winter=5),type = "response")



#Model 2a - predict the number of fawns from two variables (severity of the winter + annual precipitation)
model2a<-lm(formula=Fawns~Winter+Precipitation, data=dataAn)

summary(model2a)


predict(model2a,data.frame(Winter=1,Precipitation=14.1),type = "response")
predict(model2a,data.frame(Winter=5,Precipitation=10.6),type = "response")


#Model 2b - predicting the number of fawns from two variables (severity of the winter + adult antelope population)
model2b<-lm(formula=Fawns~Winter+AdultAntelope, data=dataAn)

summary(model2b)


predict(model2b,data.frame(Winter=1,AdultAntelope=9.7),type = "response")
predict(model2b,data.frame(Winter=5,AdultAntelope=6.8),type = "response")


#Model 3 - predicting the number of fawns from the three other variables
model3<-lm(formula=Fawns~Winter+AdultAntelope+Precipitation, data=dataAn)

summary(model3)

predict(model3,data.frame(Winter=1,AdultAntelope=9.7,Precipitation=14.1),type = "response")
predict(model3,data.frame(Winter=5,AdultAntelope=6.8,Precipitation=10.6),type = "response")


print("In my opinion model3 worked best")
print("I did two types of prediction:")
print("One with combination of low winter severity, high adult antelope population and annual precipitation")
print("Another with combination of high winter severity, low adult antelope population and annual precipitation")
print("All the three independent variables can predict the dependent variable efficiently")
print("BEST Model with 2 predictors will be with PopAdultAntelope+AnnualPrecipitation")
print("BEST Model with 1 predictor will be with PopAdultAntelope")

#End of program

