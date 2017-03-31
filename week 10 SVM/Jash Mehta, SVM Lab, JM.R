#Jash Mehta
#Lab SVM
#4/7/2016

#Installing all packages and libraries
install.packages("e1071")
library("e1071")
install.packages("kernlab")
library("kernlab")
install.packages("gridExtra")
library(gridExtra)
install.packages("ggplot2")
library("ggplot2")
#####################3
#STEP1
#############
#Copying the data into new dataset called air
air<-airquality


#remove any nulls
for(i in 1:ncol(air)){
  air[is.na(air[,i]),i] <- mean(air[,i], na.rm = TRUE)
}


air
##################
#STEP 2
###############
#Creating Random Index and Cut Point at 2/3 of data set
randIndex <- sample(1:dim(air)[1])
cutPoint2_3 <-floor(2*dim(air)[1]/3)
#Creating training and testing dataset
trainData <- air[randIndex[1:cutPoint2_3],]
testData <- air[randIndex[(cutPoint2_3+1):dim(air)[1]],]
head(trainData)

##################
#STEP 3 [Parts 1-5]
###############3
 #Using ksvm to build a model
 model.ksvm <- ksvm(Ozone ~ . , data=trainData)
 PredictY<- predict(model.ksvm, testData)
 #Computing RMSE
 error <- testData$Ozone - PredictY
 sqrt(mean(error^2))
#Plotting results using scatter plot
g1 <- ggplot(testData, aes(x=Temp, y=Wind)) + geom_point(aes(size = error, color=error)) 
g1

 #Using svm to build a model
 model.svm <- svm(Ozone ~ Solar.R+Wind , data=trainData)
 PredictY2<- predict(model.svm, testData)
 #RMSE
 error1 <- testData$Ozone - PredictY2
 sqrt(mean(error1^2))
 #Scatter plot
 g2 <- ggplot(testData, aes(x=Temp, y=Wind)) + geom_point(aes(size = error1, color=error1)) 
 g2

 #Using lm to build a model
 model.lm <- lm(formula = Ozone~Wind+Temp, data = trainData)
 PredictY3 <- predict(model.lm, testData, type="response")
 error2 <- testData$Ozone - PredictY3
 #RMSE
 sqrt(mean(error2^2))
 #Scatter Plot
 g3 = ggplot(data = testData, aes(x=Temp,y=Wind)) + geom_point(aes(size=error2, color = error2)) 
 g3
 #Showing all the charts using grid.arrange function
 grid.arrange(g1,g2,g3)
 
 ###################3
 #STEP 4: 
 ###################
 #NOTE THE V7 variable CREATED in air data 
 #is my 'goodOZONE'
 #Calculating mean ozone
 meanOzone <- mean(air$Ozone)
 meanOzone
 
 #Creating one more column in air dataset to record 0 and 1 
 #depending on the treshold of Ozone
 for ( i in 1:length(air$Ozone))
 {
   if( air[i,1] < meanOzone )
   {
     air[i,7] = '0'
   }
   else
   {
     air[i,7] = '1'
   }
 }
 
 ##########################
 #STEP 5 [Part 1-5]
 ##########################3
 #Copying air into new dataset 
 newair =air
 
 
 #Creating random and cutpoint: SAME AS I DID ABOVE
 randIndex1 <- sample(1:dim(newair)[1])
 CutPoint2_3 <-floor(2*dim(newair)[1]/3)
 
 #Creating training and Testing Dataset: SAME AS I DID ABOVE
 trainData2 <- newair[randIndex1[1:cutPoint2_3],]
 testData2 <- newair[randIndex[(CutPoint2_3+1):dim(newair)[1]],]
 head(trainData2)
 
 #Using ksvm to build a model
 model.ksvm2 <- ksvm(V7 ~ .,trainData2)
 predictGO <- predict(model.ksvm2, testData2)

 summary(predictGO)
 str(predictGO)
 #RMSE
 errorGO <- as.numeric(testData2$V7)-as.numeric(predictGO)
 sqrt(mean(errorGO^2))
 predictGO
 
 #Scatter Plot
 g4 <- ggplot(data = testData2, aes(x=Temp,y=Wind)) + geom_point(aes(size=errorGO,shape = predictGO, color = testData2$V7))
 g4
 
 #Calculating how accurately ksvm has predicted
 results <- table(predictGO, testData2$V7)
 print(results)
 percentCorrect <- (results[1,1]+results[2,2])/(results[1,1]+results[1,2]+results[2,1]+results[2,2])*100
 percentCorrect
 
 
 
 #Using svm to build a model
 #C-Classification is used otherwise R throws an error that numeric variable is required for regression
 model.svm2 <- svm(V7 ~ ., trainData2, type = "C-classification")
 predictGO1 <- predict(model.svm2, testData2)
 
 summary(predictGO1)
 
 errorGO1 <- as.numeric(testData2$V7) - as.numeric(predictGO1)
 sqrt(mean(errorGO1^2))
 #Scatter plot
 g5 = ggplot(data = testData2, aes(x=Temp,y=Wind)) + geom_point(aes(size=errorGO1,shape = predictGO1, color = testData2$V7))
 g5

 #Calculating how accurately svm has predicted
 results2 <- table(predictGO1, testData2$V7)
 print(results2)
 percentCorrect2 <- (results2[1,1]+results2[2,2])/(results2[1,1]+results2[1,2]+results2[2,1]+results2[2,2])*100
 percentCorrect2
 
 
 
 
 # Naive Bayes: SAME THINGS ARE DONE AS ABOVE
 
 
 model.nb <- naiveBayes(as.factor(V7) ~ ., trainData2)
 
 predictNB <- predict(model.nb, testData2)
 str(predictNB)
 summary(predictNB)
 
 errorNB <- as.numeric(testData2$V7) - as.numeric(predictNB)
 sqrt(mean(errorNB^2))
 
 
 #Accuracy
 
 results3 <- table(predictNB, testData2$V7)
 print(results3)
 percentCorrect3 <- (results3[1,1]+results3[2,2])/(results3[1,1]+results3[1,2]+results3[2,1]+results3[2,2])*100
 percentCorrect3
 
 
 # Scatter Plot:
 
 g6 = ggplot(data = testData2, aes(x=Temp,y=Wind)) + geom_point(aes(size=errorNB,shape = predictNB, col = testData2$V7))
 
 grid.arrange(g4,g5,g6)
 
 # IN this example
 # Naive Bayes is the best model according to me 
 #followed by KSVM and then SVM
 #Naive bayes gave the RMSE as 1.009 and the ksvm and svm gave more RMSE
 #Therefore the error rate of NAIVE bayes is less
 # Because Naive Bayes gives the least error rate among the three algorithms in this case it is better
 
 