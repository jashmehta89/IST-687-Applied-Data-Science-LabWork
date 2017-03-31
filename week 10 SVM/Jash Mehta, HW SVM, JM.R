#Jash Mehta
#04/13/2016
#hw svm

install.packages("kernlab")   
library("kernlab")

#STEP1
Food <- read.csv("C:\\Users\\jashm\\Google Drive\\IM\\687\\Home Work\\SVM\\Food_Service_Establishment__Last_Inspection.csv")
Food$V <-NULL

#Step2
Food <- na.omit(Food)
Food$NewVio <- "Null"
head(Food$NewVio,20)
fix(Food)

for(i in 1:length(Food$VIOLATIONS))
{ 
  if(Food[i,4] == "No violations found.")
  {
    Food$NewVio[i] <- "N"
  }
  
  else
  {
    Food$NewVio[i] <- "Y"
  }
  
}

#Converting ViolInd into a Factor type of variable
#Currently it is numeric type
Food$NewVio <- factor(Food$NewVio)


#Checking count of violations and non-violation
table(Food$NewVio)


#Creating training and test dataset
#First we will create the random index
RndmIndex <- sample(1:nrow(Food))


#To check whether a random index is created or not
summary(RndmIndex)
head(RndmIndex)

TrainTestCutPoint <- floor(2*nrow(Food)/3)
TrainTestCutPoint


#So here 17868 is the cut point
#Now we take from 1 to 17868 random indexes for training data
#and from 17869 till the end random indexed for test data
trainData <- Food[RndmIndex[1:TrainTestCutPoint],]
testData <- Food[RndmIndex[(TrainTestCutPoint+1):nrow(Food)],]


#checking length of Training & Test Data
nrow(trainData)
nrow(testData)

#Having a look at the training and test data set
View(trainData)
View(testData)



#Step 3: Building a Model using KSVM
NewVioSVM <- ksvm(NewVio~TOTAL...CRITICAL.VIOLATIONS+TOTAL...NONCRITICAL.VIOLATIONS,data=trainData,kernel="rbfdot",kpar="automatic",C=50,cross=10,prob.model=TRUE)

#Checking output of NewVioSVM
NewVioSVM


#Having a look over the range of support vectors
hist(alpha(ViolSVM)[[1]])


#Tried creating SVM model with different value of regularization paramter i.e from 5 to 50
#and k-fold Cross validation parameter from 2 to 10
#But Training error and cross-validation error remained almost same in all the cases
#Not able to lower the error value any more
#So testing the test data for prediction using the model


#Predicting the variable using model
ViolSvmPred <- predict(NewVioSVM,testData,type="votes")


#Creating a new data from to check the truth versus prediction
comparison <- data.frame(testData[,27],factor(ViolSvmPred[2,]))


#Renaming columns as GroundTruth and Prediction
colnames(comparison) <- c("Truth","Predicted")


#Printing out the confusion matrix
ConfusionMatrix <- table(comparison)

ConfusionMatrix



##Predicted
##Truth    0    1
##N 2967    4
##Y    2 5901


#Checking the accuracy of prediction by calculating error rate
#Formula is to sum incorreclty classified instances and divide by total instances
paste("Prediction Error rate = ",round(((ConfusionMatrix[1,2]+ConfusionMatrix[2,1])/nrow(comparison))*100,2),"%")



#Step 4: Creating second model with additional predictors
NewVioSVM2 <- ksvm(NewVio~TOTAL...CRITICAL.VIOLATIONS+TOTAL...NONCRITICAL.VIOLATIONS+INSPECTION.TYPE+LAST.INSPECTED,data=trainData,kernel="rbfdot",kpar="automatic",C=50,cross=10,prob.model=TRUE)

#Checking output of NewVioSVM2
NewVioSVM2


#Having a look over the range of support vectors
hist(alpha(NewVioSVM2)[[1]])


#Tried creating SVM model with different value of regularization paramter i.e from 5 to 50
#and k-fold Cross validation parameter from 2 to 10




#Predicting the variable using model
ViolSvmPred2 <- predict(NewVioSVM2,testData,type="votes")


#Creating a new data from to check the ground truth versus prediction
comparison2 <- data.frame(testData[,27],factor(ViolSvmPred2[2,]))


#Renaming columns as Truth and Prediction
colnames(comparison2) <- c("Truth","Predicted")


#Printing out the confusion matrix
ConfusionMatrix1 <- table(comparison2)

ConfusionMatrix1

##Predicted
##Truth    0    1
##N 2934    1
##Y   14 5925





#Difference between 1st and 2nd model
#In the second model added more predictors compared to 1st model like INSPECTION.TYPE, LAST.INSPECTED etc.

#Also, the confustion matrix has slight difference.
#Model 1 had 9 incorrectly identified instances whereas model2 had little more.

#Model 1, itself, is performing very well. So, cannot drive down the error anymore
#If I had chosen some different predictors in model1 then I could have reduced the error in model 2
#I would have then chosen good predictors for model2 and would have drived down the error rate



#Checking the accuracy of prediction by calculating error rate
#Formula is to sum incorreclty classified instances and divide by total instances
paste("Prediction Error rate = ",round(((ConfusionMatrix1[1,2]+ConfusionMatrix1[2,1])/nrow(comparison2))*100,2),"%")


#End of program