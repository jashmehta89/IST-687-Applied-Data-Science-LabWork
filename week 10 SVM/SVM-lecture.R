# example 1

install.packages("kernlab")   
library("kernlab")

data(spam)
str(spam)
dim(spam)
table(spam$type)

randIndex <- sample(1:dim(spam)[1])
cutPoint2_3 <-floor(2*dim(spam)[1]/3)

trainData <- spam[randIndex[1:cutPoint2_3],]
testData <- spam[randIndex[(cutPoint2_3+1):dim(spam)[1]],]

head(trainData)

svmOutput <- ksvm(type ~ ., data=trainData,kernel="rbfdot",
                  kpar="automatic",C=5,cross=3,prob.model=TRUE)

svmOutput <- ksvm(type ~ ., data=trainData,kernel="rbfdot",
                  kpar="automatic",C=50,cross=3,prob.model=TRUE)

svmPred <- predict(svmOutput, testData, type="votes")
compTable <- data.frame(testData[,58], svmPred[1,])
table(compTable)


#  example 2

# install package
install.packages('e1071',dependencies=TRUE)
library(e1071)

# reading data 
bank_data = read.csv("bank.csv", sep=";", header=TRUE) 
# predicting y 
model  <- svm(y~., data = bank_data)
print(model)
summary(model)

tuned <- tune.svm(y~., data = bank_data, gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tuned)

