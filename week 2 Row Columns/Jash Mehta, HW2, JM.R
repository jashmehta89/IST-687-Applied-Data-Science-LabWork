#Jash Mohinish Mehta
#Homework for Class 3 RowCols-Hw

#TASK 1
mtcars

#Copying raw data into a New Data Set
mymtcars <- mtcars
mymtcars

#First of all assigning disp/cyl to newvar
#Then assigning new variable to mymtcars
mymtcars$newvar <- mymtcars$disp/mymtcars$cyl
mymtcars

#Displaying the summary for the new column
summary(mymtcars$newvar)

#Task 2
#Creating number of Pets
pets <- c(2,2,3,4,1)
pets

#Creating the birthorder
birthorder <- c(1,2,3,4,5)
birthorder

#Creating number of siblings
siblings <- c(2,2,2,2,2)
siblings

userId <- c("Shrey","Shrutik","Jash","Ayesha","Charlotte")
userId

#Creating the data frame
myFriends <- data.frame(userId,pets,birthorder,siblings)
myFriends
#This will give X obs. Of 4 variables
str(myFriends)
#this will give Min, Mean, Max, Median
summary(myFriends)
