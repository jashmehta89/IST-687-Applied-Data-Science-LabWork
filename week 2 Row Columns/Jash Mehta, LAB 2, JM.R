#Jash Mohinish Mehta
#Lab 2

mtcars
head(mtcars)
tail(mtcars)

#Copied mtcars dataset into myCars
myCars <- mtcars
myCars

#maximum horsepower of the car in the dataset
max(myCars$hp)

#find the index of the car which has maximum horsepower
index <- which.max(myCars$hp)
index

#this will print the row which has the index with maximum horsepower
myCars[index,]




#Finding the highest mpg
max(myCars$mpg)

#finding the car which has the highest mpg, following steps are followed

#finding the index of highest mpg
index_mpg <- which.max(myCars$mpg)
index_mpg

#Displaying the row with highest mpg
myCars[index_mpg,]

#sort the column 'mpg' according to the mpg of all the cars
o <- order(myCars$mpg)
o

# Printing the sorted mpg database
myCars[o,]


#Finding the best combination of mpg and hp
Combo <- myCars$mpg/myCars$hp
Combo


#Now we add the 'Combo' into the dataset myCars 
myCars$Combo <- myCars$mpg/myCars$hp
myCars

#Now we sort the dataset according to the Combo, this will give
Sort_Combo <- order(myCars$Combo)
Sort_Combo
myCars[Sort_Combo,]


#first define a variable that states how much mpg should count (range of 0 to 1)
percentMPG <- 0.5

#first for each attribute , scale between 0-1
mpgScaled <- (myCars$mpg - min(myCars$mpg))/(max(myCars$mpg) - min(myCars$mpg))
hpScaled <- (myCars$hp - min(myCars$hp))/(max(myCars$hp) - min(myCars$hp))
    
#Now Combine them
myCars$rating <- mpgScaled*percentMPG + hpScaled*(1.0-percentMPG)

#Finding index of best car out of rating column
bestcar<-which.max(myCars$rating)
bestcar

myCars[bestcar,]
