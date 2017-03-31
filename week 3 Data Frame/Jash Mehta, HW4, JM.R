#Jash Mohinish Mehta
###########
## TASK1 ##
###########
#Creating a funtion which will accept a vector, min and max
MeraFunction <- function(myVector,min,max)
{#pasring every element in the vector to compare with the min and max
  for(i in myVector)
  {
    if(i<=max&&i>=min)
      #Storing the values between min and max
    vector <- c(vector,i)
  }
  #Counting the length of the vector
  a <- length(vector)
  return (a)
}
vectorwa <- rnorm(1000,80)
MeraFunction(vectorwa,79,81)
#> MeraFunction(vectorwa,79,81)
#[1] 678
#> vectorwa <- rnorm(1000,80)
#> MeraFunction(vectorwa,79,81)
#[1] 686
#> vectorwa <- rnorm(1000,80)
#> MeraFunction(vectorwa,79,81)
#[1] 681


###########
## TASK2 ##
###########
#Created a pareto functions
rpareto(51,51,1)
#Pareto funtion with similar distribution as of page 44
FSApops <- rparetoII(51,563626,37128286,8)
#Max value of FSApops
max(FSApops)
#Min value of FSApops
min(FSApops)
#Mean value of FSApops
mean(FSApops)
#SD value of FSApops
sd(FSApops)

hist(FSApops, breaks=20)

#Just duble checking with dataset of page number 44
sortedStates44 <- sortedStates
sortedStates44
hist(sortedStates44$Jul2011)
