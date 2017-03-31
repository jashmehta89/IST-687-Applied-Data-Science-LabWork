#Jash Mehta
#Lab Association
#3/31/2016



titanic <- load("C:\\gd\\IM\\687\\Association\\titanic.raw.rdata")
titanic <- titanic.raw

#C:\gd\IM\687\Association

#STEP1
#Part1: Calcuating the percent of people survived
n<-length(titanic$Survived[titanic$Survived=='Yes'])
n
m <- length(titanic$Survived)
m
surpercent <- (n/m)*100
surpercent

#Part2: Calcuating the percent of children 
NoOfChild <- length(titanic$Age[titanic$Age=='Child'])
NoOfChild
#I am going use this o in the further code
o <- length(titanic$Age)
o
ChildSurvivedPer <- (NoOfChild/o)*100
ChildSurvivedPer

#Part3: Calcuating the percent of females
NoOfFemale <- length(titanic$Sex[titanic$Sex=='Female'])
NoOfFemale
FemalePer <- (NoOfFemale/o)*100
FemalePer

#Part4: Calcuating the percent of 1st class people 
NoOf1st <- length(titanic$Class[titanic$Class=='1st'])
NoOf1st
stper <- (NoOf1st/o)*100
stper


#STEP2
#Part 1: percentage of children survived
NoOfChildSurvived <- length(titanic$Age[(titanic$Age=='Child')& (titanic$Survived=='Yes')])
NoOfChildSurvived                            
ChildrenSurvivedPer <- (NoOfChildSurvived/NoOfChild)*100                            
ChildrenSurvivedPer

#part 2:percentage of female survived 
NoOfFemaleSurvived <- length(titanic$Sex[(titanic$Sex=='Female')& (titanic$Survived=='Yes')])
NoOfFemaleSurvived 
FemaleSurvivedPer <- (NoOfFemaleSurvived/NoOfFemale)*100                            
FemaleSurvivedPer

#part 3:percentage of first class people survived
NoOf1stSurvived <- length(titanic$Class[(titanic$Class=='1st')& (titanic$Survived=='Yes')])
NoOf1stSurvived
stSurvivedPer <- (NoOf1stSurvived/NoOf1st)*100 
stSurvivedPer

#Part 4:percentage of 3rd class people survived
NoOf3rd <- length(titanic$Class[titanic$Class=='3rd'])
NoOf3rd
NoOf3rdSurvived <- length(titanic$Class[(titanic$Class=='3rd')& (titanic$Survived=='Yes')])
NoOf3rdSurvived
rdSurvivedPer <- (NoOf3rdSurvived/NoOf3rd)*100 
rdSurvivedPer

#######
#STEP3#
######
#Creating a function which will return a function with required parameters
selectValue<-function(a,b,c,d)
{
  index<-which(titanic$Sex==a & titanic$Age==b & titanic$Class==c & titanic$Survived==d)
  return(titanic[index,])
}
selectValue("Male","Adult","1st","No")


percentSelectValue<-function(a,b,c)
{
  firstValue<-nrow(selectValue(a,b,c,"Yes"))
  secondValue<-nrow(selectValue(a,b,c,"No"))
  percentWhoLives<-firstValue*100/(firstValue+secondValue)
  percentWhoDies<-secondValue*100/(firstValue+secondValue)
  return(c("Percentage of people who live=", percentWhoLives, "Percentage of People who die=", percentWhoDies))
}

#Part3:
percentSelectValue("Male","Adult","3rd")
percentSelectValue("Male","Child","3rd")
#Part 4:
percentSelectValue("Female","Adult","1st")
percentSelectValue("Female","Child","1st")



install.packages("arules")
library("arules")
install.packages("arulesViz")
library("arulesViz")

# Step 4: Use aRules
#Calculate some association rules using apriori() command 
summary(titanic)
rules<-apriori(titanic,parameter=list(support=0.005,confidence=0.8))
summary(rules)             
inspect(rules)

#2: Visualize results

plot(rules)

# refining Best rules
# Sort the list of previously created rules by putting support > 0.09 filter
bRules <- rules[quality(rules)$support > 0.09]
bRules
inspect(bRules)

# You can see it has support on x axis, confidence on y axis and color of dots representing value of lift
plot(bRules)
#You can see color as lift and support as the size
plot(bRules, method="graph", control=list(type="items"))

#3: Interesting results which I found
#1 {Sex=Female,Survived=Yes}          => {Age=Adult} 0.14357110 0.9186047  0.9664669
#2 {Class=3rd,Age=Adult,Survived=No}  => {Sex=Male}    0.17582917 0.8130252  1.0337773
#3 {Class=Crew}                       => {Sex=Male}    0.39164016 0.9740113  1.2384742
#4 {Survived=No}                      => {Age=Adult} 0.65333939 0.9651007  1.0153856

#4: Comparing the above results with previous desriptive analysis
# Checking the few rules using previous functions created 
percentSelectValue("Female","Adult","2nd")
percentSelectValue("Female","Child","2nd")
# As we can see the previous analysis are in tune with the association rules created

