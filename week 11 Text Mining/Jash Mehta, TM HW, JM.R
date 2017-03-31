#Jash Mehta
#IST687 Text Mining HW

#Installing tm package to provide functions for text mining
install.packages("tm")
library(tm)


#Scanning all positive words
#p <- readLines("C:\\Users\\jashm\\Google Drive\\IM\\687\\Home Work\\TM\\AFINN.txt")# sep = "\t", character(0)) 
#p <- as.character(p)
#scanAfinn
#head(p,5)
#a=NULL
#b=NULL

#p <- data.frame(p)
#p

#p <- as.character(p)

#p <- data.frame(do.call(rbind,strsplit(as.vector(p$p),split="\t")))
#p

#sba <- readLines("C:\\Users\\jashm\\Google Drive\\IM\\687\\Home Work\\TM\\MLK.txt")
#sba
#str(sba)

#CODE STARTS Here

#Step 1: Importing AFINN into r and creating two vectors
#Creating a path to my AFINN list
Apath <- scan("C:\\Users\\jashm\\Google Drive\\IM\\687\\Home Work\\TM\\AFINN.txt", character(0),sep = "\n")


#Separate words and values
Apath <- strsplit(Apath, "\t")
Apath <- unlist(Apath)
#Make two columns, first column for the words and second column for the values
Apath <- split(Apath, c(1,2))

#Create a two new vectors for the words and the scores
Awords <- unlist(Apath[1],use.names = FALSE)
AScores <- unlist(Apath[2],use.names = FALSE)

#Step 2: importing the mlk speech and computing the overall score of the speech 
#Creating a path to my MLK speech
MLK <- "C:\\Users\\jashm\\Google Drive\\IM\\687\\Home Work\\TM\\MLK.txt"
#Importing mlk speech
sba <- scan(MLK, character(0),sep = "\n")
#Removing  punctuation
sba <- gsub("[[:punct:]]", "", sba)
#Break the lines up based on spaces
sba <- strsplit(sba, " ")
#Combine all lines into single vector of characters
sba <- unlist(sba)
sba
#Creating a vector of all the words in the speech
sba.vec <- VectorSource(sba)
sba.corp <- Corpus(sba.vec)
#Create matrix from the speech
sba.TDM <- TermDocumentMatrix(sba.corp)
sbaMatrix <- as.matrix(sba.TDM)
#Sort the words based on frequency
mlkwordCount <- rowSums(sbaMatrix)
mlkwordCount <- sort(mlkwordCount, decreasing = TRUE)
mlkwordCount
#mlkwords
#Create vector of just words in the speech to help compute the score of the speech
mlkwords <- names(mlkwordCount)
#Match words in the speech to AWords
mlkMatchedwords <- match(mlkwords,Awords,nomatch = 0)
#List scores for matched words
mlkmatchedScores <- as.numeric(AScores[mlkMatchedwords])
#List matched words and frequency
mlkmatchedCount <- mlkwordCount[which(mlkMatchedwords != 0)]
#Sum the product of each word and its scrore 
sum(mlkmatchedCount*mlkmatchedScores)


#Step 3: splitting the mlk speech into 4 equal parts and creating the fuction 
#Split the mlk speech into 4 equal parts
mlknumberOfWords <- length(sba)
groups <- ceiling(seq_along(sba)/(mlknumberOfWords/4))
partsOfSpeech <- split(sba,groups)
partsOfSpeech

#Creating a function to run for each 25%
speechScoreCalc <- function (sectionOfSpeech)
{
  section.vector <- VectorSource(sectionOfSpeech)
  section.corpus <- Corpus(section.vector)
  sectionTDM <- TermDocumentMatrix(section.corpus)
  sectionMatrix <- as.matrix(sectionTDM)
  sectionWordCount <- rowSums(sectionMatrix)
  sectionWords <- names(sectionWordCount)
  Matched <- match(sectionWords,Awords,nomatch = 0)
  matchedScores <- as.numeric(AScores[Matched])
  matchedCount <- sectionWordCount[which(Matched != 0)]
  finalScore <- sum(matchedCount*matchedScores)
  return (finalScore)
}

#Run the function for each section
firstSection <- speechScoreCalc (partsOfSpeech[1])
firstSection    
secondSection <- speechScoreCalc (partsOfSpeech[2])
secondSection   
thirdSection <- speechScoreCalc (partsOfSpeech[3])
thirdSection   
fourthSection <- speechScoreCalc (partsOfSpeech[4])
fourthSection

#Step 4: plotting the results in a barchart 

mlkallSections <- data.frame(firstSection,secondSection,thirdSection,fourthSection)
# Rows are positive and negative, columns are sections
row.names(mlkallSections) <- c("mlkScore")
# COnvert to matrix to plot
mlkallSections <- as.matrix(mlkallSections)
# Generate barplot to compare sections
barplot(mlkallSections, main = "Barplot for each 25%")

