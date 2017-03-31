#Jash Mehta
#IST 687 Text Mining Lab 


#Installing package tm
install.packages("tm")
library("tm")

install.packages("wordcloud")
library("wordcloud")

#Scanning all positive words
scp <- scan("C:\\Users\\jashm\\Google Drive\\IM\\687\\Home Work\\TM\\positive.txt", sep = "\n", character(0)) 
scp
head(scp,35)

#Cleaning of 34 rows of positive words
cleanscp <- scp[-1:-34]

head(cleanscp,20)


#Scanning all negative words
scn <- scan("C:\\Users\\jashm\\Google Drive\\IM\\687\\Home Work\\TM\\negative.txt", sep = "\n", character(0)) 
scn
head(scn,40)
#Cleaning of first 34 rows
cleanscn <- scn[-1:-34]
head(cleanscn,20)

#Reading the MLK text file using readLines functions  
sba <- readLines("C:\\Users\\jashm\\Google Drive\\IM\\687\\Home Work\\TM\\MLK.txt")
sba
str(sba)

#Building a Corpus of words
words.vec <- VectorSource(sba)
words.corpus <- Corpus(words.vec)
words.corpus

#Making the words lowercase, removing punctuation, Removing numbers, Removing stop words
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))


#Creating a Term document matrix
tdm <- TermDocumentMatrix(words.corpus)
tdm

m <- as.matrix(tdm)

#Calculating total number of words in the MLK file
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)
#cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)
totalWords <- sum(wordCounts)
words <- names(wordCounts)
totalWords <- length(words)
totalWords

#Matching the postive words
matched <- match(words, cleanscp, nomatch = 0)
head(matched,10)

matched1 <- match(words, cleanscn, nomatch = 0)
head(matched1,10)

#Couting of positive words
mCounts <- wordCounts[which(matched != 0)]
length(mCounts)
#mWords <- names(mCounts)
nPos <- sum(mCounts)
nPos

#Matching and Counting the negative words
mCounts1 <- wordCounts[which(matched1 != 0)]
length(mCounts1)
#mWords <- names(mCounts)
nNeg <- sum(mCounts1)
nNeg

#Taking ratio of postitive and negative words
RatioPos <- nPos/totalWords
RatioPos
RatioNeg <- nNeg/totalWords
RatioNeg


#st <- NULL
#for(i in 1:length(matched))
#{
#  if(matched[i]!=0)
 #    st[i]<- cleanscp[matched[i],]
#}
#st


#matched1 <- match(words, cleanscn, nomatch = 0)
#matched1

#st1 <- NULL
#for(i in 1:length(matched))
#{
  #if(matched1[i]!=0)
 #   st1[i]<- cleanscn[matched1[i]]
#}
#st1


#Dividing the SBA into 4 parts so that it will be 25% each approximately
sba1 <- sba[1:9]
sba2 <- sba[10:16]
sba3 <- sba[17:24]
sba4 <- sba[25:31]

#NOTE: I am not writing the comments again as the process is same as above
words.vec1 <- VectorSource(sba1)
words.corpus1 <- Corpus(words.vec1)
words.corpus1

words.corpus1 <- tm_map(words.corpus1, content_transformer(tolower))
words.corpus1 <- tm_map(words.corpus1, removePunctuation)
words.corpus1 <- tm_map(words.corpus1, removeNumbers)
words.corpus1 <- tm_map(words.corpus1, removeWords, stopwords("english"))

tdm1 <- TermDocumentMatrix(words.corpus1)
tdm1

m1 <- as.matrix(tdm1)
wordCounts1 <- rowSums(m1)
wordCounts1 <- sort(wordCounts1, decreasing=TRUE)
head(wordCounts1)


#cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)


totalWords1 <- sum(wordCounts1)
words1 <- names(wordCounts1)

totalWords1 <- length(words1)
totalWords1

matchedsba1p <- match(words1, cleanscp, nomatch = 0)
head(matchedsba1p,10)

matchedsba1n <- match(words1, cleanscn, nomatch = 0)
head(matchedsba1n,10)

mCountssba1p <- wordCounts1[which(matchedsba1p != 0)]
length(mCountssba1p)
#mWords <- names(mCounts)
nPos1 <- sum(mCountssba1p)
nPos1


mCountssba1n <- wordCounts1[which(matchedsba1n != 0)]
length(mCountssba1n)
#mWords <- names(mCounts)
nNeg1 <- sum(mCountssba1n)
nNeg1

RatioPos1 <- nPos1/totalWords1
RatioPos1
RatioNeg1 <- nNeg1/totalWords1
RatioNeg1

Ratio1 <- abs(RatioPos1 - RatioNeg1)





words.vec2 <- VectorSource(sba2)
words.corpus2 <- Corpus(words.vec2)
words.corpus2

words.corpus2 <- tm_map(words.corpus2, content_transformer(tolower))
words.corpus2 <- tm_map(words.corpus2, removePunctuation)
words.corpus2 <- tm_map(words.corpus2, removeNumbers)
words.corpus2 <- tm_map(words.corpus2, removeWords, stopwords("english"))

tdm2 <- TermDocumentMatrix(words.corpus2)
tdm2

m2 <- as.matrix(tdm2)
wordCounts2 <- rowSums(m2)
wordCounts2 <- sort(wordCounts2, decreasing=TRUE)
head(wordCounts2)


#cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)


totalWords2 <- sum(wordCounts2)
words2 <- names(wordCounts2)

totalWords2 <- length(words2)
totalWords2

matchedsba2p <- match(words2, cleanscp, nomatch = 0)
head(matchedsba2p,10)

matchedsba2n <- match(words2, cleanscn, nomatch = 0)
head(matchedsba2n,10)

mCountssba2p <- wordCounts2[which(matchedsba2p != 0)]
length(mCountssba2p)
#mWords <- names(mCounts)
nPos2 <- sum(mCountssba2p)
nPos2


mCountssba2n <- wordCounts2[which(matchedsba2n != 0)]
length(mCountssba2n)
#mWords <- names(mCounts)
nNeg2 <- sum(mCountssba2n)
nNeg2

RatioPos2 <- nPos2/totalWords2
RatioPos2
RatioNeg2 <- nNeg2/totalWords2
RatioNeg2

Ratio2<- abs(RatioPos2 - RatioNeg2)








words.vec3 <- VectorSource(sba3)
words.corpus3 <- Corpus(words.vec3)
words.corpus3

words.corpus3 <- tm_map(words.corpus3, content_transformer(tolower))
words.corpus3 <- tm_map(words.corpus3, removePunctuation)
words.corpus3 <- tm_map(words.corpus3, removeNumbers)
words.corpus3 <- tm_map(words.corpus3, removeWords, stopwords("english"))

tdm3 <- TermDocumentMatrix(words.corpus3)
tdm3

m3 <- as.matrix(tdm3)
wordCounts3 <- rowSums(m3)
wordCounts3 <- sort(wordCounts3, decreasing=TRUE)
head(wordCounts3)


#cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)


totalWords3 <- sum(wordCounts3)
words3 <- names(wordCounts3)

totalWords3 <- length(words3)
totalWords3

matchedsba3p <- match(words3, cleanscp, nomatch = 0)
head(matchedsba3p,10)

matchedsba3n <- match(words3, cleanscn, nomatch = 0)
head(matchedsba3n,10)

mCountssba3p <- wordCounts3[which(matchedsba3p != 0)]
length(mCountssba3p)
#mWords <- names(mCounts)
nPos3 <- sum(mCountssba3p)
nPos3


mCountssba3n <- wordCounts3[which(matchedsba3n != 0)]
length(mCountssba3n)
#mWords <- names(mCounts)
nNeg3 <- sum(mCountssba3n)
nNeg3

RatioPos3 <- nPos3/totalWords3
RatioPos3
RatioNeg3 <- nNeg3/totalWords3
RatioNeg3

Ratio3 <- RatioPos3 - RatioNeg3










words.vec4 <- VectorSource(sba4)
words.corpus4 <- Corpus(words.vec4)
words.corpus4

words.corpus4 <- tm_map(words.corpus4, content_transformer(tolower))
words.corpus4 <- tm_map(words.corpus4, removePunctuation)
words.corpus4 <- tm_map(words.corpus4, removeNumbers)
words.corpus4 <- tm_map(words.corpus4, removeWords, stopwords("english"))

tdm4 <- TermDocumentMatrix(words.corpus4)
tdm4

m4 <- as.matrix(tdm4)
wordCounts4 <- rowSums(m4)
wordCounts4 <- sort(wordCounts4, decreasing=TRUE)
head(wordCounts4)


#cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)


totalWords4 <- sum(wordCounts4)
words4 <- names(wordCounts4)

totalWords4 <- length(words4)
totalWords4

matchedsba4p <- match(words4, cleanscp, nomatch = 0)
head(matchedsba4p,10)

matchedsba4n <- match(words4, cleanscn, nomatch = 0)
head(matchedsba4n,10)

mCountssba4p <- wordCounts4[which(matchedsba4p != 0)]
length(mCountssba4p)
#mWords <- names(mCounts)
nPos4 <- sum(mCountssba4p)
nPos4


mCountssba4n <- wordCounts4[which(matchedsba4n != 0)]
length(mCountssba4n)
#mWords <- names(mCounts)
nNeg4 <- sum(mCountssba4n)
nNeg4

RatioPos4 <- nPos4/totalWords4
RatioPos4
RatioNeg4 <- nNeg4/totalWords4
RatioNeg4

Ratio4 <- RatioPos - RatioNeg

#plotting a barplot
Ratios <- c(Ratio1, Ratio2, Ratio3, Ratio4) 
data.frame(Ratios)
View(Ratios)



barplot(Ratios, xlab="Ratios")

