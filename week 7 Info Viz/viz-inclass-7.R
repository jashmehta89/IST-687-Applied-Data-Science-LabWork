
install.packages("ggplot2")
library(ggplot2)


#The concept behind ggplot2 divides plot into three different fundamental parts: Plot = data + Aesthetics + Geometry.

#the principal components of every plot can be defined as follow:

#data is a data frame
#Aesthetics is used to indicate x and y variables. It can also be used to control the color, the size or the shape of points, the height of bars, etc…..
#Geometry defines the type of graphics (histogram, box plot, line plot, density plot, dot plot, ….)


#read in the data - use the function we previously created
dfStates <- readStates()

#remove 'dot' from state name
library(stringr)
dfStates$state <- str_replace(dfStates$state,"\\.","") 

#calculate %change and pop change
dfStates$popChanges <- dfStates$base2011Num - dfStates$base2010Num
dfStates$PercentChange <- dfStates$popChange/dfStates$base2010Num * 100

#histograms
ggplot(dfStates, aes(x=base2010Num)) + geom_histogram(color="black", fill="white")
ggplot(dfStates, aes(x=base2011Num)) + geom_histogram(binwidth=1000000, color="black", fill="white")
ggplot(dfStates, aes(x=popChanges)) + geom_histogram(binwidth=50000, color="black", fill="white")
ggplot(dfStates, aes(x=PercentChange)) + geom_histogram(binwidth=0.1, color="black", fill="white")


#boxplots
ggplot(dfStates,aes(x=factor(0),base2010Num))+geom_boxplot()
ggplot(dfStates,aes(x=factor(0),base2011Num))+geom_boxplot()
ggplot(dfStates,aes(x=factor(0),popChanges))+geom_boxplot()
ggplot(dfStates,aes(x=factor(0),PercentChange))+geom_boxplot()





#create data - how long it takes to get to NYC
timeToNYC <- c(4,4.5,3.5,5,4,4.2)
timeToNYCWeek2 <- c(4.5,5,3.8,5.2,4.6,4.3)
day <- c("mon","tues","wed","thurs","fri","sat")
week1 <- c(1,1,1,1,1,1)
week2 <- c(2,2,2,2,2,2)
time <- c(timeToNYC, timeToNYCWeek2)
week <- as.factor(c(week1, week2))
dayOfWeek <- c(day,day)
travel.df <- data.frame(dayOfWeek, time, week)

######################
# histograms
######################

hist(travel.df$time,breaks=4)
ggplot(travel.df, aes(x=time)) + geom_histogram(binwidth=1)

# Draw with black outline, white fill
ggplot(travel.df, aes(x=time)) + geom_histogram(binwidth=1, color="black", fill="white")

#create the chart in a variable, and then add a title
g <- ggplot(travel.df, aes(x=time)) 
g <- g + geom_histogram(binwidth=1, color="black", fill="white")
g <- g + ggtitle("time buckets")
g



#################
# boxplot
#################
ggplot(travel.df,aes(x=factor(0),time))+geom_boxplot()
ggplot(travel.df,aes(x=factor(week),time))+geom_boxplot()
ggplot(travel.df,aes(x=factor(week),time))+geom_boxplot() + coord_flip()




######################
#basic line chart - some simple observations
###########################
g <- ggplot(travel.df,aes(x=day,y=timeToNYC, group=1)) + stat_summary(fun.y=identity,geom="line") 
g

ggplot(travel.df,aes(x=dayOfWeek,y=time, group=1)) + geom_line() 


g <- ggplot(travel.df,aes(x=dayOfWeek,y=time, group=1)) + 
  stat_summary(fun.y=identity,geom="line", color="red",linetype="dashed", size=1.5) 
g

g <- ggplot(travel.df,aes(x=dayOfWeek,y=time, group=1)) + geom_line() 
g <- g + geom_point(colour="black", size=4, shape=21, fill="white")
g

g <- g + ylab("time to NYC (in hours)")
g


#show multiple lines in one chart
g <- ggplot(travel.df, aes(x = dayOfWeek, group=week, color=week)) + geom_line(aes(y = time))
g 
g <- g + geom_point(y=time, colour="black", size=4, shape=21, fill="white")
g
g <- g + ylab("time to NYC (in hours)") + ggtitle("compare weekly times")
g




################################
#basic barchart - explore mtcars
################################

#barcharts
g <- ggplot(dfStates,aes(x=state, y=base2010Num)) + geom_bar(stat="identity")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g



ggplot(travel.df, aes(x = dayOfWeek, fill = factor(week))) + geom_bar()
ggplot(travel.df, aes(x = dayOfWeek, y=time)) + geom_bar(stat="identity")
ggplot(travel.df, aes(x = dayOfWeek, y=time, fill = factor(week))) + geom_bar(stat="identity")
ggplot(travel.df, aes(x = dayOfWeek, y=time, fill = factor(week))) + geom_bar(stat="identity",position="dodge")

firstWeek.df <- travel.df[week==1,]
ggplot(firstWeek.df, aes(x = dayOfWeek, y=time)) + geom_bar(stat="identity")



##############
#scatter plot
##############
ggplot(travel.df, aes(x=dayOfWeek, y=time)) + geom_point()

#add size of bubble
ggplot(travel.df, aes(x=dayOfWeek, y=time)) + geom_point(aes(size = time)) 

#add color of bubble
g <- ggplot(travel.df, aes(x=dayOfWeek, y=time)) + geom_point(aes(size = time, color=week)) 
g

#add some text
g <- g  + geom_text(aes(label=dayOfWeek), size=3)
g



#DO US sates - add size & color of bubble - 
g <- ggplot(dfStates, aes(x=popChanges, y=PercentChange))+ geom_point(aes(size = base2010Num, color=base2010Num)) 
g

#add some text  
g <- g  + geom_text(aes(label=state), size=3)
g


##################
# heatmap
####################

melted.df <- melt(travel.df)
melted.df <- melt(travel.df,id=c("week","dayOfWeek"))
p <- ggplot(melted.df, aes(x=week, y=dayOfWeek))
g <- p + geom_tile(aes(fill=value)) + scale_fill_gradient(low="white", high="darkblue") + xlab("") + ylab("")
g + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#look at states --  


library(reshape2)
dat.m <- melt(dfStates,id.vars='state', measure.vars=c('base2010Num','base2011Num'))
#dat.m <- melt(dfStates,id.vars='ID', measure.vars=c('base2010Num','base2011Num'))
library(ggplot2)
p <- ggplot(dat.m) +
  geom_boxplot(aes(x=factor(0), y=value, color=variable))
p


pc <- as.factor(round(dfStates$PercentChange, digits=1))
g <- ggplot(dfStates,aes(x=state, y=base2010Num, fill=pc, color=pc)) + geom_bar(stat="identity", position="dodge")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g















###################################
# put all the code required for reading the states csv file into this function
#######################################
readStates <- function() {
  #read a cvs file into a dataframe
  urlToRead <- "http://www.census.gov/popest/data/state/totals/2011/tables/NST-EST2011-01.csv"
  dfStates <- read.csv(url(urlToRead))
  
  #now have to fix up the mess
  #dfStates[1,]
  
  #str(dfStates)
  
  #remove the last 5 columns (all NAs)
  #dfStates[1,]
  #length(dfStates[1,])
  #dfStates[1,6]
  #dfStates[1,7]
  #dfStates[1,8]
  #dfStates[1,9]
  #dfStates[1,10]
  dfStates <- dfStates[,1:5]
  #dfStates
  #str(dfStates)
  
  
  #remove the header stuff in front
  head(dfStates,5)
  
  #dfStates[5,]
  #see that the 9'th row is the first state
  #dfStates[9,]
  
  dfStates <- dfStates[-1:-8,]
  #dfStates
  
  #update header names
  colnames(dfStates) <- c("state", "Jul2010", "Jul2011", "base2010", "base2011" )
  #dfStates[1,]
  #head(dfStates)
  
  #how about the end 
  #tail(dfStates)
  #nrow(dfStates)
  dfStates <- dfStates[1:51,]
  
  #tail(dfStates)
  #dfStates
  #dfStates$base2011
  #why does this line cause an error?
  #mean(dfStates$Jul2011)
  
  #str(dfStates)
  dfStates$base2011Num <- as.numeric(gsub(",","", dfStates$base2011))
  dfStates$base2010Num <- as.numeric(gsub(",","", dfStates$base2010))
  
  return (dfStates)
}