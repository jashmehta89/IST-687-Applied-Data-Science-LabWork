#Jash Mehta
#Homework Info Viz

#Installing package ggplot2
install.packages("ggplot2")
library(ggplot2)
require(ggplot2)



#Loading mtcars  data into mtc
mtc <- mtcars
mtc

str(mtc)

#Step 3 A
#Histogram for mpg 
a<-ggplot(mtc, aes(x=mpg)) + geom_histogram(binwidth=5,color="black", fill="white")
a

#Boxplot of mpg by cyl
b<-ggplot(mtc,aes(factor(cyl),mpg))  +geom_boxplot()
b


#MultiLine chart 
c=ggplot(mtc, aes(x=wt, y=mpg, group=am)) +geom_line (aes(color=am))+geom_point()
c

#Bar chart
d<-ggplot(mtc,aes((x=rownames(mtc)), y=wt,group=1))
d<-d+geom_bar(stat="identity")
d<-d+theme(axis.text.x=element_text(angle=90,hjust=1))
d


#Scatter plot
e<-ggplot(mtc,aes(x=mpg,y=wt))
e<-e+geom_point(aes(size=qsec,color=qsec))
e

#Creating the heatmap
heat<-ggplot(mtc, aes(y=cyl,x=wt))
heat1<-heat+geom_tile(aes(fill=mpg))+scale_fill_gradient(low="white",high="orange")+xlab("")+ylab("days")
heat1
