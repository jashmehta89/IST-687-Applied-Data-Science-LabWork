#Jash Mehta
#HW Association
#4/6/2016

#Step 1
# Downloaded the term document matrix.Rdata file and opened it in RStudio
# Loading the data into a new data frame called Term

#Step 2
# Carrying out the transpose function in order to have container as rows and words as columns  
# Using t() command for transportation
#Transposing the dataset
Term <- termDocMatrix
view(Term)
Term
TransTerm <- t(Term)

#Step 3
install.packages("arules")
library("arules")

# Now using the apriori command to get a good set of rules for the data
# Setting the support to 0.05 and confidence to 0.4
rulesT<-apriori(TransTerm,parameter=list(support=0.05,confidence=0.4))
# Using summary function to inspect the set of rules created 
summary(rulesT)
inspect(rulesT)

# Visualize results

install.packages("arulesViz")
library("arulesViz")

plot(rulesT)

plot(rulesT, method="graph")

#Step 4

# As we can see here words like time and series occur together quite frequently also social, analysis and network which makes a lot of sense

# How can we use this technique for large set of documents
# Retail outlets and E-commerce websites keep track of customer activity and product search
# This technique can be used to perform healthcare analysis - patients, drugs they take, diagnosis, adverse events
# Using this technique, security agencies track suspicious activities across the globe - monitoring emails
# This technique can also be used to sort the emails whether they go in junk folder or important folder