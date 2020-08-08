# Goal is to find out best rules.. 
# rules means subset like {Milk,sugar} -> Butter
#install the library first from right 

library(arules)
library(arulesViz)
library(readxl)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(plyr)

# Read  Main excel file 
duds <- read_excel('/Users/anik_chowdhury/Downloads/online-retail-mba/MBA/sample_data.xlsx')
# summary of main file
summary(duds)
glimpse(duds)
duds
#Convert column as factor and bind it
duds %>% mutate(Description = as.factor(Description))
InvoiceNo <- as.numeric(as.character(duds$InvoiceNo))
cbind(duds,InvoiceNo)
glimpse(duds)
#ONly need INvoice No and Items(Here items is called description)
transactionData <- ddply(duds,c("InvoiceNo"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))
#Now invoice No is set to Null
transactionData$InvoiceNo<- NULL
transactionData
#Now, Save the transaction data based on description.
#location apni jekan e rakben oita set koren.....
#file name same thakbe datas1.csv
write.csv(transactionData,"/Users/anik_chowdhury/Downloads/online-retail-mba/MBA/datas1.csv", quote = FALSE, row.names = FALSE)
#Open the new file 
tr<-read.transactions(file.choose())
inspect(tr)
summary(tr)
# duplicate value remove
# ekhon dorkar nai eta run korar.. already kora acey.. 
#t[!is.na(t)]
#na.omit(t)
#d <- df[! duplicated(df[c("Shirt")]),]


#PLot the top 10 products
itemFrequencyPlot(tr,topN = 10,type = "relative",horiz = TRUE,cex.names = 0.7)
# PLot the products from bottom
barplot(sort(itemFrequency(tr), decreasing=TRUE),horiz = TRUE,cex.names = 0.7)

#check image function by own
image(tr[1:2])
image(sample(tr, 40))
dev.off() #when plot does not work, call it
#see every support and conf of every products
itemFrequency(tr)

#set the parameter min.support and min.confidence
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
length(association.rules)
#remove the redundant rules
# rules means subset like {Milk,sugar} -> Butter
association.rules<-association.rules[!is.redundant(association.rules)]
length(association.rules)
# rules number 10 inspectation
inspect(association.rules[10])
#Plot rules in scatter view
plot(association.rules)
#plot rules in graph view
plot(association.rules,method = "graph", control = list(cex = 0.7))
#Inspect top  1 to 10 rules
inspect(association.rules[1:10])
inspect(subset(association.rules,rhs %in% c("whole milk")))
inspect(subset(association.rules,lhs %ain% c("Black Leather Formal Belt,Casual Short Sleeve White Check Shirt")))

#inspect(association.rules[1:5])
summary(association.rules)
inspect(sort(association.rules, by ="support")[1:5])

