library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)
setwd('/Users/JackVoigt/Documents/MSIA Files/Datasets')

retail <- read_excel('Online Retail.xlsx')
View(retail)

retail = slice(retail, -c(250000:600000))
retail <- retail[complete.cases(retail), ]

retail <- retail %>% mutate(Description = as.factor(Description))			   
retail <- retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))


#What is most common time to purchase?
retail$Time <- as.factor(retail$Time)
a <- hms(as.character(retail$Time))
retail$Time = hour(a)

retail %>% 
	ggplot(aes(x=Time)) + 
	geom_histogram(stat="count",fill="indianred")

#Number of items
detach("package:plyr", unload=TRUE)

retail %>% 
	group_by(InvoiceNo) %>% 
	summarize(n_items = mean(Quantity)) %>%
	ggplot(aes(x=n_items))+
	geom_histogram(fill="indianred", bins = 100000) + 
	geom_rug()+
	coord_cartesian(xlim=c(0,80))

#Top sales
tmp <- retail %>% 
	group_by(StockCode, Description) %>% 
	summarize(count = n()) %>% 
	arrange(desc(count))
tmp <- head(tmp, n=10)
tmp

tmp %>% 
	ggplot(aes(x=reorder(Description,count), y=count))+
	geom_bar(stat="identity",fill="indian red")+
	coord_flip()

#Transform to transactions
retail_sorted <- retail[order(retail$CustomerID),]

itemList <- ddply(retail,c("CustomerID","Date"), 
				  function(df1)paste(df1$Description, 
				  				   collapse = ","))

itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket1.csv", quote = FALSE, row.names = TRUE)


tr <- read.transactions('market_basket1.csv', format = 'basket', sep=',')
tr
summary(tr)


rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)



inspect(rules[0:10])
#If confidence is 1, 100% of people who bought x bought y

#Plot of top rules
topRules <- rules[1:10]
plot(topRules)
plot(topRules, method="graph")
