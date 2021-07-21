require(tidyverse)
install.packages('arules')
require(arules)
mybasket1 = read.transactions("GroceryStore_Basket.csv", format = 'basket', sep = ",")
mybasket2 = read.transactions("GroceryStore_Single.csv", format = "single", sep = ",",cols = c("TransactionID", "Item"), header = T)

inspect(mybasket1)
inspect(mybasket2)

summary(mybasket1)
summary(mybasket2)

itemFrequencyPlot(mybasket1)
itemFrequencyPlot(mybasket2)

rules1 = apriori(mybasket2, parameter = list(supp = 0.01, conf = 0.8, maxlen = 4, minlen = 2))

summary(rules)
inspect(rules)

rules = sort(rules, by = "support")

inspect(rules)

is.redundant(rules)
rules[is.redundant(rules)]
rules[!is.redundant(rules)]


install.packages('arulesViz')
require(arulesViz)

plot(rules)
plot(rules, method = "graph")
plot(rules[1:10], method = "graph")

bread_rule = apriori(mybasket2, parameter = list(supp = 0.01, 
conf = 0.5, maxlen = 4, minlen = 2),
appearance = list(default = "lhs", rhs = "BREAD"))

bread_rule = sort(bread_rule, by = "confidence", decreasing = T)

inspect(bread_rule)

bread_rule = bread_rule[!is.redundant(bread_rule)]

inspect(bread_rule)

#Bread Rule 2, Bread is the lhs
bread_rule2 = apriori(mybasket2, parameter = list(supp = 0.01, 
conf = 0.1, maxlen = 4, minlen = 2),
appearance = list(default = "rhs", lhs = "BREAD"))

inspect(bread_rule2)


#Hands on 
transaction = read.transactions("Online Retail_Small.csv", format = 'single', sep = ",",
                           cols = c("InvoiceNo", "Description"), header = T)

inspect(transaction[1:5])

summary(transaction)

rules = apriori(transaction, parameter = list(supp = 0.02, conf = 0.2, maxlen = 4, minlen = 2))

summary(rules)
inspect(rules)

rules = sort(rules, by = "confidence", decreasing = T)
inspect(rules[1:5])


is.redundant(rules)
rules[is.redundant(rules)]
