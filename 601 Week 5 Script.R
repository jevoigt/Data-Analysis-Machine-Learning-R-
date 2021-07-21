install.packages("tidyr")
require(tidyr)
library(tidyr)
library(dplyr)
library(stringr)
install.packages("scales")
library(scales)

ex1 = read.csv("example1.csv")
str(ex1)
head(ex1)
summary(ex1)
gather(ex1, "X1999", "X2000", key = "Year", value = "cases")
getwd()
ex1 = rename(ex1, "Country!!" = "country")


ex2 = read.csv("example2.csv")
str(ex2)
head(ex2, 30)
summary(ex2)
ex2 = spread(ex2, "type", "count") 

#remove "K" from cases
ex2$cases = gsub("K","",ex2$cases)

#Rename column
ex2 = ex2 %>%
	rename(CasesInK = cases)

#Add lots of cases
ex2$CasesInK = as.integer(ex2$CasesInK)
ex2 %>%
	mutate("LotsOfCases" = (CasesInK >= 200)) %>%
	View()

#Convert vales to thousands
ex2$CasesInK = as.integer(ex2$CasesInK)
ex2$CasesInK = as.character(ex2$CasesInK)


print(X)
ex3 = read.csv("example3.csv")
str(ex3)
head(ex3)
summary(ex3)
separate(ex3, "rate", into = c("cases", "pop"))
separate(ex3, "rate", into = c("cases", "pop"), sep = "/")

install.packages('openxlsx')
library(openxlsx)
ex4 = read.xlsx("example4.xlsx")
str(ex4)
head(ex4)
summary(ex4)
unite(ex4, 'century', 'year', col = 'year')
unite(ex4, 'century', 'year', col = 'year', sep = '')

library(dplyr)
messy1 = read.csv('messy_data1.csv')
head(messy1)
gather(messy1, "brown", "blue", "other", key = "eye color", value = "value") %>%
	filter(value == 1)%>%
	mutate(value = NULL)

messy2 = read.csv("messy_data2.csv")
head(messy2)
spread(messy2, "measurement", "value")

messy3 = read.csv("messy_data3.csv")
head(messy3)
separate(messy3, "sex_age", into =c("Sex", "Age"))

#Export
messy_data4 = separate(messy3, "sex_age", into =c("Sex", "Age"))
write.csv(messy_data4, "messy_data4.csv")



#Missing Data
ex5 = read.csv("example5.csv")
head(ex5)
summary(ex5)
drop_na(ex5)
fill(ex5, col2)
fill(ex5, col2, .direction = "up")

replace_na(ex5, list(col2 = 5))
replace_na(ex5, list(col2 = mean(ex5$col2)))
replace_na(ex5, list(col2 = mean(ex5$col2, na.rm = T)))

messy4 = read.csv("messy_data4.csv")
head(messy4)

messy4%>%
  mutate(age = if_else(Age>100, Age-100, as.double(Age))) %>%
  replace_na(list(eye_color = "Others", height = "Unknown", Age = 0))

table1 = read.csv("Week 5 Hands-on Table 1.csv")
table2 = read.csv("Week 5 Hands-on Table 2.csv")         
table3 = read.csv("Week 5 Hands-on Table 3.csv") 
table4 = read.csv("Week 5 Hands-on Table 4.csv")
table5 = read.csv("Week 5 Hands-on Table 5.csv") 

#Tidy 2
table2
spread(table2, "key", "value")

#Tidy 3
table3
separate(table3, "rate", into  =c("cases", "population"), sep = "/")

#Tidy 4
table4
gather(table4, "X1999", "X2000", key = "year", value = "cases")

