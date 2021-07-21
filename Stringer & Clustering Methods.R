install.packages("stringr")
install.packages("tidyverse")
require(tidyverse)
require(stringr)
require(tidyr)
require(dplyr)
require(ggplot2)

#Handling Strings
string_vec = c("ISM 645: Principles of Predictive Analytics", "IAF 601: Introduction to Data Analytics")

str_length(string_vec)
length(string_vec)

str_c("UNC ", "Greensboro")
str_c("UNCG ", string_vec)

str_split("UNC Greensboro", " ")
str_split(string_vec, ":")

str_sub(string_vec, 1,3)

str_to_upper(string_vec)
str_to_lower(string_vec)
str_to_title(string_vec)

course = c("ISM 5", "ISM 11", "ISM 645")
str_length(course)

course_padding = str_pad(course, 7, side = "right")
course_padding
str_length(course_padding)

whitespace1 = c("    white", "space     ")
whitespace2 = c("ISM    645", "    IAF    601    ")

str_trim(whitespace1)
str_trim(whitespace2)
str_squish(whitespace1)
str_squish(whitespace2)

str_detect(string_vec, "ISM")
str_count(string_vec, "ISM")
str_subset(string_vec, "ISM")
str_locate(string_vec, "ISM")
str_locate_all("ISM ISM","ISM")           

str_extract(string_vec, "ISM")

str_replace(string_vec, "ISM", "ISSCM")
str_replace_all(string_vec, "ISM", "ISSCM")

str_detect(string_vec, "I.")
str_count(string_vec, "I.")
str_subset(string_vec, "I.")
str_locate(string_vec, "I.")
str_locate_all(string_vec, "I.")


phone_book = c("apple", "219 733 8965", "329-293-8753", "Work: 579-499-7524; Home: 543.355.3679")
phone_format = "[2-9][0-9]{2}[-. ][0-9]{3}[-. ][0-9]{4}"
phone_format

str_detect("211.111.1111", phone_format)
str_detect("211 111 1111", phone_format)

str_replace(phone_book, phone_format, "XXX-XXX-XXXX")
str_replace_all(phone_book, phone_format, "XXX-XXX-XXXX")

str_count(phone_book, phone_format)

phrase = 'I love to work with strings'
cities = c("Chicago", "San Fran", "Miami")
Rise = "   Rise Academy    "
string1 = "What is"
string2 = "your name?"
str_length(phrase)
str_length(cities)
str_detect(phrase, "love to")
str_detect(phrase, "love   to")
str_trim(Rise)
str_c(string1, string2, sep = " ")
str_replace_all(phrase, "I", "")

#Handling Date and Time
require(lubridate)

now()
today()

date_time1 = "2020-10-1 18:30:00"
date_time2 = "10-1-2020 18:30:00"
date_time3 = "1-10-2020 18:30"

date_vec = c(ymd_hms(date_time1), mdy_hms(date_time2), dmy_hm(date_time3))

year(date_vec)  
month(date_vec)             
day(date_vec)
yday(date_vec)
wday(date_vec)
hour(date_vec)
minute(date_vec)
second(date_vec)

date_vec + months(1)
date_vec + days(7)
date_vec + hours(3)

round_date(date_vec, unit = "year")
round_date(date_vec, unit = "month")

floor_date(date_vec, unit = "year")
ceiling_date(date_vec, unit = "year")



# K-Means Clustering
library(tidyverse)
setwd("/Users/JackVoigt/Documents/MSIA Files/Datasets")
my_countries = read.csv("countries.csv")
head(mycountries)
count(my_countries)

my_countries10 = my_countries%>%
  filter(year==2010)
head(my_countries10)
count(my_countries10)

kmeans(my_countries10, centers = 4, iter.max = 25, nstart = 10)

#Drop all missing values

my_countries2 = my_countries%>%
  drop_na()

summary(my_countries10)
summary(my_countries2)

kmeans(my_countries2, centers = 4, iter.max = 25, nstart = 10)

str(my_countries2)

kmeans_result = kmeans(my_countries2[, c("infant_mortality", "life_expectancy", "fertility_rate", "population", "gdp")], 
       centers = 4, iter.max = 100, nstart = 10)

kmeans_result$cluster

my_countries2 = my_countries2%>%
  mutate(cluster_kmeans = kmeans_result$cluster)

str(my_countries2)


#Interpreting Clusters
library(ggrepel)
library(ggplot2)

my_countries2 %>%
  group_by(cluster_kmeans)%>%
  summarize(num_obs = n(), avg_infant_mort = mean(infant_mortality), 
            avg_life_expec = mean(life_expectancy), avg_fertility = mean(fertility_rate),
            avg_pop = mean(population), avg_gdp = mean(gdp))

ggplot(my_countries2, aes(x = gdp, y = life_expectancy)) +
  geom_point(aes(color = as.factor(cluster_kmeans))) +
  geom_text_repel(aes(label = country),force = 5, data = subset(my_countries2, cluster_kmeans %in% c(1,3,4)))

ggplot(my_countries2, aes(x = gdp, y = life_expectancy)) +
  geom_point(aes(color = as.factor(cluster_kmeans))) +
  geom_text_repel(aes(label = country),force = 5, data = subset(my_countries2, cluster_kmeans %in% c(1,3,4)))+
  scale_x_log10()+
  scale_y_log10()

install.packages('factoextra')
require(factoextra)
#Elbow Method

fviz_nbclust(my_countries2[ , c("infant_mortality", "life_expectancy", "fertility_rate", "population", "gdp")],
             kmeans, method = "wss") +
  labs(subtitles = "Elbow method")

fviz_nbclust(my_countries2[ , c("infant_mortality", "life_expectancy", "fertility_rate", "population", "gdp")],
             kmeans, method = "silhouette") +
  labs(subtitles = "Silhouette Method")



#Hierarchical Clustering

d_matrix = dist(my_countries2[ , c("infant_mortality", "life_expectancy", "fertility_rate", "population", "gdp")],
     method = "euclidean")

hc_single = hclust(d_matrix, method = "single")

plot(hc_single)

hc_result = cutree(hc_single, k = 4)

my_countries2 = my_countries2%>%
  mutate(cluster_hc = hc_result)

str(my_countries2)

ggplot(my_countries2, aes(x = gdp, y = life_expectancy)) +
  geom_point(aes(color = as.factor(cluster_hc))) +
  geom_text_repel(aes(label = country),force = 5, data = subset(my_countries2, cluster_hc %in% c(2,3,4)))+
  scale_x_log10()

summary(my_countries2) 

#Normalize the data
my_countries3 = my_countries2[ , c("infant_mortality", "life_expectancy", "fertility_rate", "population", "gdp")]%>%
  scale()%>%
  data.frame()



#Hands On Lab
library(tidyverse)
mtcars = read.csv("mtcars.csv")
str(mtcars)
head(mtcars)
summary(mtcars)

#Cluster the vehicles using kmeans and hierarchial

#k-means
mtcars = mtcars%>%
  drop_na()

kmeans_result = kmeans(mtcars[, c(-1,-2)], centers = 4, iter.max = 25, nstart = 10)
kmeans_result
kmeans_result$cluster

mtcars = mtcars%>%
  mutate(cluster_kmeans = kmeans_result$cluster)

str(mtcars)

#Heiarchical clustering

d_matrix = dist(mtcars[, c(-1,-2)], method = "euclidean")

hc_result = hclust(d_matrix, method = "single")

plot(hc_result)

hc_cluster = cutree(hc_result, k = 4)
hc_cluster

mtcars = mtcars%>%
  mutate(cluster_hc = hc_cluster)

str(mtcars)


#Summarize characteristics of each cluster
library(ggrepel)
mtcars %>%
  group_by(cluster_kmeans)%>%
  summarize(num_obs = n(), avg_mpg = mean(mpg), avg_hp = mean(hp))

ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point(aes(color = as.factor(cluster_kmeans))) +
  labs(title = "K-means clustering") +
  geom_text_repel(aes(label = model), force = 10)

#Find optimal number of clusters
library(factoextra)  

fviz_nbclust(mtcars[, c(-1,-2)], kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")


fviz_nbclust(mtcars[, c(-1,-2)], kmeans, method = "silhouette") +
  labs(subtitle = 'Silhouete Method')

kmeans_result2 = kmeans(mtcars[, c(-1,-2)], centers = 2, iter.max = 25, nstart = 10)

mtcars = mtcars%>%
  mutate(cluster_kmeans2 = kmeans_result2$cluster)

ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point(aes(color = as.factor(cluster_kmeans2))) +
  labs(title = "K-means clustering") +
  geom_text_repel(aes(label = model), force = 10)
