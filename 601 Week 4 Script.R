library(dplyr)
require(dplyr)
1+2 %>% +3 %>% +4
c(1,2,3,4,5) %>% sqrt %>% max
temp = c(30,0,12,40,28)
temp %>% 
  diff(lag = 2) %>%
  log %>% 
  round(digits=2)
  

round(log(diff(temp, lag = 2)), digits = 2)
mycountries = read.csv("countries.csv")
str(mycountries)
head(mycountries)
tail(mycountries)
summary(mycountries)
library(dplyr)

selected_df = mycountries %>%
  select(country, year, life_expectancy, gdp, population, continent)

str(selected_df)

filtered_df = mycountries %>%
  filter(year == 1970 | year == 2015)
head(filtered_df)
tail(filtered_df)
View(filtered_df)
#The same function
filtered_df = mycountries %>%
  filter(year %in% c(1970, 2010))

head(filtered_df)
tail(filtered_df)

distinct_df = mycountries %>%
  distinct(country, continent)

head(distinct_df)

#Arrange

sorted_df = mycountries %>%
  select(country, year, life_expectancy, gdp, population, continent) %>%
  filter(year==2010) %>%
  arrange(desc(gdp))

head(sorted_df)

sorted_df = sorted_df %>%
  arrange(desc(gdp))

head(sorted_df)

sorted_df = sorted_df %>%
  arrange(continent, desc(gdp))

head(sorted_df)

top3_df = mycountries %>%
  select(country, year, life_expectancy, gdp, population, continent) %>%
  filter(year==2010) %>%
  top_n(3, gdp) %>%
  arrange(gdp)

top3_df

bottom3_df = mycountries %>%
  select(country, year, life_expectancy, gdp, population, continent) %>%
  filter(year==2010) %>%
  top_n(3, desc(gdp)) %>%
  arrange(gdp)
  
bottom3_df

#Mutate

new_countries = mycountries %>%
  filter(year==2010) %>%
  mutate(gdp_capita = gdp / population) %>%
  arrange(desc(gdp_capita))

head(new_countries)

new_countries3 = new_countries %>%
  mutate(gdp_capita = if_else(gdp_capita<40000, 0, gdp_capita))

new_countries3

#Summarize

summarize_countries = mycountries %>%
  mutate(gdp_capita = gdp/population) %>%
  summarize(mean_gdp = mean(gdp, na.rm = T),
            mean_gdp_capita = mean(gdp_capita, na.rm = T))

summarize_countries

summarize_countries2 = mycountries %>%
  mutate(gdp_capita = gdp/population) %>%
  group_by(year) %>%
  summarize(mean_gdp = mean(gdp, na.rm = T),
            mean_gdp_capita = mean(gdp_capita, na.rm = T))

summarize_countries2

summarize_countries3 = mycountries %>%
  mutate(gdp_capita = gdp / population) %>%
  filter(year>2010) %>%
  group_by(continent) %>%
  summarize(num_ob = n(),
            mean_gdp_capita = mean(gdp_capita, na.rm = T))

summarize_countries3  
