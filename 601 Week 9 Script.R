wine = read.csv("wine.csv")
install.packages("broom")
require(ggplot2)
require(dplyr)
require(tidyr)
str(wine)

#Simple Linear Regression
plot1 = ggplot(wine, aes(x = AGST, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")

regression1 = lm(Price~AGST, data = wine)
summary(regression1)


library(broom)

tidy(regression1)%>%
  filter(term == "AGST")%>%
  select(estimate)

glance(regression1)  

wine = wine%>%
  mutate(log_price = log(Price))

plot2 = ggplot(wine, aes(x = AGST, y = log_price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")
plot2
require(ggpubr)
ggarrange(plot1, plot2)

regression2 = lm(log_price~AGST, data = wine)
summary(regression2)
tidy(regression2)

glance(regression1)
glance(regression2)

augment(regression1)

regression1 %>%
  augment() %>%
  mutate(residual = Price - .fitted)%>%
  summarize(r_squared = 1 - var(residual)/var(Price))

#Model Evaluation
glance(regression1)%>%
  select(adj.r.squared)

regression1 %>%
  augment() %>%
  mutate(residual = Price - .fitted) %>%
  mutate(sq_residual = (residual)^2)%>%
  summarize(mse = mean(sq_residual)) %>%
  summarize(rmse = sqrt(mse))

regression2 %>%
  augment() %>%
  mutate(residual = log_price - .fitted) %>%
  mutate(sq_residual = (residual)^2)%>%
  summarize(mse = mean(sq_residual)) %>%
  summarize(rmse = sqrt(mse))

#Multivariate

str(wine)
regression3 = lm(log_price ~ AGST + WinterRain + HarvestRain + Age + FrancePop, data = wine)
tidy(regression3)
summary(regression3)
glance(regression3)

regression4 = lm(log_price ~ AGST + WinterRain + HarvestRain + FrancePop, data = wine)
tidy(regression4)
glance(regression4)

regression5 = lm(log_price ~ WinterRain + HarvestRain + Age + FrancePop, data = wine)
glance(regression5)


#Hands on 
library(tidyverse)
countries = read.csv("countries.csv")
head(countries)
str(countries)
count(countries)


plot1 = ggplot(countries, aes(x = gdp, y = life_expectancy)) +
  geom_point(data = subset(countries, year == 2010)) +
  geom_smooth(method = "lm", se = F, color = "red")

countries = countries%>%
  mutate(log_gdp = log(gdp))

regression1 = lm(life_expectancy~gdp, data = countries)
summary(regression1)

regression2 = lm(life_expectancy~log_gdp, data = countries)
summary(regression2)

plot2 = ggplot(countries, aes(x = log_gdp, y = life_expectancy)) +
  geom_point(data = subset(countries, year == 2010)) +
  geom_smooth(method = "lm", se = F, color = "red")

require(ggpubr)

ggarrange(plot1, plot2)

regression3 = lm(life_expectancy~log_gdp + fertility_rate + infant_mortality, data = countries)
summary(regression3)

regression4 = lm(life_expectancy~log_gdp + fertility_rate + infant_mortality + as.factor(continent), data = countries)
summary(regression4)

countries = countries%>%
  mutate(continent = as.factor(continent))
str(countries)
