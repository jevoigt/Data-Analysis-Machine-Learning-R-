library(tidyverse)
library(broom)
setwd("/Users/JackVoigt/Documents/MSIA Files")
wine = read.csv("wine.csv")


str(wine)
wine = wine %>%
  mutate(log_price = log(Price))

#Full model (including all predictor variables) / Null model (no variables)

full_model = lm(log_price ~ AGST + WinterRain + HarvestRain + Age + FrancePop, data = wine)

full_model = lm(log_price ~ . - Year - Price - log_price, data = wine)

null_model = lm(log_price ~ 1, data = wine)

summary(null_model)

step_model_backward = step(full_model, scope = list(lower = null_model, upper = full_model), 
                           direction = "backward")

step_model_both = step(full_model, scope = list(lower = null_model, upper = full_model), 
                           direction = "both")

step_model_forward = step(null_model, scope = list(lower = null_model, upper = full_model), 
                       direction = "forward")

summary(step_model_backward)


#Video 2

require(tidyverse)
require(broom)

wine_new = read.csv("wine_new.csv")

head(wine)
str(wine)
str(wine_new)

regression1 = lm(log_price ~ AGST + WinterRain + HarvestRain +FrancePop, data = wine)

summary(regression1)

regression1 %>%
  augment()

wine_new = wine_new %>%
  mutate(log_price = log(Price))

regression1 %>%
  augment(newdata = wine_new)

install.packages("yardstick")
require(yardstick)

regression1 = lm(log_price ~ AGST + WinterRain + HarvestRain + FrancePop, data = wine)
regression2 = lm(log_price ~ AGST + WinterRain + HarvestRain, data = wine)

regression1 %>%
  augment() %>%
  rmse(log_price, .fitted)

regression2 %>%
  augment() %>%
  rmse(log_price, .fitted)

regression1 %>%
  augment(newdata = wine_new) %>%
  rmse(log_price, .fitted)

regression2 %>%
  augment(newdata = wine_new) %>%
  rmse(log_price, .fitted)


install.packages("caTools")
require(caTools)

set.seed(645)

sample = sample.split(wine, SplitRatio = 0.7)

train = subset(wine, sample == T)
test = subset(wine, sample == F)

str(train)
str(test)

regression3 = lm(log_price ~ AGST + WinterRain + HarvestRain + FrancePop, data = train)
regression4 = lm(log_price ~ AGST + WinterRain + HarvestRain, data = train)

regression3 %>%
  augment(newdata = test) %>%
  rmse(log_price, .fitted)

regression4 %>%
  augment(newdata = test) %>%
  rmse(log_price, .fitted)


#Video 3 
require(ggplot2)
require(tidyverse)
moneyball = read.csv("moneyball.csv")

str(moneyball)
head(moneyball)
head(moneyball$Playoffs,20)

moneyball = moneyball %>%
  mutate(WS = if_else(RankPlayoffs==1, T, F))

ggplot(moneyball, aes(x = W, y = Playoffs)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)


logistic_playoffs = glm(Playoffs ~ W, data = moneyball, family = "binomial")

logistic_worldseries = glm(WS ~ W, data = moneyball, family = "binomial")

summary(logistic_playoffs)
summary(logistic_worldseries)

install.packages("DescTools")
install.packages("margins")
require(DescTools)
require(margins)

logistic_playoffs %>%
  PseudoR2()

logistic_worldseries %>%
  PseudoR2()

logistic_playoffs %>%
  margins() %>%
  summary()


logistic_worldseries %>%
  margins() %>%
  summary()

#Hands On

houses = read.csv("house_prices.csv")

head(houses)

houses = houses %>%
  mutate(renovated = if_else(yr_renovated>0, 1, 0))


#Approach 1
require(caTools)

houses_split = sample.split(houses, SplitRatio = 0.7)

houses_train = subset(houses, houses_split == T)
houses_test = subset(houses, houses_split == F)


regression1 = lm(price ~ bedrooms + bathrooms + floors + condition + grade + sqft_living + sqft_lot + yr_built + renovated, data = houses_train)
regression2 = lm(price ~ bedrooms + bathrooms + floors + condition + grade + sqft_living + sqft_lot + yr_built + renovated + waterfront + view, data = houses_train)

summary(regression1)
summary(regression2)

#calculate RMSE
require(broom)
require(yardstick)

#Approach 1

regression1 %>%
  augment(newdata = houses_test) %>% 
  rmse(price, .fitted)
  

regression2 %>%
  augment(newdata = houses_test) %>% 
  rmse(price, .fitted)
  


#Approach 2 (using predict)

predicted_value1 = regression1 %>%
  predict(newdata = houses_test)

head(predicted_value1)

predicted_value2 = regression2 %>%
  predict(newdata = houses_test)

houses_test = houses_test %>%
  mutate(.fitted1 = predicted_value1, .fitted2 = predicted_value2)

str(houses_test)

houses_test %>%
  rmse(price, .fitted1)
houses_test %>%
  rmse(price, .fitted2)

#Exercise 2 logistic regression

loan = read.csv("loan.csv")
head(loan$loan_status,30)

ggplot(loan, aes(x = annual_inc, y = loan_status)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)


logistic1 = glm(loan_status ~ ., data = loan, family = "binomial")

summary(logistic1)

logistic1 %>%
  PseudoR2()

logistic1 %>%
  margins() %>%
  summary()





