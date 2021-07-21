library(tidyverse)
library(cutpointr)

grade = read.csv("grade.csv", stringsAsFactors = T)

str(grade)
grade = grade %>%
  mutate(pass = if_else(final_grade >= 7, 1, 0))

library(rsample)
set.seed(645)

grade_split = initial_split(grade, prop = 0.7)

grade_train = training(grade_split) 
grade_test = testing(grade_split)

logistic = glm(as.factor(pass) ~ . -final_grade, data = grade_train, family = "binomial")


#Predicted probability 

predicted_value = logistic %>%
  predict(new_data = grade_test, type = "response")

head(predicted_value)

grade_test = grade_test %>%
  mutate(.fitted = predicted_value)

grade_test = grade_test %>%
  mutate(predicted_class = if_else(.fitted > 0.5, 1, 0))


table(grade_test$predicted_class, grade_test$pass)

library(caret)

confusionMatrix(as.factor(grade_test$predicted_class), as.factor(grade_test$pass), positive = '1')


# ROC and AUC

library(cutpointr)

roc = roc(grade_test, x = .fitted, class = pass, pos_class = 1, neg_class = 0, direction = ">=")

plot(roc)
auc(roc)

#Optimal Cutoff

cutpointr(grade_test, x = .fitted, class = pass, method = maximize_metric, metric = accuracy,
          pos_class = 1, neg_class = 0, direction = ">=")

grade_test = grade_test %>%
  mutate(predicted_class_optimal = if_else(.fitted > 0.216884, 1, 0))

str(grade_test)


cutpointr(grade_test, x = .fitted, class = pass, method = maximize_metric, metric = sum_sens_spec,
          pos_class = 1, neg_class = 0, direction = ">=")

grade_test = grade_test %>%
  mutate(predicted_class_optimal2 = if_else(.fitted > 0.0200935, 1, 0))



#Video 2, Ensemble Method

library(rpart)
library(rpart.plot)

grade_ctree = rpart(pass ~ . - final_grade, data = grade_train, method = "class")

rpart.plot(grade_ctree, cex = 0.7)


#Bagging + Aggregation
install.packages("randomForest")
library(randomForest)

grade_rf_class = randomForest(as.factor(pass) ~ . - final_grade, data = grade_train, ntree = 1000, importance = T)

varImpPlot(grade_rf_class)
plot(grade_rf_class)

grade_rf_reg = randomForest(pass ~ . -final_grade, data = grade_train, ntree = 1000, importance = T)

varImpPlot(grade_rf_reg)
plot(grade_rf_reg)


#Gradient Boosting Model

install.packages("gbm")
library(gbm)

grade_gbm_class = gbm(pass ~ . -final_grade, data = grade_train, n.trees = 1000, distribution = "bernoulli")

summary(grade_gbm_class)

grade_gbm_reg = gbm(final_grade ~ . -pass, data = grade_train, n.trees = 1000, distribution = "gaussian")

summary(grade_gbm_reg)

