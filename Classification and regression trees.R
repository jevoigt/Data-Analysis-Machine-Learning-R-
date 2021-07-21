require(tidyverse)
install.packages("rpart")
install.packages("rpart.plot")
require(rpart)
require(rpart.plot)

grade = read.csv("grade.csv")
str(grade)


grade = grade %>%
  mutate(pass = if_else(final_grade >= 7, 1, 0))

install.packages("rsample")
require(rsample)

grade_split = initial_split(grade, prop = 0.7)

grade_train = training(grade_split)
grade_test = testing(grade_split)

grade_dtree = rpart(pass ~ . -final_grade, data = grade_train, method = "class")

rpart.plot(grade_dtree, cex = 0.8)


#Video 2

install.packages("ROSE")
require(ROSE)

grade_train_over = ovun.sample(pass~ ., data = grade_train, method = "over", p = 0.5)$data
grade_train_under = ovun.sample(pass~ ., data = grade_train, method = "under", p = 0.5)$data

table(grade_train$pass)
table(grade_train_over$pass)
table(grade_train_under$pass)


#Complexity Parameter
install.packages("caret")
require(caret)

cp_test = train(pass ~ . -final_grade, data = grade_train_over, method = "rpart", trcontrol = trainControl("cv", number = 5), tuneLength = 30)

plot(cp_test)

grade_dtree_optimal = rpart(pass ~ . -final_grade, data = grade_train_over, method = "class", cp = 0.005)

rpart.plot(grade_dtree)

grade_dtree_pruned = prune(grade_dtree_optimal, cp = 0.005)

rpart.plot(grade_dtree_pruned)


#Video 3

grade_dtree_prob = grade_dtree_optimal %>%
  predict(newdata = grade_test, type = "prob")

head(grade_dtree_prob)

grade_dtree_class = grade_dtree_optimal %>%
  predict(newdata = grade_test, type = "class")

head(grade_dtree_class)

grade_test = grade_test %>%
  mutate(.fitted = grade_dtree_prob[, 2], predictive_class = grade_dtree_class)

str(grade_test)

library(caret)

confusionMatrix(as.factor(grade_test$predicted_class), as.factor(grade_test$pass), positive = "1")


install.packages("cutpointr")
require(cutpointr)

roc = roc(grade_test, x = .fitted, class = pass, pos_class = 1, neg_class = 0, direction = '>=')


dev.off()
plot(roc)
auc(roc)

plot(roc) +
  geom_abline(slope = 1, color = "red")


#Hands On

moneyball = read.csv("Moneyball.csv")
str(moneyball)
head(moneyball)

moneyball = moneyball %>%
  mutate(RD = RS - RA)

moneyball_logistic = glm(Playoffs ~ RD + BA + OBP + SLG, data = moneyball, family = "binomial")

predicted_logistic = moneyball_logistic %>%
  predict(type = "response")

head(predicted_logistic)

moneyball = moneyball%>%
  mutate(.fitted = predicted_logistic)


moneyball = moneyball%>%
  mutate(predicted_class1 = if_else(.fitted > 0.2, 1, 0))

head(moneyball)

moneyball = moneyball%>%
  mutate(predicted_class2 = if_else(.fitted > 0.35, 1, 0))

moneyball = moneyball%>%
  mutate(predicted_class3 = if_else(.fitted > 0.5, 1, 0))

moneyball = moneyball%>%
  mutate(predicted_class4 = if_else(.fitted > 0.65, 1, 0))

moneyball = moneyball%>%
  mutate(predicted_class5 = if_else(.fitted > 0.8, 1, 0))

str(moneyball)

library(caret)

confusionMatrix(as.factor(moneyball$predicted_class1), as.factor(moneyball$Playoffs), positive = "1")

confusionMatrix(as.factor(moneyball$predicted_class2), as.factor(moneyball$Playoffs), positive = "1")

confusionMatrix(as.factor(moneyball$predicted_class3), as.factor(moneyball$Playoffs), positive = "1")

confusionMatrix(as.factor(moneyball$predicted_class4), as.factor(moneyball$Playoffs), positive = "1")

confusionMatrix(as.factor(moneyball$predicted_class5), as.factor(moneyball$Playoffs), positive = "1")


#Hands On 2

loan = read.csv("loan.csv")
str(loan)

require(rsample)
set.seed(645)

loan_split = initial_split(loan, prop = 0.7)

loan_train = training(loan_split)
loan_test = testing(loan_split)

require(ROSE)

loan_train_over = ovun.sample(loan_status ~ . , data = loan_train, method = "over", p = 0.5)$data
table(loan_train$loan_status)
table(loan_train_over$loan_status)

loan_logistic = glm(loan_status ~ ., data = loan_train, family = "binomial")

loan_logistic_over = glm(loan_status ~ ., data = loan_train_over, family = "binomial")

library(DescTools)

PseudoR2(loan_logistic)
PseudoR2(loan_logistic_over)

library(rpart)
library(rpart.plot)

loan_ctree = rpart(loan_status ~ ., data = loan_train, method = "class")

loan_ctree_over = rpart(loan_status ~ ., data = loan_train_over, method = "class")

rpart.plot(loan_ctree)
rpart.plot(loan_ctree_over)


#Compute predicted probability 


predicted_logistic = loan_logistic %>%
  predict(newdata = loan_test, type = "response")

predicted_logistic_over = loan_logistic_over %>%
  predict(newdata = loan_test, type = "response")

predicted_ctree = loan_ctree %>%
  predict(newdata = loan_test, type = "prob")

head(predicted_ctree)

predicted_ctree_over = loan_ctree_over %>%
  predict(newdata = loan_test, type = "prob")

head(predicted_ctree_over)

loan_test = loan_test %>%
  mutate(.fitted_logistic = predicted_logistic) %>%
  mutate(.fitted_logistic_over = predicted_logistic_over) %>%
  mutate(.fitted_ctree = predicted_ctree[, 2]) %>%
  mutate(.fitted_ctree_over = predicted_ctree_over[, 2])


library(cutpointr)

roc_logistic = roc(loan_test,x =.fitted_logistic, class = loan_status,
                   pos_class = 1, neg_class = 0, direction = ">=")

roc_logistic_over = roc(loan_test,x =.fitted_logistic_over, class = loan_status,
                        pos_class = 1, neg_class = 0, direction = ">=")

roc_ctree = roc(loan_test,x =.fitted_ctree, class = loan_status,
                pos_class = 1, neg_class = 0, direction = ">=")

roc_ctree_over = roc(loan_test,x =.fitted_ctree_over, class = loan_status,
                     pos_class = 1, neg_class = 0, direction = ">=")

plot(roc_logistic) +
  labs(title = "ROC for week 12 hands on 2") +
  geom_line(data = roc_logistic, color = "red") +
  geom_line(data = roc_logistic_over, color = "blue") +
  geom_line(data = roc_ctree, color = "green") +
  geom_line(data = roc_ctree_over, color = "black")
  


auc(roc_logistic)
auc(roc_logistic_over)
auc(roc_ctree)
auc(roc_ctree_over)







