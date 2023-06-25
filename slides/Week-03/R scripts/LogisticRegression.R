# Default data example
library(ISLR2)

# Calling the data set, Default is in ISLR2
data("Default")
help("Default")

# First insight of the data set
head(Default)

# Some descriptive statistics on Default
summary(Default)

# dim(Default)


# Data Partitioning as %75
set.seed(442)
default_idx = sample(nrow(Default), 0.75 * nrow(Default))
default_idx
head(default_idx)
length(default_idx)

# Training data set
default_trn = Default[default_idx, ]
dim(default_trn)
# table(default_trn$default)
# Testing data set
default_tst = Default[-default_idx, ]
dim(default_tst)


#library("glm2")
# Start with one single predictor, focusing on balance 
help("glm")
# Build the model on the training data set with family = binomial
# for balance as predictor
model_glm = glm(default ~ balance, data = default_trn, family = "binomial")
summary(model_glm)

# class(Default$default)

# for income 
model_glm2 = glm(default ~ income, data = default_trn, family = "binomial")
summary(model_glm2)


# Calling just coefficients of the model with balance as predictor
coef(model_glm)

# Prediction part for the fitted model 
# Result of logistic link function by default
head(predict(model_glm))

# Now we can get probabilities by changing the type as response
head(predict(model_glm, type = "response"))

#  these are not predicted probabilities. To obtain the predicted probabilities
head(predict(model_glm, type = "response", newdata = default_tst))

# Note that these are probabilities, not classifications. 
# To obtain classifications, we will need to compare to the correct cutoff value with an ifelse() statement.
predict(model_glm, type = "response") > 0.5

# model_glm_pred = ifelse(predict(model_glm, type = "link") > 0, "Yes", "No")
model_glm_pred = ifelse(predict(model_glm, type = "response", 
                                newdata = default_tst) >= 0.5, "Yes", "No")

head(model_glm_pred)
# Look at the frequency
table(model_glm_pred)
table(default_tst$default)
# To make a small comparison
 # Coming from the original values in training data
table(default_trn$default)

# Coming from the predicted values over the training data
table(model_glm_pred)

# Training error rate
# For the calculation of basically mean error rate
calc_class_err = function(actual, predicted) 
  {
  mean(actual != predicted) 
  }

# error rate on testing
calc_class_err(actual = default_tst$default, predicted = model_glm_pred)


# You can do a similar thing for testing, you must do indeed !
model_glm_pred = ifelse(predict(model_glm, type = "response", 
                                newdata = default_tst) > 0.5, "Yes", "No")

head(model_glm_pred)
length(model_glm_pred)
length(predict(model_glm, type = "response", 
               newdata = default_tst))

calc_class_err(actual = default_tst$default, predicted = model_glm_pred)

# Calculation of Confusion Matrix on training data
library(caret)

# Training data set
# train_tab = table(predicted = model_glm_pred, actual = default_trn$default)
# train_tab
# train_con_mat = confusionMatrix(train_tab, positive = "Yes")
# train_con_mat

# Testing data set 
model_glm_pred = ifelse(predict(model_glm, newdata = default_tst, 
                                type = "response") > 0.5, "Yes", "No")


test_tab = table(predicted = model_glm_pred, actual = default_tst$default)
test_tab
help("confusionMatrix")
test_con_mat = confusionMatrix(test_tab, positive = "Yes")
test_con_mat

# OR to see all relevant quantities

test_con_mat$byClass

# OR choose some of them TRY to RUN
#c(train_con_mat$overall["Accuracy"], 
#  train_con_mat$byClass["Sensitivity"], 
#  train_con_mat$byClass["Specificity"])


# Multiple Logistic Regression Case ---------------------------------------

# Fitting the model on training data set, with balance and student as predictors
model_glm = glm(default ~ balance + student, data = default_trn, family = "binomial")
summary(model_glm)

# prediction on testing data 
model_glm_pred_mult = ifelse(predict(model_glm, newdata = default_tst, 
                                type = "response") > 0.5, "Yes", "No")

head(model_glm_pred_mult)

# Confusion matrix on testing
testing_tab_mult = table(predicted = model_glm_pred_mult, actual = default_tst$default)
testing_con_mat_mult = confusionMatrix(testing_tab_mult, positive = "Yes")

print(testing_con_mat_mult)
testing_con_mat_mult$byClass

# For graphical interpretation one can use ROC curve 
# For multiple logistic regression example

library(pROC)
# Predictions on testing data 
test_prob_mult = predict(model_glm, newdata = default_tst, type = "response")
# Drawing ROC curve for the given model

test_roc_mult = roc(default_tst$default ~ test_prob_mult, plot = T, 
                    print.auc = T)

test_roc_mult
# The value of AUC (Area under the curve), high values are indicators of good model !
as.numeric(test_roc_mult$auc)


# Multinomial Example  ----------------------------------------------------
set.seed(442)

# Well known iris data for multinomial case 
# Look at the data detail first
help(iris)

head(iris)

# THE MOST IMPORTANT CHARACTERISTIC
# BALANCED DATA 
table(iris$Species)

iris_obs = nrow(iris)
iris_idx = sample(iris_obs, size = trunc(0.75 * iris_obs))
iris_trn = iris[iris_idx, ]
dim(iris_trn)
iris_test = iris[-iris_idx, ]

# NOW we have different package for multiclass problem
library(nnet)

# Fitting model on species by using all predictors
model_multi = multinom(Species ~ ., data = iris_trn, trace = FALSE)

# We have coefficient estimates and related std errors only in this summary
summary(model_multi)
summary(model_multi)$coefficients

# A difference between glm() and multinom() is how the predict() function operates.
model_multi_pred <- (predict(model_multi, newdata = iris_test, type = "probs"))

# First 6 predicted values
head(model_multi_pred)

# By looking at the largest probability we can see the category
# First row says that it is classified as Setosa as an example

# EXERCISE
# Think about the creation of Confusion matrix on this multi-class example ??
