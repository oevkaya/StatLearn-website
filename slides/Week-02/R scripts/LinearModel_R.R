
# Calculation by Hand with mtcars -----------------------------------------


# Starting point toy example for manual calculations 
data("mtcars")

# Details of the data set
help("mtcars")
str(mtcars)

mtcars

# hist(mtcars$mpg)
plot(mtcars$wt, mtcars$mpg)

# sample mean of x
xbar <- mean( mtcars$wt )

# sample mean of y
ybar <- mean( mtcars$mpg)

# numerator for beta1 estimate
Sxy <- sum( (mtcars$wt - xbar) * (mtcars$mpg - ybar) )

# denumerator for beta1 estimate
Sxx <- sum( (mtcars$wt - xbar)^2 )

# estimate of beta1, of slope
beta1_hat <- Sxy / Sxx
beta1_hat
# estimate of beta0, of intercept
beta0_hat <- ybar - beta1_hat * xbar
beta0_hat

# Our linear fit has the form
# mpg \approx beta0_hat + beta1_hat * wt

# For wt = 4

mpg = beta0_hat + beta1_hat * 4
mpg


# Use of lm function ------------------------------------------------------

# Use of lm function 
help(lm)

mpg_model <- lm(formula = mpg ~ wt, data = mtcars)  

mpg_model
# For more information
summary(mpg_model)

abline(mpg_model, col = "red")

# Use of lm on a created data  -----------------------------

pool <- data.frame(
  time = c(2, 4, 6, 8, 10, 12),
  chlorine = c(1.8, 1.5, 1.4, 1.1, 1.1, 0.9)
)
plot(pool$time, pool$chlorine, 
     xlab = "Time (hours)", 
     ylab = "Chlorine (p.p.m.)",
     xlim = c(0, 14), ylim = c(0.5, 2.2))
grid()
pool

# Fit the Linear Regression Model:  E[ chlorine[i] ] = alpha + beta * time[i]
pool_model <- lm(formula = chlorine ~ time, data = pool)  
pool_model
abline(pool_model, col = "red")

summary(pool_model)


# Boston House Data Example -----------------------------------------------


# Data set from the package ISLR2 
# install.packages("MASS")
# install.packages("ISLR2")
library(MASS)
library(ISLR2)

# First insight about data set
data(Boston)
str(Boston)
head(Boston)
summary(Boston)

# To understand the relationship between two variables
plot(Boston[, c("lstat", "medv")])

# The correlation coefficient
cor(Boston[, c("lstat", "medv")])


# Fit a regression line to describe medv by lstat
hist(Boston$medv)
# attach(Boston)
lm.fit <- lm(formula = medv ~ lstat, data = Boston)
summary(lm.fit)
plot(lm.fit)
# OR y = beta_0 + beta_1 X
lm.fit <- lm(medv ~ lstat , data = Boston)
lm.fit


# Data Partitioning -------------------------------------------------------


# ALWAYS START with DATA PARTITIONING
# set.seed(123) # for reproducible results
sample.size <- floor(0.75 * nrow(Boston)) # %75 for training, %25 for testing
train.index <- sample(seq_len(nrow(Boston)), size = sample.size)

# Partitioning on training and testing
train <- Boston[train.index, ]
test <- Boston[-train.index, ]

dim(train)
dim(test)
# MODEL BUILDING
# Simple linear regression 
lm.fit = lm(formula = medv ~ lstat, data = train) 
summary(lm.fit) 


# PREDICTION : Use the testing data
class(lm.fit)

# Make prediction on training data
predict(lm.fit)
# Make predictions on testing only, newdata set 
predict_boston <- predict(lm.fit, newdata = test)
head(predict_boston)
head(test$medv)

# Looking at RMSE basically

MSE_fit <- mean((test$medv - predict_boston)^2)
MSE_fit

RMSE_fit <- sqrt(MSE_fit)
RMSE_fit

cor(predic_boston, test$medv)^2


# Testing the Performance -------------------------------------------------

library(tidyverse)
library(modelr)


test %>%
  add_predictions(lm.fit) %>%
  summarise(
    R2 = cor(medv, pred)^2,
    MSE = mean((medv - pred)^2), # average of the squared of difference between y and yhat
    RMSE = sqrt(MSE), # squrate of the MSE 
    MAE = mean(abs(medv - pred)) # average of the absolute difference between y and yhat
  )

# OR alternatively to summarize
data.frame(
  R2 = rsquare(lm.fit, data = test),
  MSE = mse(lm.fit, data = test), 
  RMSE = rmse(lm.fit, data = test),
  MAE = mae(lm.fit, data = test)
)

# Model diagnostic part 
par(mfrow = c(2,2))
plot(lm.fit)


# Transformed Response ----------------------------------------------------

# Apply some transformation on response 
summary(Boston$medv)
# mean(Boston$medv)
mode(Boston$medv)

hist(Boston$medv)
hist(log(Boston$medv))

# Let me try log(Y)
# Simple linear regression 
lm.fit2 = lm(formula = log(medv) ~ lstat, data = train) 
summary(lm.fit2) 

par(mfrow = c(2,2))
plot(lm.fit2)

data.frame(
  R2 = rsquare(lm.fit2, data = test),
  MSE = mse(lm.fit2, data = test), 
  RMSE = rmse(lm.fit2, data = test),
  MAE = mae(lm.fit2, data = test)
)


# Adding Interaction Covariates -------------------------------------------

# Adding interaction
# summary(lm(medv ~ age, data = Boston))
summary(lm(medv ~ lstat * age, data = Boston))

# Adding a transformed value of a variable 
# attach(Boston)
lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = train)
summary(lm.fit2)

data.frame(
  R2 = rsquare(lm.fit2, data = test),
  MSE = mse(lm.fit2, data = test), 
  RMSE = rmse(lm.fit2, data = test),
  MAE = mae(lm.fit2, data = test)
)

# attach(Boston)
# lm.fit <- lm(medv ~ lstat)

names(lm.fit)
# General summary for the model fit 
summary(lm.fit3)

# Model diagnostic plots in general
par(mfrow = c(2, 2))
plot(lm.fit)


# Confidence Interval -----------------------------------------------------

# Confidence interval for the fitted model parameters

confint(lm.fit)


# Prediction for new x values 
predict(lm.fit, data.frame(lstat = (c(5, 10, 15))),
  interval = "confidence")

# Plotting manually 
# lm.fit$fitted.values
# lm.fit$residuals
plot(predict(lm.fit), residuals(lm.fit))


# Multiple Regression part (Simple examples)
colnames(Boston)

# MLR with lstat and age
lm.fit_mult <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit_mult)

# All predictors are included with . notation
lm.fit_all <- lm(medv ~ ., data = Boston)
summary(lm.fit_all)


# Qualitative Predictors --------------------------------------------------

head(Carseats)
lm.fit <- lm(Sales ~ . , data = Carseats)
summary(lm.fit)

lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)
