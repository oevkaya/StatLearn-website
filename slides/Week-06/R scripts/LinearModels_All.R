# GOAL: focus on regression models that predict a continuous outcome
# how to implement linear and regularized regression models using R ?

# The topics we'll cover include:

# 1. Linear Regression

# 2. Subset selection

# 3. Polynomial Regression

# 4. Ridge Regression

# 5. Lasso Regression

# About Data Set
data()
library(ISLR2)
data("Boston")

summary(Boston)
names(Boston)

unique(Boston$chas)


# Data Partitioning
set.seed(442) # for reproducible results
sample.size <- floor(0.80 * nrow(Boston)) # %80 for training, %20 for testing
train.index <- sample(seq_len(nrow(Boston)), size = sample.size)
# Creating training and testing data sets
train <- Boston[train.index, ]
test <- Boston[-train.index, ]

summary(train$medv)
summary(test$medv)
# Linear Regression -------------------------------------------------------

# Fit a regression line to describe medv by all possible predictors
# Multiple linear regression 
summary(lm(medv ~ lstat, data = train))


lm.fit = lm(medv ~ ., data = train) 
summary(lm.fit) 

par(mfrow = c(2,2))
plot(lm.fit)

# Confidence interval for the fitted model parameters
confint(lm.fit)

# About performance metrics
library(modelr)
data.frame(
  R2 = rsquare(lm.fit, data = test),
  MSE = mse(lm.fit, data = test), 
  RMSE = sqrt(mse(lm.fit, data = test)),
  MAE = mae(lm.fit, data = test)
)


# Subset Selection --------------------------------------------------------

# Consider leaps package for subset selection 
# install.packages("leaps)
library(leaps)

regfit.full <- regsubsets(medv ~ ., data = train, nvmax = 12)
#regfit.full
reg.summary <- summary(regfit.full)
reg.summary

# Try to figure out optimal numbers in best subset selection 
which.min(reg.summary$rss)
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

par(mfrow = c(2, 2))
plot(reg.summary$rss , xlab = "Number of Variables", ylab = "RSS", type = "l")
points(12, reg.summary$rss[12] , col = "red", cex = 2, pch = 20)

plot(reg.summary$adjr2 , xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
points(11, reg.summary$adjr2[11] , col = "red", cex = 2, pch = 20)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
points(10, reg.summary$cp[10] , col = "red", cex = 2, pch = 20)

plot(reg.summary$bic , xlab = "Number of Variables", ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

# Stick to the smallest one for interpretation by BIC value 
plot(regfit.full, scale = "bic")

reg.summary$bic
coef(regfit.full, 6)

# Forward selections
regfit.forw <- regsubsets(medv ~ ., data = train, nvmax = 12, method = "forward")
#regfit.full
reg.summary_forw <- summary(regfit.forw)
reg.summary_forw

which.min(reg.summary_forw$bic)
plot(reg.summary_forw$bic , xlab = "Number of Variables", ylab = "BIC", type = "l")
points(6, reg.summary_forw$bic[6], col = "red", cex = 2, pch = 20)
plot(regfit.forw , scale = "bic")

# Backward selections
regfit.back <- regsubsets(medv ~ ., data = train, nvmax = 12, method = "backward")
#regfit.full
reg.summary_back <- summary(regfit.back )
reg.summary_back

which.min(reg.summary_back$bic)
plot(reg.summary_back$bic , xlab = "Number of Variables", ylab = "BIC", type = "l")
points(6, reg.summary_back$bic[6], col = "red", cex = 2, pch = 20)
plot(regfit.back, scale = "bic")

# Compare those subset selections to see which variables more useful
# Basic comparison with 6 predictors

reg.summary$adjr2; reg.summary_forw$adjr2; reg.summary_back$adjr2
reg.summary$bic; reg.summary_forw$bic; reg.summary_back$bic

coef(regfit.full, 6)

coef(regfit.forw, 6)

coef(regfit.back, 6)

# All agreed on 6 variables;
# chas, nox, rm, dis, ptratio and lstat

# We can drop useless ones and apply multiple linear regression again
# multiple linear regression 
lm.fit2 = lm(medv ~ chas + nox + rm + dis + ptratio + lstat, data = train) 
summary(lm.fit2) 

par(mfrow = c(2,2))
plot(lm.fit2)

# About performance metrics
library(modelr)
data.frame(
  R2 = rsquare(lm.fit2, data = test),
  MSE = mse(lm.fit2, data = test), 
  RMSE = sqrt(MSE),
  MAE = mae(lm.fit2, data = test)
)

# Add some non-linearity
plot(Boston$medv, Boston$lstat)
lm.fit3 = lm(medv ~ chas + nox + rm + dis + ptratio + lstat + I(lstat^2), data = train) 
summary(lm.fit3) 

data.frame(
  R2 = rsquare(lm.fit3, data = test),
  MSE = mse(lm.fit3, data = test), 
  RMSE = rmse(lm.fit3, data = test),
  MAE = mae(lm.fit3, data = test)
)


# Try other alternatives like, between 6-10 in general !

# Ridge regression --------------------------------------------------------
x <- model.matrix(medv ~ ., Boston)[, -1]
head(x)

# response variable
y <- Boston$medv
hist(y)
hist(log(y))

# Minimizing RSS + penalty term ! including lambda

library(glmnet)

grid <- 10^seq(10, -2, length = 100)
head(grid)
tail(grid)
grid


ridge.mod <- glmnet(x[train.index, ], y[train.index], alpha = 0, lambda = grid)
ridge.mod

plot(ridge.mod)
summary(ridge.mod)
# Lambda as an example here, lambda = 4
ridge.pred <- predict(ridge.mod, s = 4, newx = x[-train.index, ])
mean(( ridge.pred - y[-train.index] ) ^ 2 )


# CV on Ridge regression by default nfolds = 10, 10-fold CV 
cv.out <- cv.glmnet(x[train.index, ], y[train.index], alpha = 0)
cv.out$lambda

plot(cv.out$lambda)

plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

# Apply again with the best lambda value 
ridge.mod <- glmnet(x[train.index, ], y[train.index], alpha = 0, lambda = bestlam)
ridge.mod$beta


ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[-train.index, ])
mean((ridge.pred - y[-train.index])^2)
mean(abs((ridge.pred - y[-train.index])))

sqrt(mean((ridge.pred - y[-train.index])^2))

# which(ridge.mod$lambda == bestlam)

ridge.mod$beta[, ridge.mod$lambda == bestlam]

# extract the coefficient estimates wrt the best lambda ? 
coef(ridge.mod, s = bestlam)

# Now we can refit the model on full data set 
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)

# For more measures create function like 
# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  MSE <- SSE/nrow(df)
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    MSE = MSE,
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on train data
predictions_train <- predict(ridge.mod, s = bestlam, newx = x[train.index, ])
eval_results(y[train.index], predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge.mod, s = bestlam, newx = x[-train.index,])
eval_results(y[-train.index], predictions_test, test)

# LASSO regression --------------------------------------------------------

# grid <- 10^seq(10, -2, length = 100)
# grid
lasso.mod <- glmnet(x[train.index, ], y[train.index], alpha = 1, lambda = grid)

summary(lasso.mod)

coef(lasso.mod, s = 0.01) # Play with s value for lambda
plot(lasso.mod)


# CV on Lasso lambda = grid
# Put a comment on it ? 
cv.lasso <- cv.glmnet(x[train.index, ], y[train.index], alpha = 1)# nfolds = 10 default

plot(cv.lasso)
cv.lasso$lambda
bestlam_lasso <- cv.lasso$lambda.min
bestlam_lasso


# Apply again with the best lambda value 
lasso.pred <- predict(lasso.mod, s = bestlam_lasso, newx = x[-train.index, ])
mean((lasso.pred - y[-train.index])^2)
sqrt(mean((lasso.pred - y[-train.index])^2))

# Prediction and evaluation on train data
predictions_train_lasso <- predict(lasso.mod, s = bestlam_lasso, newx = x[train.index, ])
eval_results(y[train.index], predictions_train_lasso, train)

# Prediction and evaluation on test data
predictions_test_lasso <- predict(lasso.mod, s = bestlam_lasso, newx = x[-train.index,])
eval_results(y[-train.index], predictions_test_lasso, test)

# Coefficients of lasso
lasso.coef <- predict(lasso.mod, type = "coefficients", s = bestlam_lasso)
lasso.coef

# Examples ----------------------------------------------------------------

# https://www.pluralsight.com/guides/linear-lasso-and-ridge-regression-with-r



# CV on MLR ---------------------------------------------------------------

# k-fold CV ---------------------------------------------------------------

# Define training control
set.seed(123) 
library(caret)
train.control <- trainControl(method = "cv", number = 10)

# Train the model with all predictors
model_cv10 <- train(medv ~chas + nox + rm + dis + ptratio + lstat + I(lstat^2), 
                   data = train, method = "lm", trControl = train.control)
summary(model_cv10)

# Summarize the results
print(model_cv10)


# Make predictions and compute the R2, RMSE and MAE
predictions_poly <- predict(model_cv10, test)
data.frame( R2 = R2(predictions_poly, test$medv),
            RMSE = RMSE(predictions_poly, test$medv),
            MSE = RMSE^2,
            MAE = MAE(predictions_poly, test$medv))
