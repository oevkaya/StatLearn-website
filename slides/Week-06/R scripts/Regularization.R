
# Prerequisites -----------------------------------------------------------
# Helper packages
library(recipes)  # for feature engineering

# Modeling packages
library(glmnet)   # for implementing regularized regression
library(caret)    # for automating the tuning process

# Model interpretability packages
install.packages("vip")
library(vip)      # for variable importance


# About Data Set ----------------------------------------------------------
# Ames housing data
install.packages("AmesHousing")
library(AmesHousing)

help()

# Loading the data set 
ames <- AmesHousing::make_ames()
head(ames)
colnames(ames)
dim(ames)
# Stratified sampling with the rsample package
set.seed(123)

# to use initial_split we need rsample
library(rsample)

summary(ames$Sale_Price)
hist(ames$Sale_Price)

help(initial_split)
# 70-30 percent rule is applied including stratified sampling

split <- initial_split(ames, prop = 0.7, 
                       strata = "Sale_Price")

# Training data set 
ames_train  <- training(split)
dim(ames_train)
# Testing data set
ames_test   <- testing(split)
dim(ames_test)

# Check the distribution of Y values
hist(ames_train$Sale_Price)
hist(ames_test$Sale_Price)
# Without stratified sampling !
# split <- initial_split(ames, prop = 0.7)
# ames_train  <- training(split)
# ames_test   <- testing(split)
# hist(ames_train$Sale_Price)
# hist(ames_test$Sale_Price)

# Implementation ----------------------------------------------------------

# we illustrate an implementation of regularized regression using 
# the direct engine glmnet

# The glmnet package is extremely efficient and fast, even on very large data sets 
# (mostly due to its use of Fortran to solve the lasso problem via 
# coordinate descent)

head(ames_train)

# Create training  feature matrices
# we use model.matrix(...)[, -1] to discard the intercept
X <- model.matrix(Sale_Price ~ ., ames_train)[, -1]
head(X)
dim(X)



# transform y with log transformation
# parametric models such as regularized regression are sensitive to 
# skewed response values so transforming can often improve predictive performance
hist(ames_train$Sale_Price)
Y <- log(ames_train$Sale_Price)
hist(Y)
length(Y)

# To apply a regularized model we can use the glmnet::glmnet() function. 
# The alpha parameter tells glmnet to perform a ridge (alpha = 0), 
# lasso (alpha = 1), or elastic net (0 < alpha < 1) model

# glmnet will do two things that you should be aware of:
# 1. By default, glmnet automatically standardizes your features. 
# If you standardize your predictors prior to glmnet you can turn this argument 
# off with standardize = FALSE.

# 2. glmnet will fit ridge models across a wide range of lambda values

# Apply ridge regression to ames data
ridge <- glmnet(
  x = X,
  y = Y,
  alpha = 0
)

plot(ridge, xvar = "lambda")

# glmnet can auto-generate the appropriate  λ values based on the data; 
# the vast majority of the time you will have little need to adjust this default.

# lambdas applied to penalty parameter
ridge$lambda %>% head()

# small lambda results in large coefficients
coef(ridge)[c("Latitude", "Overall_QualVery_Excellent"), 100]

# large lambda results in small coefficients
coef(ridge)[c("Latitude", "Overall_QualVery_Excellent"), 1]  

# we do not understand how much improvement we are experiencing in our 
# loss function across various λ values.


# CV on Ridge and Lasso ---------------------------------------------------

# Recall that lambda is a tuning parameter that helps to control our model from 
# over-fitting to the training data. To identify the optimal lambda
# value we can use k-fold cross-validation (CV). glmnet::cv.glmnet() can 
# perform k-fold CV, and by default, performs 10-fold CV

# By default, glmnet::cv.glmnet() uses MSE as the loss function but you can 
# also use mean absolute error (MAE) for continuous outcomes by changing the 
# type.measure argument
cv.glm
# Apply CV ridge regression to Ames data
ridge <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 0
)

# Apply CV lasso regression to Ames data
lasso <- cv.glmnet(
  x = X,
  y = Y,
  alpha = 1
)

# plot results
par(mfrow = c(1, 2))
plot(ridge, main = "Ridge penalty\n\n")
plot(lasso, main = "Lasso penalty\n\n")

# Ridge model
min(ridge$cvm)       # minimum MSE
ridge$lambda.min     # lambda for this min MSE

ridge$cvm[ridge$lambda == ridge$lambda.1se]  # 1-SE rule
ridge$lambda.1se  # lambda for this MSE

# Lasso model
min(lasso$cvm)       # minimum MSE
lasso$lambda.min     # lambda for this min MSE
lasso$nzero[lasso$lambda == lasso$lambda.min] # No. of coef | Min MSE

lasso$cvm[lasso$lambda == lasso$lambda.1se]  # 1-SE rule
lasso$lambda.1se  # lambda for this MSE

lasso$nzero[lasso$lambda == lasso$lambda.1se] # No. of coef | 1-SE MSE


# Plotting ----------------------------------------------------------------

# Ridge model
ridge_min <- glmnet(
  x = X,
  y = Y,
  alpha = 0
)

# Lasso model
lasso_min <- glmnet(
  x = X,
  y = Y,
  alpha = 1
)
lasso

par(mfrow = c(1, 2))
# plot ridge model
plot(ridge_min, xvar = "lambda", main = "Ridge penalty\n\n")
abline(v = log(ridge$lambda.min), col = "red", lty = "dashed")
abline(v = log(ridge$lambda.1se), col = "blue", lty = "dashed")

# plot lasso model
plot(lasso_min, xvar = "lambda", main = "Lasso penalty\n\n")
abline(v = log(lasso$lambda.min), col = "red", lty = "dashed")
abline(v = log(lasso$lambda.1se), col = "blue", lty = "dashed")

# Ridge model
ridge_min <- glmnet(
  x = X,
  y = Y,
  alpha = 0, lambda = ridge$lambda.min
)
coef(ridge_min)

# Tuning parameters -------------------------------------------------------

# The following performs a grid search over 10 values of the alpha parameter 
# between 0 and 1 and ten values of the lambda parameter from the lowest to highest 
# lambda values identified by glmnet.
# for reproducibility
set.seed(123)

# grid search across 
cv_glmnet <- train(
  x = X,
  y = Y,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)


# model with lowest RMSE
cv_glmnet$bestTune


# results for model with lowest RMSE
cv_glmnet$results %>%
  filter(alpha == cv_glmnet$bestTune$alpha, lambda == cv_glmnet$bestTune$lambda)

# plot cross-validated RMSE
library(ggplot2)
ggplot(cv_glmnet)

# predict sales price on training data
pred <- predict(cv_glmnet, X)
head(pred)

# compute RMSE of transformed predicted
RMSE(exp(pred), exp(Y))



# Feature interpretation --------------------------------------------------

# Variable importance for regularized models provides a similar interpretation as 
# in linear (or logistic) regression. Importance is determined by magnitude of 
# the standardized coefficients

vip(cv_glmnet, num_features = 20, geom = "point")

# For more details about above example check the followings

# https://bradleyboehmke.github.io/HOML/regularized-regression.html

# https://bradleyboehmke.github.io/HOML/process.html#put-process-together

# https://bradleyboehmke.github.io/HOML/process.html#stratified

# https://www.rdocumentation.org/packages/caret/versions/4.47/topics/train

# https://search.r-project.org/CRAN/refmans/caret/html/preProcess.html

# https://topepo.github.io/caret/pre-processing.html
