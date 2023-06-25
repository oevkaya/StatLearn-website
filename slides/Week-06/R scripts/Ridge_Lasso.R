
# Data Set ----------------------------------------------------------------

library(ISLR2)
data(Hitters)
head(Hitters)
dim(Hitters)
summary(Hitters)

# Creation of predictor set as a model.matrix, qualitative variables
# are transformed into dummy
x <- model.matrix(Salary ~ ., Hitters)[,-1]
x[,1] # The first column is not useful so we dropped
head(x)
dim(x)

# Keeping response
y <- Hitters$Salary 
summary(y)

# Droping NAs from the response
y <- y[is.na(y) == FALSE]
length(y)
#y <- which(y == NA)


# glmnet package usage ----------------------------------------------------

library(glmnet)

# Create grid of lambda values
grid <- 10^seq(10, -2, length = 100)
length(grid)
head(grid)
tail(grid)

# Fit a model over the whole data set
# Alpha = 0 is for Ridge
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
dim(ridge.mod$beta)

# Plotting the ridge model object
plot(ridge.mod)

# Dimension of coefficients 
dim(coef(ridge.mod))

# Looking lambda values separately
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]

# Getting coefficients with predict() function
predict(ridge.mod, s = ridge.mod$lambda[50], type = "coefficients")
predict(ridge.mod, s = ridge.mod$lambda[100], type = "coefficients")

# Data Partitioning -------------------------------------------------------

set.seed(442) 

# For 50-50 data splitting 
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)

# Defining training data
x.train <- x[train , ]
y.train <- y[train]
length(y.train)

# Testing data
x.test <- x[test , ]
y.test <- y[test]
length(y.test)

# Fit Ridge model 
ridge.mod <- glmnet(x[train , ], y[train], alpha = 0, lambda = grid)
# summary(ridge.mod)

# Prediction in terms of test data 
ridge.pred <- predict(ridge.mod , s = 6, newx = x[test , ])
mean(( ridge.pred - y.test)^2)


# Apply CV to choose optimal lambda ---------------------------------------

# For Ridge Regression
# By default, the function performs ten-fold cross-validation, 
# though this can be changed using the argument nfolds.
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 0)
plot(cv.out)
cv.out$lambda
bestlam <- cv.out$lambda.min
bestlam

# Fitting the model over the best lambda
ridge.mod <- glmnet(x[train , ], y[train], alpha = 0, lambda = bestlam)

# Making predictions over the test data
ridge.pred <- predict(ridge.mod , s = bestlam, newx = x[test , ])
head(ridge.pred)
# MSE over the test data set based on optimal lambda
mean(( ridge.pred - y.test)^2)

# For LASSO 
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1)
# plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

lasso_mod <- glmnet(x[train , ], y[train], alpha = 1, lambda = bestlam)

# coef(lasso_mod)

lasso.pred <- predict(lasso_mod , s = bestlam, newx = x[test , ])
mean(( lasso.pred - y.test)^2)
