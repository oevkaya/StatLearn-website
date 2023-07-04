# About the application of CART in R

# Hitters data pruned tree reproduction -----------------------------------

# About package
# install.packages("tree")
library(tree)

# Using the data set from ISLR2 package
library(ISLR2)

data("Hitters")
attach(Hitters)
head(Hitters)
# How to implement 
tree.hitters <- tree(log(Salary) ~ Years + Hits, Hitters)
# tree.hitters
summary(tree.hitters)

# Plotting the fitted tree model
plot(tree.hitters)
text(tree.hitters , pretty = 0)

# To decide the prunning level 
cv.hitters <- cv.tree(tree.hitters, FUN = prune.tree)
names(cv.hitters)
cv.hitters 

prune.hitters <- prune.tree(tree.hitters , best = 3)

plot(prune.hitters)
text(prune.hitters , pretty = 0)

# Use of tree package -----------------------------------------------------

# About package
# install.packages("tree")
library(tree)

# Using the data set from ISLR2 package
library(ISLR2)
attach(Carseats)

head(Carseats)
# 
High <- factor(ifelse(Sales <= 8, "No", "Yes"))
High

# Add a new column in data frame
Carseats <- data.frame(Carseats , High)

# How to implement 
tree.carseats <- tree(High ~ . - Sales, Carseats)
# tree.carseats
summary(tree.carseats)

# Plotting the fitted tree model
plot(tree.carseats)
text(tree.carseats , pretty = 0)

# More realistic scenario, data splitting and model fitting thereafter
set.seed(442)

# Creation of sample
train <- sample(1:nrow(Carseats), 200)
# Keep the rest as a testing data 
Carseats.test <- Carseats[-train, ]
Carseats.test$High
# testing part of response
High.test <- High[-train]
# Carseats.test$High

# Build the classification tree on the training observations 
# by using subset argument below
tree.carseats <- tree(High ~. - Sales, Carseats, subset = train)

# Alternatively
# Carseats <- Carseats[, -1]
# tree.carseats <- tree(High ~ . , Carseats, subset = train)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

summary(tree.carseats)

# Predictions on largest tree
tree.pred <- predict(tree.carseats , Carseats.test , type = "class")
table(tree.pred, High.test)

mean(tree.pred == High.test)

# To decide the prunning level 
cv.carseats <- cv.tree(tree.carseats , FUN = prune.misclass)
names(cv.carseats)
cv.carseats 

# CV error results 
par(mfrow = c(1, 2))

plot(cv.carseats$size , cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$size, type = "b")


# We now apply the prune.misclass() function in order to prune the tree
prune.misclas
prune.carseats <- prune.misclass(tree.carseats , best = 3)

plot(prune.carseats)
text(prune.carseats , pretty = 0)

# About the performance 
tree.pred_pruned <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred_pruned , High.test)

mean(tree.pred_pruned == High.test)

# Playing with the best parameter above

prune.carseats <- prune.misclass(tree.carseats , best = 14)
plot(prune.carseats)
text(prune.carseats , pretty = 0)

tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred , High.test)
# Compute model accuracy rate on test data
mean(tree.pred == Carseats.test$High)

# Use of rpart package ----------------------------------------------------

# tidyverse for easy data manipulation and visualization
# caret for easy machine learning workflow
# rpart for computing decision tree models

library(tidyverse)
library(caret)
library(rpart)

# Data set and EDA

# Data set: PimaIndiansDiabetes2 [in mlbench package]
library(mlbench)

# Load the data and remove NAs
data("PimaIndiansDiabetes2", package = "mlbench")
head(PimaIndiansDiabetes2)

# Help for the data set, for the details of the variables
?PimaIndiansDiabetes2

# Some details for a variable
class(PimaIndiansDiabetes2$pregnant)

# Number of missing values
sum(is.na(PimaIndiansDiabetes2))

# Simply, we removed those missing values
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)

# Inspect the data
head(PimaIndiansDiabetes2)

# Data splitting before model fit
# Split the data into training and test set, using caret package
set.seed(442)
training.samples <- PimaIndiansDiabetes2$diabetes %>%
  createDataPartition(p = 0.8, list = FALSE)

head(training.samples)

# Splitting the whole data frame with respect to above rule: 80%- 20%
train.data  <- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[-training.samples, ]

# Fully grown trees -------------------------------------------------------

# Build the model
model1 <- rpart(diabetes ~ ., data = train.data, method = "class")
model1
class(model1)

# Plot the trees
par(xpd = NA) # Avoid clipping the text in some device
plot(model1)
text(model1, digits = 3)

# About variable importance
model1$variable.importance


# Make predictions on the test data
predicted.classes <- model1 %>%
  predict(test.data, type = "class")
head(predicted.classes)

# Compute model accuracy rate on test data
mean(predicted.classes == test.data$diabetes)

# Confusion matrix idea
table(predicted.classes, test.data$diabetes)

# Pruning the tree --------------------------------------------------------

# Briefly, our goal here is to see if a smaller sub-tree can give us comparable results to the
# fully grown tree. If yes, we should go for the simpler tree because it reduces
# the likelihood of over-fitting.

# In rpart package, this is controlled by the complexity parameter (cp), which imposes a penalty to the
# tree for having two many splits. The default value is 0.01. The higher the cp, the smaller the tree.

# Pruning can be easily performed in the caret package workflow, which invokes the rpart method
# for automatically testing different possible values of cp,
# then choose the optimal cp that maximize the cross-validation accuracy,
# and fit the final best CART model that explains the best our data.

# Fit the model on the training set
# set.seed(442)

# Try also 10-fold CV below !
model2 <- train(
  diabetes ~., data = train.data, method = "rpart",
  trControl = trainControl("cv", number = 5),
  tuneLength = 10
)

# Some details for the train function from caret package
# ?train
model2
summary(model2)
# Plot model accuracy vs different values of
# cp (complexity parameter)
plot(model2)

# Print the best tuning parameter cp that
# maximizes the model accuracy
model2$bestTune

# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
plot(model2$finalModel)
text(model2$finalModel,  digits = 3)

# Decision rules in the model, as a prunned tree
model2$finalModel

# Make predictions on the test data
predicted.classes <- model2 %>% predict(test.data)
# Compute model accuracy rate on test data
mean(predicted.classes == test.data$diabetes)

# Confusion matrix idea
table(predicted.classes, test.data$diabetes)

# Regression trees --------------------------------------------------------

# Similarly to classification trees,
# the following R code uses the caret package to build regression trees and
# to predict the output of a new test data set.

# Data set: We will use the Boston data set [in MASS package]
library(MASS)
data(Boston)
head(Boston)

# Split the data into training and test set with caret package
create
set.seed(442)
training.samples <- Boston$medv %>%
  createDataPartition(p = 0.8, list = FALSE)

# Alternatively 
# training.samples <- createDataPartition(Boston$medv, p = 0.8, list = FALSE)

train.data <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

# Fit the model on the training set
tree.boston <- train(
  medv ~ ., data = train.data, method = "rpart",
  trControl = trainControl("cv", number = 5),
  tuneLength = 10
)

# Model summary
summary(tree.boston)

# Plotting the model
plot(tree.boston)

# Print the best tuning parameter cp that
# minimize the model RMSE
tree.boston$bestTune

# Some other performance measures
tree.boston$results

# Final model details
tree.boston$finalModel

# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
plot(tree.boston$finalModel)
text(tree.boston$finalModel, digits = 3)

# Make predictions on the test data
predictions <- tree.boston %>% predict(test.data)
head(predictions)
# Compute the prediction error RMSE
RMSE(predictions, test.data$medv)

# Pruning tree
tree.boston <- rpart(medv ~ ., train.data)
tree.boston
plot(tree.boston)
text(tree.boston, digits = 3)

pruned.boston <- prune(tree.boston, cp=0.1)
plot(pruned.boston)
text(pruned.boston, digits = 3)

# PLEASE LET ME KNOW IF YOU FACE WITH ANY AWKWARD ERRORS/ISSUES 
# WHEN YOU TRY TO REPRODUCE

