# CV with caret package


# Step 1: Read in data and needed packages. -------------------------------

# libraries needed
library(caret)
library(psych)

# read in the data
data <- sat.act
head(data)

# Previously
summary(lm(ACT ~ gender + age + SATV + SATQ, data = data))


# Step 2: Cross-validation using caret package. ---------------------------

# We are going to use the caret package to predict a participant’s ACT score from gender, 
# age, SAT verbal score, and SAT math score using the “sat.act” data from the psych package, 
# and assess the model fit using 5-fold cross-validation.

# We first set up the number of folds for cross-validation by defining the training control.

help("trainControl")
data_ctrl <- trainControl(method = "cv", number = 5)
class(data_ctrl)

# Then run the regression model
help("train")
model_caret <- train(ACT ~ gender + age + SATV + SATQ,  # model to fit (multiple linear regression)
                     data = data,                        
                     trControl = data_ctrl,             # folds
                     method = "lm",                     # specifying regression model
                     na.action = na.pass)               # pass missing data to model - some models will handle this

# Examine model predictions.
model_caret

# Summarize the results
print(model_caret)
summary(model_caret)

# We can also examine model predictions for each fold.
model_caret$resample
model_caret$results

# Furthermore, we can find the standard deviation around the Rsquared value by examining the R-squared from each fold.
sd(model_caret$resample$Rsquared)
# which is a relatively large window for a Rsquared value


# Look at LOOCV case, k = n
data_ctrl <- trainControl(method = "LOOCV")
# Alternatively, trainControl(method = "cv", number = nrow(data))

# Then run the regression model

model_caret <- train(ACT ~ gender + age + SATV + SATQ,   # model to fit
                     data = data,                        
                     trControl = data_ctrl,              # folds
                     method = "lm",                      # specifying regression model
                     na.action = na.pass)                # pass missing data to model - some models will handle this

model_caret

model_caret$resample

# Summarize the results
print(model_caret)

##### EXTRA with Calculation Error 

# Without using caret
# Define the model
glm.fit <- glm(ACT ~ gender + age + SATV + SATQ, 
               data = data, 
               family = gaussian)

# CV with 5 fold, K = 5
cv_5fold <- cv.glm(data, glm.fit, K = 5)

# Look at the output
cv_5fold$delta
cv_5fold$delta[1]

# NOTE THAT delta could not be calculated here with cv.glm() function

# Further readings --------------------------------------------------------

# http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/#cross-validation-methods

# https://quantdev.ssri.psu.edu/sites/qdev/files/CV_tutorial.html
