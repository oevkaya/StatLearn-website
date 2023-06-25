## Recap for Resampling

# tidyverse for easy data manipulation and visualization
# caret for easily computing cross-validation methods
library(tidyverse)
library(caret)

# Data example is 

# Load the data
data("swiss")
head(swiss)
# Inspect the data
sample
sample_n(swiss, 3)

# Split the data into training and test set
set.seed(442) # Reproducibility purpose

# 
create
training.samples <- swiss$Fertility %>%
  createDataPartition(p = 0.8, list = FALSE) # 80-20 % rule 

head(training.samples)

# To create training and testing subsets
train.data  <- swiss[training.samples, ]
test.data <- swiss[-training.samples, ]

# Build the model, multiple linear regression here
model <- lm(Fertility ~ ., data = train.data)
summary(model)

# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test.data)
head(predictions)
# predictions <- predict(model, test.data)
head(test.data$Fertility)

data.frame( R2 = R2(predictions, test.data$Fertility),
            RMSE = RMSE(predictions, test.data$Fertility),
            MAE = MAE(predictions, test.data$Fertility))


model2 <- lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, 
             data = train.data)
summary(model2)

predictions2 <- model2 %>% predict(test.data)

data.frame( R2 = R2(predictions2, test.data$Fertility),
            RMSE = RMSE(predictions2, test.data$Fertility),
            MAE = MAE(predictions2, test.data$Fertility))

# Continue with model 1 including all the variables
# LOOCV
train.control <- trainControl(method = "LOOCV")

# Train the model
model <- train(Fertility ~ ., data = swiss, method = "lm",
               trControl = train.control)

# Summarize the results
print(model)

# CV with 10-fold
train.control <- trainControl(method = "cv", number = 10)

# Train the model
model <- train(Fertility ~., data = swiss, method = "lm",
               trControl = train.control)

# Summarize the results
print(model)

