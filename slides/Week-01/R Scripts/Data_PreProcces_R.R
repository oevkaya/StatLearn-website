# Data Preprocessing Example in R

# Source is https://www.section.io/engineering-education/data-preprocessing-in-r/



# Step 1: Importing the dataset ---------------------------------------------------------

library(readr)
Dataset = read_csv('Dataset.csv') 

# view(Dataset)
summary(Dataset)


# Step 2: Handling the missing data ---------------------------------------



# From the dataset, the Age and Salary column report missing data. 
# Before implementing our machine learning models, this problem needs to be solved

# 1. Delete the observation reporting the missing data:
# 2. Replace the missing data with the average of the feature in which the data is missing:
Dataset$Age
Dataset$Age = ifelse(is.na(Dataset$Age),
              ave(Dataset$Age, FUN = function (x)mean(x, na.rm = TRUE)), Dataset$Age)
Dataset$Age

# Similarly for Salary column

Dataset$Salary = ifelse(is.na(Dataset$Salary),
                  ave(Dataset$Salary, FUN = function (x)mean(x, na.rm = TRUE)), Dataset$Salary)
Dataset$Salary



# Step 3: Encoding categorical data ---------------------------------------

# Encoding refers to transforming text data into numeric data. 
# Encoding Categorical data simply means we are transforming data that fall into 
# categories into numeric data.

# Let start by encoding the Country column.

Dataset$Country = factor(Dataset$Country, 
                         levels = c('France','Spain','Germany'), 
                         labels = c(1.0, 2.0 , 3.0 ))
Dataset$Country
# We do the same for the purchased column.

Dataset$Purchased = factor(Dataset$Purchased,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1))
Dataset$Purchased
class(Dataset$Purchased)
# Dataset$Purchased[is.na(Dataset$Purchased)] <- 0
# as.factor(Dataset$Purchased)


# Step 4: Splitting the dataset into the training and test set ------------

# whenever we are building a machine learning model, the idea is to implement it on the training set 
# and evaluate it on the test set

install.packages("caTools")
library(caTools)# required library for data splition
set.seed(123)
split = sample.split(Dataset$Purchased, SplitRatio = 0.8)# returns true if observation goes to the Training set and false if observation goes to the test set.

#Creating the training set and test set separately
training_set = subset(Dataset, split == TRUE)
test_set = subset(Dataset, split == FALSE)
training_set
test_set


# Step 5: Feature scaling -------------------------------------------------

# It happens that, the features with the large units dominate those with small units 
# when it comes to calculation of the Euclidian distance and 
# it will be as if those features with small units do not exist.

# The normalization technique is used when the data is normally distributed 
# while standardization works with both normally distributed and 
# the data that is not normally distributed.

training_set[, 2:3] = scale(training_set[, 2:3])
test_set[, 2:3] = scale(test_set[, 2:3])
training_set
test_set
