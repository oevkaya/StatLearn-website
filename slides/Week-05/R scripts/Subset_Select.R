library(ISLR2)
# View(Hitters)

#Details of data set 
names(Hitters)
dim(Hitters)

# There are some missing values, we simply dropped
sum(is.na(Hitters$Salary))
Hitters$Salary

summary(Hitters)
# Dropping NA values from Salary
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

# Consider leaps package for subset selection 
# install.packages("leaps)
library(leaps)
help("regsubsets")
# Fitting a linear model using all the predictors
regfit.full <- regsubsets(Salary ~ ., Hitters)
regfit.full
summary(regfit.full)

# To plot the model object !
plot(regfit.full)
# To look at some details 
summary(regfit.full)$which
summary(regfit.full)$rss

# Try to see whole picture 
regfit.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
reg.summary

# Plot rss value 
plot(reg.summary$rss, type = "l")

par(mfrow = c(2, 2))
plot(reg.summary$rss , xlab = "Number of Variables", 
     ylab = "RSS", type = "l")
plot(reg.summary$adjr2 , xlab = "Number of Variables", 
     ylab = "Adjusted RSq", type = "l")

# Minimum value in terms different metrics
which.min(reg.summary$rss)
which.max(reg.summary$adjr2)
which.min(reg.summary$cp)
which.min(reg.summary$bic)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
# which.min(reg.summary$cp)
points(10, reg.summary$cp[10] , col = "red", cex = 2, pch = 20)

which.min(reg.summary$bic)
plot(reg.summary$bic , xlab = "Number of Variables", ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

# Observing the gain in terms of metrics and 
# which variables should be added or not
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

# Coefficients
reg.summary$bic
coef(regfit.full, 6)

# Forward and backward stepwise algorithms

regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, 
                         nvmax = 19, method = "forward")
# Save teh summary for the model
reg_fwd <- summary(regfit.fwd)

which.min(reg_fwd$bic)
plot(reg_fwd$bic , xlab = "Number of Variables", ylab = "BIC", type = "l")
points(6, reg_fwd$bic[6], col = "red", cex = 2, pch = 20)

# To see the predictors
reg_fwd$outmat[, 6]

# Now consider the backward selection idea 
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters , 
                         nvmax = 19, method = "backward")
reg_bwd <- summary(regfit.bwd)
which.min(reg_bwd$bic)

plot(reg_bwd$bic , xlab = "Number of Variables", ylab = "BIC", type = "l")
points(8, reg_bwd$bic[8], col = "red", cex = 2, pch = 20)

# To see the predictors
reg_bwd$outmat

# Basic comparison of all selections

coef(regfit.full, 7)

coef(regfit.fwd, 7)

coef(regfit.bwd, 7)
