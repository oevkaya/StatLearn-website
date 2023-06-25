
# Cross validation without caret --------------------------------------------------------

# CV and Bootstrap Examples 
library(ISLR2)
# to split the set of observations sample()
# into two halves, by selecting a random subset of 196 observations out of
# the original 392 observations
# set.seed(442)
# train <- sample(392, 196) # 1:392
# head(train)

# Data Partitioning as %75
set.seed(442)
autocar_idx = sample(nrow(Auto), 0.75 * nrow(Auto))
autocar_idx
head(autocar_idx)
length(autocar_idx)

# About data set 
summary(Auto)
dim(Auto)

# Reasonable model search
plot(Auto$horsepower, Auto$mpg)

# training data indices
train = autocar_idx

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
summary(lm.fit)

# OR 
train_Auto = Auto[autocar_idx,]
lm.fit <- lm(mpg ~ horsepower, data = train_Auto)
summary(lm.fit)

# For MSE calculation over the testing data 
testing_Auto = Auto[-autocar_idx,]
mean((testing_Auto$mpg - predict(lm.fit , newdata = testing_Auto))^2)

# LOOCV

#install.packages("boot")
library(boot)
help("glm")
glm.fit <- glm(mpg ~ horsepower, data = Auto, family = "gaussian")
#Above calculation matchs with the output of lm(mpg ~ horsepower, data = Auto)
summary(glm.fit)

# What about the impact of CV 
help("cv.glm")

# Without defining K here 
# leave-one-out cross-validation estimate without any extra model-fitting
cv.err <- cv.glm(Auto, glm.fit)
cv.err

cv.err$K
# The first component is the raw cross-validation estimate of prediction error
# The second component is the adjusted cross-validation estimate
cv.err$delta
cv.err$delta[1]

# By choosing some K 
cv.err <- cv.glm(Auto, glm.fit, K = 5) # Corresponds 5-fold CV
cv.err

cv.err$K
# The first component is the raw cross-validation estimate of prediction error
# The second component is the adjusted cross-validation estimate
cv.err$delta[1]
cv.err$delta[2]
  
# Example for K= 10 fold for the same data 
set.seed(542)
glm.fit <- glm(mpg ~ horsepower, data = Auto, family = gaussian)
cv_10fold <- cv.glm(Auto, glm.fit, K = 10)
cv_10fold$delta[1]

# Based on polynomial regression !
cv.error <- rep(0, 10)

for (i in 1:10) { 
  glm.fit <- glm(mpg ~ poly(horsepower , i), data = Auto)
cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1] 
}
cv.error


# Based on polynomial regression !
# For K = 10

cv.error.10 <- rep(0, 10)
for (i in 1:10) { 
  glm.fit <- glm(mpg ~ poly(horsepower , i), data = Auto)
cv.error.10[i] <- cv.glm(Auto , glm.fit , K = 10)$delta[1] }
cv.error.10

# Based on polynomial regression !
# For K = 5

cv.error.5 <- rep(0, 10)
for (i in 1:10) { glm.fit <- glm(mpg ~ poly(horsepower , i), data = Auto)
cv.error.5[i] <- cv.glm(Auto, glm.fit, K=5)$delta[1] }
cv.error.5


# Bootstrap ---------------------------------------------------------------

# Bootstrap part 
summary(Portfolio)
alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  alpha <- (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
  return(alpha)
}

alpha.fn(Portfolio, 1:100)

sample(10, 10, replace = T)

set.seed(442)
alpha.fn(Portfolio, sample(100, 100, replace = T))

alpha_est <- rep(0, 1000)
head(alpha)
length(alpha)

for (i in 1:length(alpha_est)) {
  alpha_est[i] <- alpha.fn(Portfolio, sample(100, 100, replace = T))
}

head(alpha_est)  

# About distributions for the alpha quantity
mean(alpha_est)
sqrt(var(alpha_est))

hist(alpha_est)  
boxplot(alpha_est)

# Instead, use the boot() function 
?boot
boot(Portfolio, alpha.fn, R = 1000)

# bootstrap for accuracy of linear regression coefficients
boot.fn <- function(data, index){
  coef(lm(mpg ~ horsepower , data = data , subset = index))
}

boot.fn(Auto, 1:392)

# Add resampling by using sample function 
boot.fn(Auto, sample(392, 392, replace = T))

boot(Auto, boot.fn, 1000)

summary(lm(mpg ~ horsepower , data = Auto))$coef




