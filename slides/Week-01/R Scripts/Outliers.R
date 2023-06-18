# Outliers detection in R

# Source is https://statsandr.com/blog/outliers-detection-in-r/


# Introduction ------------------------------------------------------------

# An outlier is a value or an observation that is distant from other observations, 
# that is to say, a data point that differs significantly from other data points. 
# Enderlein (1987) goes even further as the author considers outliers as values that 
# deviate so much from other observations one might suppose a different underlying sampling mechanism

# Although there is no strict or unique rule whether outliers should be removed or not 
# from the dataset before doing statistical analyses, it is quite common to, at least, remove or 
# impute outliers that are due to an experimental or measurement error 
# (like the weight of 786 kg (1733 pounds) for a human). 
# Some statistical tests require the absence of outliers in order to draw sound conclusions, 
# but removing outliers is not recommended in all cases and must be done with caution.

# Removing or keeping outliers mostly depend on three factors:

# 1. The domain/context of your analyses and the research question. 
# In some domains, it is common to remove outliers as they often occur due to a malfunctioning process. 
# In other fields, outliers are kept because they contain valuable information. 
# It also happens that analyses are performed twice, once with and once without outliers to 
# evaluate their impact on the conclusions. 
# If results change drastically due to some influential values, this should caution the researcher 
# to make overambitious claims

# 2. Whether the tests you are going to apply are robust to the presence of outliers or not. 
# For instance, the slope of a simple linear regression may significantly varies with just one outlier, 
# whereas non-parametric tests such as the Wilcoxon test are usually robust to outliers

# 3. How distant are the outliers from other observations? 
# Some observations considered as outliers (according to the techniques presented below) 
# are actually not really extreme compared to all other observations, 
# while other potential outliers may be really distant from the rest of the observations


# The dataset mpg from the {ggplot2} package will be used to illustrate the different 
# approaches of outliers detection in R, and 
# in particular we will focus on the variable hwy (highway miles per gallon).



# Descriptive Statistics --------------------------------------------------

# The first step to detect outliers in R is to start with some descriptive statistics, 
# and in particular with the minimum and maximum.


library(ggplot2)

dat <- ggplot2::mpg

colnames(mpg)
# min-max -----------------------------------------------------------------


summary(dat$hwy)

# OR 
min(dat$hwy)
max(dat$hwy)



# Histogram ---------------------------------------------------------------

# Another basic way to detect outliers is to draw a histogram of the data.

hist(dat$hwy,
     xlab = "hwy",
     main = "Histogram of hwy",
     breaks = sqrt(nrow(dat))
)


# Boxplots ----------------------------------------------------------------

# In addition to histograms, boxplots are also useful to detect potential outliers.


boxplot(dat$hwy, ylab = "hwy")

# Or using ggplot2

ggplot(dat) +
  aes(x = "", y = hwy) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


# A boxplot helps to visualize a quantitative variable by displaying five common 
# location summary (minimum, median, first and third quartiles and maximum) and 
# any observation that was classified as a suspected outlier using the 
# interquartile range (IQR) criterion

# It is also possible to extract the values of the potential outliers based on the IQR criterion 
# thanks to the boxplot.stats()$out function

length(dat$hwy)

boxplot.stats(dat$hwy)$out

# Their position 
out <- boxplot.stats(dat$hwy)$out
out_ind <- which(dat$hwy %in% c(out))
out_ind

# Call all the variables from the data set, by filtering in terms of 
# potential outliers
dat[out_ind, ]

# It is also possible to print the values of the outliers directly on the boxplot with the mtext() function:

boxplot(dat$hwy,
        ylab = "hwy",
        main = "Boxplot of highway miles per gallon" )
mtext(paste("Outliers: ", paste(out, collapse = ", ")))


# Percentiles -------------------------------------------------------------

# This method of outliers detection is based on the percentiles. 
# With the percentiles method, all observations that lie outside the interval 
# formed by the 2.5 and 97.5 percentiles will be considered as potential outliers. 
# Other percentiles such as the 1 and 99, or the 5 and 95 percentiles can also be 
# considered to construct the interval
quantil
lower_bound <- quantile(dat$hwy, 0.025)
lower_bound

upper_bound <- quantile(dat$hwy, 0.975)
upper_bound

outlier_ind <- which(dat$hwy < lower_bound | dat$hwy > upper_bound)
outlier_ind

dat[outlier_ind, "hwy"]

# Or with all variables
dat[outlier_ind, ]

# Or change the boundries to reduce the number of outliers

lower_bound <- quantile(dat$hwy, 0.01)
upper_bound <- quantile(dat$hwy, 0.99)

outlier_ind <- which(dat$hwy < lower_bound | dat$hwy > upper_bound)

dat[outlier_ind, "hwy"]


# Hampel filter -----------------------------------------------------------

# Another method, known as Hampel filter, consists of considering as outliers the values outside the interval (
# formed by the median, plus or minus 3 median absolute deviation

lower_bound <- median(dat$hwy) - 3 * mad(dat$hwy, constant = 1)
lower_bound

upper_bound <- median(dat$hwy) + 3 * mad(dat$hwy, constant = 1)
upper_bound

outlier_ind <- which(dat$hwy < lower_bound | dat$hwy > upper_bound)
outlier_ind

# According to the Hampel filter, there are 3 outliers for the hwy variable.


# Statistical tests -------------------------------------------------------

# In this section, we present 3 more formal techniques to detect outliers:

# 1. Grubb's test
# 2. Dixon's test
# 3. Rosner's test

# Note that the 3 tests are appropriate only when the data (without any outliers) 
# are approximately normally distributed. The normality assumption must thus be verified 
# before applying these tests for outliers

library(car)
qqPlot(dat$hwy)


shapiro.test(dat$hwy)

# From the output, we see that the  p -value < 0.05
# implying that we reject the null hypothesis that the data follow a normal distribution !!

# Grubbs test -------------------------------------------------------------

# The Grubbs test allows to detect whether the highest or lowest value in a dataset is an outlier.

# To perform the Grubbs test in R, we use the grubbs.test() function from the {outliers} package:

# install.packages("outliers")
library(outliers)
test <- grubbs.test(dat$hwy)
test

# The p-value is 0.056. At the 5% significance level, we do not reject the hypothesis that 
# the highest value 44 is not an outlier.

test <- grubbs.test(dat$hwy, opposite = TRUE)
test

# The p-value is 1. At the 5% significance level, we do not reject the null hypothesis that 
# the lowest value 12 is not an outlier.

dat[34, "hwy"] <- 212
 
test <- grubbs.test(dat$hwy)
test


# Dixons test -------------------------------------------------------------

# Similar to the Grubbs test, Dixon test is used to test whether a single 
# low or high value is an outlier. So if more than one outliers is suspected, 
# the test has to be performed on these suspected outliers individually


# Note that Dixon test is most useful for small sample size (usually  n <= 25 ).


# To perform the Dixon's test in R, we use the dixon.test() function from the {outliers} package. 
# However, we restrict our dataset to the 20 first observations as the Dixon test 
# can only be done on small sample size (R will throw an error and accepts only dataset of 
# 3 to 30 observations)
dixon.test(dat$hwy)

subdat <- dat[1:20, ]
subdat$hwy
test <- dixon.test(subdat$hwy)
test

# The results show that the lowest value 15 is an outlier (p-value = 0.007).

test <- dixon.test(subdat$hwy, opposite = TRUE )
test

# The results show that the highest value 31 is not an outlier (p-value = 0.858).


# It is a good practice to always check the results of the statistical test for outliers 
# against the boxplot to make sure we tested all potential outliers

out <- boxplot.stats(subdat$hwy)$out
out
boxplot(subdat$hwy, ylab = "hwy" )
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

# find and exclude lowest value
remove_ind <- which.min(subdat$hwy)
subsubdat <- subdat[-remove_ind, ]
subsubdat
# Dixon test on dataset without the minimum
test <- dixon.test(subsubdat$hwy)
test

# The results show that the second lowest value 20 is not an outlier (p-value = 0.13).


# Rosners test ------------------------------------------------------------

# Rosner's test for outliers has the advantages that:

# it is used to detect several outliers at once 
# (unlike Grubbs and Dixon test which must be performed iteratively to screen for multiple outliers), 
# and
# it is designed to avoid the problem of masking, where an outlier that is close in value 
# to another outlier can go undetected

# Unlike Dixon test, note that Rosner test is most appropriate when the sample size is large ( n >= 20)

# To perform the Rosner test we use the rosnerTest() function from the {EnvStats} package. 
# This function requires at least 2 arguments: the data and the number of suspected outliers k

# install.packages("EnvStats")
library(EnvStats)
test <- rosnerTest(dat$hwy, k = 3 )
test

test$all.stats

