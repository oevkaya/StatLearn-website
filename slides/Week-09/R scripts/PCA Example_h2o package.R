# PCA with another package in R

# Prerequisite packages

library(dplyr)       # basic data manipulation and plotting
library(ggplot2)     # data visualization
install.packages("h2o")
library(h2o)         # performing dimension reduction


# About Data --------------------------------------------------------------

# To illustrate dimension reduction techniques,
# we’ll use the my_basket data set (Section 1.4).
# This data set identifies items and quantities purchased for 2,000 transactions
# from a grocery store
url <- "https://koalaverse.github.io/homlr/data/my_basket.csv"
my_basket <- readr::read_csv(url)
dim(my_basket)


# Preprocessing -----------------------------------------------------------

# To perform dimension reduction techniques, generally,
# the data should be prepared as follows:

# 1. Data are in tidy format;
# 2. Any missing values in the data must be removed or imputed;
# 3. Typically, the data must all be numeric values
# (e.g., one-hot, label, ordinal encoding categorical features);
# 4. Numeric data should be standardized (e.g., centered and scaled)
# to make features comparable.



# Idea --------------------------------------------------------------------

# Dimension reduction methods, such as PCA, focus on reducing the feature space,
# allowing most of the information or variability in the data set to be
# explained using fewer features;
# in the case of PCA, these new features will also be uncorrelated


# Finding PCA -------------------------------------------------------------

# There are several built-in and external packages to perform PCA in R.
# We recommend to use h2o as it provides consistency across
# the dimension reduction methods we’ll discuss later and
# it also automates much of the data preparation steps previously discussed
# (i.e., standardizing numeric features, imputing missing values, and encoding categorical features).


h2o.no_progress()  # turn off progress bars for brevity
h2o.init(max_mem_size = "5g")  # connect to H2O instance

# First, we convert our my_basket data frame to an appropriate h2o object
# and then use h2o.prcomp() to perform PCA.
# A few of the important arguments you can specify in h2o.prcomp() include:

# convert data to h2o object
my_basket.h2o <- as.h2o(my_basket)

# run PCA
my_pca <- h2o.prcomp(
  training_frame = my_basket.h2o,
  pca_method = "GramSVD", # When your data contains mostly numeric data
  k = ncol(my_basket.h2o),
  transform = "STANDARDIZE", # Character string specifying how (if at all) your data should be standardized.
  impute_missing = TRUE,
  max_runtime_secs = 1000
)

# About the model
glimpse(my_pca)

# For output
my_pca

# The most important information is stored in
my_pca@model$importance

# Naturally, the first PC (PC1) captures the most variance
# followed by PC2, then PC3, etc


# Loadings ----------------------------------------------------------------

# The  loadings represent each features influence on the associated PC

my_pca@model$eigenvectors %>%
  as.data.frame() %>%
  mutate(feature = row.names(.)) %>%
  ggplot(aes(pc2, reorder(feature, pc2))) +
  geom_point()

# We can also compare PCs against one another.
# how the different features contribute to PC1 and PC2.

my_pca@model$eigenvectors %>%
  as.data.frame() %>%
  mutate(feature = row.names(.)) %>%
  ggplot(aes(pc1, pc2, label = feature)) +
  geom_text()


# Selection of PCAs -------------------------------------------------------

# There are three common approaches in helping to make this decision:

# 1. Eigenvalue criterion
# 2. Proportion of variance explained criterion
# 3. Scree plot criterion

# h2o.prcomp() automatically computes the standard deviations of the PCs,
# which is equal to the square root of the eigenvalues.
# Therefore, we can compute the eigenvalues easily and identify PCs
# where the sum of eigenvalues is greater than or equal to 1

# Compute eigenvalues
eigen <- my_pca@model$importance["Standard deviation", ] %>%
  as.vector() %>% .^2

# Sum of all eigenvalues equals number of variables
sum(eigen)

# Find PCs where the sum of eigenvalues is greater than or equal to 1
which(eigen >= 1)

# 2. Proportion of variance explained criterion

# The proportion of variance explained (PVE) identifies the optimal number
# of PCs to keep based on the total variability that we would like to account for.

# Extract and plot PVE and CVE
ve <- data.frame(
  PC  = my_pca@model$importance %>% seq_along(),
  PVE = my_pca@model$importance %>% .[2,] %>% unlist(),
  CVE = my_pca@model$importance %>% .[3,] %>% unlist() )

ve %>%
  tidyr::gather(metric, variance_explained, -PC) %>%
  ggplot(aes(PC, variance_explained)) +
  geom_point() +
  facet_wrap(~ metric, ncol = 1, scales = "free")

# Note that
# The first PCt in our example explains 5.46% of the feature variability,
# and the second principal component explains 5.17%

# How many PCs required to explain at least 75% of total variability
min(which(ve$CVE >= 0.75))

# What amount of variability is reasonable?
# This varies by application and the data being used.

# 3. Scree Plot

# A scree plot shows the eigenvalues or PVE for each individual PC.
# Most scree plots look broadly similar in shape, starting high on the left,
# falling rather quickly, and then flattening out at some point

data.frame(
  PC  = my_pca@model$importance %>% seq_along,
  PVE = my_pca@model$importance %>% .[2,] %>% unlist()
) %>%
  ggplot(aes(PC, PVE, group = 1, label = PC)) +
  geom_point() +
  geom_line() +
  geom_text(nudge_y = -.002)


# Final Thoughts ----------------------------------------------------------

# So how many PCs should we use in the my_basket example?
# The frank answer is that there is no one best method for determining
# how many components to use.
# In this case, differing criteria suggest to retain 8 (scree plot criterion),
# 10 (eigenvalue criterion), and
# 27 (based on a 75% of variance explained requirement) components







