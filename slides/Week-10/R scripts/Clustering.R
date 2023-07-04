
# Clustering analysis: k-means and hierarchical clustering in R

# Use of other packages as well. 

# Source: https://statsandr.com/blog/clustering-analysis-k-means-and-hierarchical-clustering-by-hand-and-in-r/


# Reading data ------------------------------------------------------------

# Import data
Eurojobs <- read.csv(
  file = "https://statsandr.com/blog/data/Eurojobs.csv",
  sep = ",",
  dec = ".",
  header = TRUE, 
  row.names = 1 # if this is missing !
)

head(Eurojobs) 

dim(Eurojobs) # displays the number of rows and columns

# k-means -----------------------------------------------------------------

# Note that in this case it is not necessary to standardize the data 
# because they are all expressed in the same unit (in percentage). 
# If this was not the case, we would have had to standardize the data 
# 

# kmeans() with 2 groups

model <- kmeans(Eurojobs, centers = 3)
model$cluster

model

# The output model$cluster specifies the group (i.e., 1 or 2) 
# to which each country belongs to.

# Quality of a k-means partition

# BSS and TSS are extracted from the model and stored
(BSS <- model$betweenss)

(TSS <- model$totss)

# We calculate the quality of the partition
BSS / TSS * 100

# The quality of the partition is 51.87%. 
# This value has no real interpretation in absolute terms except that 
# a higher quality means a higher explained percentage

# Note that k-means is a non-deterministic algorithm so running it multiple times 
# may result in different clustering. 
# This is the case because the k-means algorithm uses a random 
# set of initial points to arrive at the final classification

model2 <- kmeans(Eurojobs, centers = 2, nstart = 10)
100 * model2$betweenss / model2$totss

model3 <- kmeans(Eurojobs, centers = 2, nstart = 20)
100 * model3$betweenss / model3$totss


# Adding the nstart argument in the kmeans() function limits this issue 
# as it will generate several different initializations and take the most optimal one, 
# leading to a better stability of the classification

# kmeans() with 3 groups

model3 <- kmeans(Eurojobs, centers = 3, nstart = 10)

BSS3 <- model3$betweenss
TSS3 <- model3$totss
BSS3 / TSS3 * 100

# It can be seen that the clustering into three groups allows 
# for a higher explained percentage and a higher quality.


# Alternative way ---------------------------------------------------------

# An alternative method to perform a k-means is to use the cluster_analysis() 
# function from the {parameters} package:
# install.packages("parameters")
library(parameters)

res_kmeans <- cluster_analysis(Eurojobs,
                               n = 3,
                               method = "kmeans")

res_kmeans$Cluster
res_kmeans$Cluster

predict(res_kmeans) # get clusters

# An advantage of this method is that it is possible to visualize the centers 
# (i.e., the average of each variable for each cluster):

plot(res_kmeans)


# Optimal number of clusters ----------------------------------------------

# the following four approaches:

# 1. Elbow method (which uses the within cluster sums of squares)
# 2. Average silhouette method
# 3. Gap statistic method
# 4. Consensus-based algorithm


# Elbow -------------------------------------------------------------------

# The Elbow method looks at the total within-cluster sum of square (WSS) as 
# a function of the number of clusters.

# load required packages
install.packages(c("factoextra", "NbClust"))

library(factoextra)
library(NbClust)

# Elbow method

fviz_nbclust(Eurojobs, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle

#  This method seems to suggest 4 clusters. However, the Elbow method is sometimes 
# ambiguous and an alternative is the average silhouette method.


# Silhouette --------------------------------------------------------------

# Silhouette method
fviz_nbclust(Eurojobs, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")


# The Silhouette method suggests 2 clusters


# Gap statistic method ----------------------------------------------------

# Gap statistic
set.seed(42)
fviz_nbclust(Eurojobs, kmeans,
             nstart = 25, method = "gap_stat",
             nboot = 500 # reduce it for lower computation time (but less precise results)
) + labs(subtitle = "Gap statistic method")

#  This method suggests only 1 cluster (which is therefore a useless clustering)


# Consensus based ---------------------------------------------------------

# Because no method is clearly better, a fourth alternative is to 
# run many methods and take the number of clusters that is the most agreed upon 
# (i.e., find the consensus).
# This can easily be done with the n_clusters() function from the {parameters} package:



n_clust <- n_clusters(Eurojobs,
                      package = c("easystats", "NbClust", "mclust"),
                      standardize = FALSE)
n_clust

# The optimal number of clusters to retain can also be visualized:
  
plot(n_clust)

# It is also possible to plot clusters by using the fviz_cluster() function. 
# Note that a principal component analysis is performed 
# to represent the variables in a 2 dimensions plane.

set.seed(42)
km_res <- kmeans(Eurojobs, centers = 3, nstart = 20)

fviz_cluster(km_res, Eurojobs, ellipse.type = "norm")


# Hierarchical  -----------------------------------------------------------


# Single Linkage ----------------------------------------------------------

# Hierarchical clustering: single linkage
hclust <- hclust(dist(Eurojobs), method = "single")
hclust

plot(hclust)

# In R, we can even highlight these clusters directly in the dendrogram with the 
# rect.hclust() function:


plot(hclust)
rect.hclust(hclust,
            k = 4, # k is used to specify the number of clusters
            border = "blue"
)


# Complete ----------------------------------------------------------------

hclust <- hclust(dist(Eurojobs), method = "complete")
plot(hclust)
rect.hclust(hclust,
            k = 3, # k is used to specify the number of clusters
            border = "blue"
)

 # Play with the values of k !


# Average -----------------------------------------------------------------

hclust <- hclust(dist(Eurojobs), method = "average")
plot(hclust)
rect.hclust(hclust,
            k = 3, # k is used to specify the number of clusters
            border = "blue"
)




# References 

# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/hclust

# https://rdrr.io/cran/parameters/man/cluster_analysis.html

# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/kmeans

# https://cran.r-project.org/web/packages/NbClust/NbClust.pdf
