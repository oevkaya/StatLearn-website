# PCA Example with some available data !!!

data("USArrests")

dim(USArrests)
states <- row.names(USArrests)
states

names(USArrests)

# Be aware of the variability
apply(USArrests , 2, mean)
apply(USArrests , 2, var)

# PCA with scaling 
pr.out <- prcomp(USArrests , scale = TRUE)
pr.out
names(pr.out)

# Summary of the model 
summary(pr.out)
# The center and scale components correspond to the means and standard
# deviations of the variables
pr.out$center
pr.out$scale

# The rotation matrix provides the principal component loadings
pr.out$rotation

# PC1 = -0.5358995 * Murder -0.5831836 * Assault -0.2781909 * UrbanPop -0.5434321 * Rape

# That is, the kth column is the kth principal component score vector
dim(pr.out$x)
head(pr.out$x)

# Scree plot 
plot(pr.out)

# biplot 
biplot(pr.out , scale = 0)

# The prcomp() function also outputs the standard deviation of each principal
# component and the corresponding explained variance by each PC

pr.out$sdev
pr.var <- pr.out$sdev^2

# To compute the proportion of variance explained by each PC, 
# simply we have a division to get
pve <- pr.var / sum(pr.var)
pve

# We can plot the PVE explained by each component

par(mfrow = c(1, 2))
plot(pve , xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1),
     type = "b")

plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")


# Note that the difference between princomp() and prcomp()
# By default it uses covariance matrix without any scaling on that data
summary(princomp(USArrests))

# To get similar values we need to use cor = TRUE like we got prcomp()
pc.cr <- princomp(USArrests, cor = TRUE) # =^= prcomp(USArrests, scale=TRUE)
summary(pc.cr)


# Documentation 

# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/prcomp
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/princomp

# Another example for biplot 
# https://www.geo.fu-berlin.de/en/v/soga/Geodata-analysis/Principal-Component-Analysis/principal-components-basics/Interpretation-and-visualization/index.html#:~:text=Interpreting%20Biplots,in%20a%20single%20biplot%20display.&text=The%20plot%20shows%20the%20observations,principal%20components%20(synthetic%20variables).

# For another example and nice PCA plots 
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/#package-for-pca-visualization

# From youtube (A good channel to watch)

# https://www.youtube.com/watch?v=X4wsXba_tZI
