#Author: Dr.R.Parvathi,Professor, School of Computing Science & Engineering, VIT Chennai
#Ex1- Visualization of K Means Clustering
#R version 3.3.2 (2016-10-31)
#RStudio version 1.2.1335
rm(list=ls())
#1. Load the iris dataset and view the data.

#Load and view dataset
require("datasets")
data("iris") # load Iris Dataset
str(iris) #view structure of dataset
#2. Display the Statistical Summary of the dataset
summary(iris) #view statistical summary of dataset
head(iris) #view top  rows of dataset
#3. Apply the preprocessing to remove the class attribute eg., Species , since #clustering is a type of unsupervised learning
#Preprocess the dataset
#Since clustering is a type of Unsupervised Learning, we would not require Class Label(output) during 
#execution of our algorithm. We will, therefore, remove Class Attribute “Species” 
#and store it in another variable. 
#We would then normalize the attributes between 0 and 1 using our own function.
iris.new<- iris[,c(1,2,3,4)]
iris.class<- iris[,"Species"]
head(iris.new)
head(iris.class)
#4. Create a function to normalize the data before clustering
# Normalization
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
iris.new$Sepal.Length<- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width<- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length<- normalize(iris.new$Petal.Length)
iris.new$Petal.Width<- normalize(iris.new$Petal.Width)
head(iris.new)
#5. Apply k-means clustering algorithm with k = 3
result<- kmeans(iris.new,3) #aplly k-means algorithm with no. of centroids(k)=3
#6. Find the number of records in each cluster
result$size # gives no. of records in each cluster
#7. Display the cluster center data point values 
result$centers # gives value of cluster center datapoint value(3 centers for k=3)
#8. Display the cluster vector showing the cluster where each record falls
result$cluster #gives cluster vector showing the cluster where each record falls

# Verify results of clustering
par(mfrow=c(2,2), mar=c(5,4,2,2))
#9. Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.new[c(1,2)], col=result$cluster)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
#10. Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.new[c(1,2)], col=iris.class)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
#11.Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.new[c(3,4)], col=result$cluster)# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
#12.Plot to see how Petal.Length and Petal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.new[c(3,4)], col=iris.class)
result$cluster <- as.factor(result$cluster)
#13.  Install the package ggplot2 and import it.
library(ggplot2)
#14. Plot the clusterresults using ggplot
ggplot(iris.new, aes(Petal.Length, Petal.Width, color = result$cluster)) + geom_point()
plot(iris.new[c("Sepal.Length", "Sepal.Width")], col=result$cluster)
#15. Display the clustering results with all parameters 
#points(result$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=23, cex=3)
plot(iris.new[,], col=result$cluster)
#16. Display the results in table
table(result$cluster,iris.class) # Result of table shows that Cluster 1 corresponds to Virginica, Cluster 2 corresponds to Versicolor and Cluster 3 to Setosa.
#Total number of correctly classified instances are: 36 + 47 + 50= 133
#Total number of incorrectly classified instances are: 3 + 14= 17
#Accuracy = 133/(133+17) = 0.88 i.e our model has achieved 88% accuracy!
#In order to improve this accuracy further, we may try different values of “k”. 
#=============================================================================
# K means algorithms with Animation
#==============================================================================
#17. Display the K Means Algorithm with Animation and visualize the changes in the cluster center
library(animation)
km1<-kmeans.ani(iris.new,3)
#18. Import factoextra package and visualize the cluster result
library(factoextra) # clustering algorithms & visualization
fviz_cluster(result, data = iris.new)
#19. Explore the cluster analysis result with various value of k like 3,4,5
k2 <- kmeans(iris.new, centers = 2, nstart = 25)
k3 <- kmeans(iris.new, centers = 3, nstart = 25)
k4 <- kmeans(iris.new, centers = 4, nstart = 25)
k5 <- kmeans(iris.new, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = iris.new) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = iris.new) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = iris.new) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = iris.new) + ggtitle("k = 5")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)
