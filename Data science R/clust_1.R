library(openxlsx)
A <- read.xlsx(file.choose(),2)
View(A)
a <- A[,-1]
View(a)

#K-means clustering
norm_a <- scale(a)
View(norm_a)
wss=(nrow(norm_a)-1)*sum(apply(norm_a,2,var))
for(i in 2:8)wss[i]=sum(kmeans(norm_a,centers = i)$withinss)
plot(1:8,wss,type = "o",xlab = "no of clusters",ylab = "within sum of squares")
title("scree plot")
twss <- NULL
for(i in 2:8){
  twss <- c(twss,kmeans(norm_a,i)$tot.withinss)
}
twss
plot(2:8,twss,type = "o")
km1 <- kmeans(norm_a,3)
str(km1)
km2 <- kmeans(norm_a,5)
str(km2)

#choosing no of clusters as 5
library(animation)
km <- kmeans(norm_a,5)
kmanimation <- kmeans.ani(norm_a,5)
kmfinal <- data.frame(km$cluster,A)
View(kmfinal)
aggregate(kmfinal[,-c(1,2)],by=list(km$cluster),FUN = mean)


#Hierarchical clustering
View(a)
View(norm_a)
library(dendextend)
d <- dist(norm_a,method = "euclidean")
clust <- hclust(d,method = "ward.D")
clust_col=color_branches(clust,k=5)
plot(clust_col)
groups <- cutree(clust,k=5)
rect.hclust(clust,k=5,border = "red") 
cluster_no <- as.matrix(groups)
finaldata <- data.frame(cluster_no,A)
View(finaldata)