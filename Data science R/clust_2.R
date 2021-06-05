a<- read.csv(file.choose())
View(a)
norm_crime <- scale(crime[,-1])  
View(norm_a)
library(animation)
library(cluster)

#finding number of clusters
cl=(nrow(norm_a)-1)*sum(apply(norm_a,2,var))
for(i in 2:8) cl[i]=sum(kmeans(norm_a,centers=i)$withinss)
plot(1:8,cl,type = "b",xlab = "clusters",ylab = "with in sum of squares",main = "K-means clustering")
library(factoextra)
fviz_nbclust(norm_a,method = 'wss',FUNcluster = kmeans)
fviz_nbclust(norm_a,method = 'silhouette',FUNcluster = kmeans)
fviz_nbclust(norm_a,method = 'gap_stat',FUNcluster = kmeans)

#number of clusters is 2
final <- kmeans(norm_a,2)
finalanim <- kmeans.ani(norm_a,2)
fviz_cluster(final,data = a[-1])
finaldata <- data.frame(final$cluster,a)
View(finaldata)
aggregate(a[,-1],by=list(final$cluster),FUN = mean)