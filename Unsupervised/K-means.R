# K- means Clustering Analysis


library(ggplot2)
set.seed(42)

X = matrix(rnorm(50*2), ncol=2)

#creating artificial clusters

X[1:25,1] = X[1:25,1] + 2
X[1:25,2] = X[1:25,2] - 2

D = data.frame(X)
plot(D)

D$ID[1:25] = 1
D$ID[26:50] = 2

D$ID = factor(D$ID)

ggplot(data = D) + geom_point(mapping = aes(x = D$X1, y = D$X2, col =D$ID)) 

km = kmeans(D, centers = 2, nstart = 20) #20 initial cluster assignments - trying 20 times

km #has a vector of clusters
km$cluster

D$Cluster = factor(km$cluster)

ggplot(data = D) + geom_point(mapping = aes(x = D$X1, y = D$X2, col =D$ID, shape = D$Cluster, size = 0.5)) 


#trying with more than 2 clusters in kmeans:
km3 = kmeans(D, centers = 2, nstart = 1) #20 initial cluster assignments - trying 20 times

D$Cluster = factor(km3$cluster)

ggplot(data = D) + geom_point(mapping = aes(x = D$X1, y = D$X2, col =D$ID, shape = D$Cluster, size = 0.5)) 
