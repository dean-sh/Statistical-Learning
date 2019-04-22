# IRIS Cluster Analysis

attach(iris)
ggplot(iris) + geom_point(mapping = aes(Sepal.Length,Sepal.Width, col = Species))
ggplot(iris) + geom_point(mapping = aes(Petal.Length,Petal.Width, col = Species))

kmeans.fun = function(k){
  d = data.frame(Petal.Length, Petal.Width)
  km = kmeans(d, centers = k, nstart = 20) 
  return(km$tot.withinss)
}

kmeans.fun(3)

wss = rep(NA,10)
for (k in 1:10){
  wss[k] = kmeans.fun(k)
}

# elbow graph - 3 is the best number of clusters.
plot(wss, type = "b")

d = data.frame(Petal.Length, Petal.Width)
km.3 = kmeans(d, centers = 3, nstart = 20) 

iris$Cluster = factor(km.3$cluster)
ggplot(iris) + geom_point(mapping = aes(iris$Petal.Length,iris$Petal.Width, col = iris$Species, shape = iris$Cluster))

table(iris$Species, iris$Cluster)


#Hierarchical Cluster Analysis

df =data.frame(Sepal.Length, Sepal.Width)
dist_mat = dist(d, method = 'euclidean')
hc.avg = hclust(dist_mat, method = "average")

clustering.complete = cutree(hc.avg, 3)
table(iris$Species,clustering.complete)
plot(hc.avg)
