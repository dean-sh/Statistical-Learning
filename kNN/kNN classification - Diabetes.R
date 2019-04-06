library(readxl)
library(class)
library(dplyr)

data = read_xlsx("diabetes.xlsx")
attach(data)


data$Outcome = as.factor(data$Outcome)

summary(data)

split = 700
train.X = select (data,-c(Outcome))[1:split,]
train.y = select (data,c(Outcome))[1:split,]
test.X = select (data,-c(Outcome))[(split+1):nrow(data),]
test.y = select (data,c(Outcome))[(split+1):nrow(data),]

train = data[1:split,]
test = data[(split+1):nrow(data),]
cl = pull(train.y)

knn.1 =  knn(train.X, test.X, cl, k=1)
knn.3 =  knn(train.X, test.X, cl, k=3)
knn.5 =  knn(train.X, test.X, cl, k=5)


attributes(.Last.value)
test.y = pull(test.y)


table(knn.1,test.y)
table(knn.3,test.y)
table(knn.5,test.y)

100 * sum(test.y == knn.1)/length(test.y)  # For knn = 1
100 * sum(test.y == knn.3)/length(test.y)  # For knn = 3
100 * sum(test.y == knn.5)/length(test.y)  # For knn = 5


kvec = seq(1, 39, by=2)
probs = c()
for (k in kvec){
  prob = 100 * sum(test.y == knn(train.X, test.X, cl, k))/length(test.y)
  probs = append(probs, prob)
}

kvec = data.frame(kvec,probs)

plot(kvec)
plot.