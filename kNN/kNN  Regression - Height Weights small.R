library(readxl)
library(ggplot2)
library(dplyr)
#predict weight using height and age.

data = read_xlsx("HeightWeightSmall.xlsx")
length(data)
colnames(data)
data = select(data, -c(ID, Age))
ggplot (data, aes (x = Height, y = Weight)) +  geom_point(size = 3)

split = floor(0.8*nrow(data))

train_index = sample(seq_len(nrow(data)), size = split)
train.x = data[train_index,]
test.x =  data[-train_index,]
#removing dependant vars
depvars =  names(train.x) %in% c("Weight")
train.x = train.x[!depvars]
test.x = test.x[!depvars]

train.y = data$Weight[train_index]
test.y = data$Weight[-train_index]


predict = function(train.x, train.y, K, newData){
  distance = c()
  for (i in 1:nrow(train.x)){
    distance[i][1] = abs(train.x$Height[i]-newData)
  }
  #adding distance to the dataframe
  train.x$Weight = train.y
  train.x$EuclideanDistance = distance
  train = train.x
  colnames(train) = c("Height", "Weight", "EuclideanDistance")
  ordered.train = train[order(train$EuclideanDistance),][1:K,]
  
  mean.y = mean(ordered.train$Weight)
  
  return (mean.y)
}

K = 3
error = 0
for (i in 1:nrow(test.x)){
  pred = predict(train.x, train.y, K, as.numeric(test.x[i,]))
  real = test.y[i]
  error = error + abs(pred-real)
  print(round(c(pred, real, abs(pred-real)),2))
}

cat("ABS ERROR:", error/nrow(test.x))
sprintf("ABS ERROR: %f", error/nrow(test.x))
