#Random Forest and bagging of Boston Dataset

library(MASS)
library(randomForest)
set.seed(42)

data(Boston)
text(Boston)
attach(Boston)
names(Boston)

#Train-Test Split
train.index = sample(1:nrow(Boston), 0.7*nrow(Boston))
train = Boston[train.index,]
test = Boston[-train.index,]

#Modeling a regression random forest, with m = p variables (all 13 features)
rf.Boston = randomForest(medv~., data = Boston, subset =  train.index 
                         ,mtry = 13, importance = T)

par(mfrow = c(1,1))
plot(rf.Boston) #Number of trees vs training error

print(rf.Boston)


