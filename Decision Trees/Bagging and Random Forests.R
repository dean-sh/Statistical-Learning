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

dim(test)
#Modeling a regression random forest, with m = p variables (all 13 features)
rf.Boston = randomForest(medv~., data = Boston, subset =  train.index 
                         ,mtry = 13, importance = T)

par(mfrow = c(1,1))
plot(rf.Boston,main = "Number of trees vs training error")

 print(rf.Boston)

#Prediction:
rf.y = predict(rf.Boston, newdata = test)
plot(rf.y, test$medv)
abline(0,1)

#Test MSE:
cat("MSE:" ,mean((rf.y-test$medv)^2))

#Now, I will use only 6 variables, "Feature Bagging", to build each tree.
rf.bagging.Boston = randomForest(medv~., data = Boston, subset = train.index,
                                 mtry=6, importance = T)
rf.y = predict(rf.bagging.Boston, newdata = test)
cat("MSE:" ,mean((rf.y-test$medv)^2))

#We got a MSE which is a bit higher, however generally bagging prodoces a better result.

##importance
# Calling "importance()" shows the importance of each variable, calculated by permuting the Out-of-bag examples - for every tree, there are examples which were not used to create it. This sample is used to calculate the importance measure.
# The prediction accuracy is calculated from those OOB examples.
# Then, one variable on the OOB examples is randomly shuffled, effectivly removing its predictive power, while keeping other variables fixed. 
# Then, the difference between the accuracies is measured.
# 
# This is averaged on all the trees and variables, and normalized by the std.

importance(rf.bagging.Boston)
varImpPlot(rf.bagging.Boston, main = "Variable Importance")


  