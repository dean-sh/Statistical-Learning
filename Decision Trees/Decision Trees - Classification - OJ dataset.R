library(ISLR)
library(tree)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)

OJ = OJ
attach(OJ)
set.seed(1)

# inTrain <- createDataPartition(OJ$Purchase, p = 800/1070, list = FALSE)
smp_size = 800
train_index = sample(seq_len(nrow(OJ)), size = smp_size) # sample from seq of row numbers

training <- OJ[train_index,]
testing <- OJ[-train_index,]

treefit = tree(Purchase ~ ., data = training, mindev = 0.001, method = "class")
summary(treefit)

tree.rpart = rpart(Purchase~., data = training, method = "class", control = rpart.control(cp = 0))
summary(tree.rpart, cp=1)


#Train accuracy and Kappa using Caret
postResample(predict(tree.rpart, training, type = 'class'), training$Purchase)

tree.rpart
rpart.plot(tree.rpart, cex = 0.5)

#prediction of test set
pred = predict(tree.rpart, testing, type = "class")
pred1 = predict(treefit, testing, type = "class")

postResample(pred, testing$Purchase)

caret::confusionMatrix(pred, testing$Purchase)
caret::confusionMatrix(pred1, testing$Purchase)
plotcp(tree.rpart)


#rpart model is better than the tree model, however results are similar at this time 
#This is probably due to rpart using a different algorithm of tree building.

rpart.cv.model = train(training[,-1], training[,1],
                        method = 'rpart',
                        trControl = trainControl(method = 'cv', number = 10), #10 fold CV
                        tuneGrid = expand.grid(cp = seq(0, 0.5, length.out = 10)))
rpart.cv.model

plot(rpart.cv.model)
rpart.cv.model$bestTune 
#best tree complexity is cp = 0.4444, however results are the same for cp =[0.05, 0.44]

rpart.cv.model$results

#Modeling a tree based on the best CV parameters
rpart.best = rpart(Purchase~., data = training, method = "class", 
                   control = rpart.control(cp = 0.04))

rpart.plot(rpart.best)

#Pruned model: Accuracy 0.87
postResample(pred = predict(rpart.best, data = training, type = "class"), training$Purchase)
#Unpruned model: Accuracy 0.81
postResample(pred = predict(tree.rpart, data = training, type = "class"), training$Purchase)

#Unpruned Test acc: Accuracy = 0.492
postResample(pred = predict(rpart.best, data = testing, type = "class"), testing$Purchase)
#Unpruned Test acc: Accuracy = 0.503
postResample(pred = predict(tree.rpart, data = testing, type = "class"), testing$Purchase)


#In this case, the pruned model provides worse results.
#However, this shouldn't mean that prunning leads to worse models in the general case.


#Comparing to logistic regression:

fold = 10
folds <- cut(seq(1,nrow(OJ)),breaks=fold,labels=FALSE)

for(i in 1:fold){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- OJ[testIndexes, ]
  trainData <- OJ[-testIndexes, ]
  model = glm(Purchase~., data = training, family = binomial)
  test.prediction = predict(object = model, newdata = testing, type = 'response')
  predictions = rep("CH", length(testing$Purchase))
  predictions[test.prediction>0.5] = "MM" #Cutoff probability
  #
}


model = glm(Purchase~., data = training, family = binomial)
library(boot)
cv.error= cv.glm(training, model, K=10)$delta[1]  # 10 fold cross validation
test.prediction = predict(object = model, newdata = testing, type = 'response')
predictions = rep("CH", length(testing$Purchase))
predictions[test.prediction>0.5] = "MM" #Cutoff probability

#GLM accuracy
postResample(pred = predictions, testing$Purchase)


library(pROC)
roc(response = testing$Purchase, predictor = test.prediction, auc = T, plot = T)


contrasts(Purchase)