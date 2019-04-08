############################################################################
# Decision Tree Algorithm  - Classification 


#Pmk - the proportion of the samples in the training set, in 
# m set, that belongs to group k.

#Gini index gets smaller values if Pmk is close to 0 or 1 - Purity of the node.
############################################################################

library(ISLR)
data = Carseats

str(data)

#Turning the depandant variable into categorical
data$Sales = as.factor(ifelse(Carseats$Sales<=9, "Low", "High"))
str(data)

seatTree = tree(Sales~. , data = data)

summary(seatTree)
plot(seatTree)
text(seatTree, pretty = 0)


tree.cv = cv.tree(seatTree, best = 5)
plot(tree.cv)
text(tree.cv, pretty = 0)

summary(tree.cv)


#train-test split

set.seed(42)
seat_ixc = sample(1:nrow(data),200)
train = data[seat_ixc,]
test  = data[-seat_ixc,]


TrainTree = tree(Sales~. , data = train)

summary(TrainTree)
plot(TrainTree)
text(TrainTree, pretty = 0)

seat_trn_predict = predict(TrainTree, train, type="class")
seat_test_predict = predict(TrainTree, test, type="class")

#Confusion Matrixes:
table(predicted = seat_trn_predict, actual = train$Sales)
table(predicted = seat_test_predict, actual = test$Sales)

accuracy = function(actual, predicted){
  return(mean(actual==predicted))
}

#Accuracy:
accuracy(predicted = seat_trn_predict, actual = train$Sales)
accuracy(predicted = seat_test_predict, actual = test$Sales)


#Cross Validation:

