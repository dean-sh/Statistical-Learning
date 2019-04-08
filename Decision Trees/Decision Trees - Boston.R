############################################################################
# Decision Tree Algorithm  Boston Dataset        

# 1. binary splitting for creating a big tree over the train tracing data
# 2. cost complexity pruning
# 3. using kfolds CV to choose alpha 
# 4. prunning accourding to the chosen alpha
# 5. testing the tree

############################################################################

library(MASS)
library(ggplot2)
library(tree)

H = Boston

ggplot(data = H) + 
  geom_point(mapping = aes(x = ptratio, y = age, col = medv), size = 2) +
  scale_color_gradientn(colours = rainbow(10))

H = na.omit(H)

attach(H)
rad = factor(rad)

############################################################################
#                         Train-Test Split                                 #
############################################################################

fold = floor(runif(nrow(H),1,11))
H$fold = fold

test.set = H[H$fold ==1,]
train.set = H[H$fold!=1,]


my.tree = tree(medv ~., data = train.set, mindev = 0.001)
#the within-node deviance must be at least this times part of the mindev
#the tree will continue to develop until this is the minimum of the error.
#you can also declare the min-size of points inside each separation.
set.seed(42)
train.tree = prune.tree(my.tree, best = 5)

plot(train.tree)
text(train.tree)

summary(train.tree)

############################################################################
#                             CV on trees                                  #
############################################################################
my.tree = tree(medv ~., data = train.set, mindev = 0.001)
my.tree.cv = cv.tree(my.tree)

plot(my.tree.cv, type = 'b')

my.tree.cv = cv.tree(my.tree, best = 7)

summary(my.tree.cv)
plot(my.tree.cv)
text(my.tree.cv)

my.tree.cv1 = cv.tree(my.tree)

############################################################################
#                      Prediction and MSE                                  #
############################################################################

my.tree = tree(medv ~., data = train.set, mindev = 0.0001)

yhat = predict(my.tree, newdata = test.set)
tree.test = test.set$medv
plot(yhat, tree.test)
abline(0,1)

mean((yhat-tree.test)^2)

trainMSE = c()
cvMSE = c()
testMSE = c()

for (k in 20:1){
  my.tree = tree(medv ~., data = train.set, mindev = 0.001)
  pruned = prune.tree(my.tree, best = k)
  yhat = predict(pruned, newdata = test.set)
  tree.test = test.set$medv
  testMSE[k] = mean((yhat-tree.test)^2)
  
  testhat = predict(pruned)
  trainMSE[k] = mean((testhat-train.set$medv)^2)
}
  
plot(testMSE)

plot(trainMSE)
