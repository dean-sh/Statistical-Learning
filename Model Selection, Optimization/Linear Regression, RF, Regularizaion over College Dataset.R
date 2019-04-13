#Comparing multiple models:
# 1- linear regression (model selection),
# 2 -3 Ridge, Lasso linear regression
# 4 best pruned- Decision Tree
# 5 best random forest model
library(ISLR)

data(College)
text(College)
attach(College)
names(College)

plot(Apps~., data = College)

#Correlation:
library(corrplot)
corrplot(cor(College[,-1]))
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = cor(College[,-1]), col = palette, symm = TRUE)

#Train Test Split:
train.index = sample(1:nrow(College), 0.7*nrow(College))
train = College[train.index,]
test = College[-train.index,]

############################################################

# 1 - Linear Regression - subset selection

lr.best = regsubsets(Apps~., data = train, nvmax = 17)
lr.best.summary = summary(lr.best)

#Predictions

#model.matrix creates the X matrix
test.mat = model.matrix(Apps~., data = test)
test.mse = rep(NA, 17)
#for each model in size i, create a vector of the coefficiens and multiply X to get y.
#Y = X * b > {Nx1} = {N x (P+1)} * {(P+1) x 1}
for (i in 1:17){
  coeffs.i = coef(lr.best, id = i)
  pred = test.mat[,names(coeffs.i)] %*% coeffs.i
  test.mse[i] = mean((test$Apps-pred)^2)
}

which.min(test.mse)
coef(lr.best, which.min(test.mse))

fit.best.summary = lr.best.summary
par(mfrow = c(2,2))
plot(fit.best.summary$cp, xlab = "Number of Variables", ylab = "CP", type = "b")
points(which.min(fit.best.summary$cp), fit.best.summary$cp[which.min(fit.best.summary$cp)], col = "red")
plot(fit.best.summary$adjr2, xlab = "Number of Variables", ylab = "adjr2", type = "b")
points(which.max(fit.best.summary$adjr2), fit.best.summary$adjr2[which.max(fit.best.summary$adjr2)], col = "red")
plot(fit.best.summary$rss, xlab = "Number of Variables", ylab = "rss", type = "b")
points(which.min(fit.best.summary$rss), fit.best.summary$rss[which.min(fit.best.summary$rss)], col = "red")
plot(fit.best.summary$bic, xlab = "Number of Variables", ylab = "bic", type = "b")
points(which.min(fit.best.summary$bic), fit.best.summary$bic[which.min(fit.best.summary$bic)], col = "red")

#best variables are Private, Accept, Enroll, Top10perc, Top25, F.under, P.under, Outstate,
#Roobvoard, PHD, sfration, Expend, Grad.rate.

#Building the best model:

lr.bestmodel = lm(Apps~ Accept + Enroll +  Top10perc + Outstate +
                    PhD + Grad.Rate + PhD*Grad.Rate +  Expend , 
                  data = train)
summary(lr.bestmodel) #R2 = 93%
plot(lr.bestmodel)

#Prediction:
lr.predict = predict(lr.bestmodel, newdata = test)

#MSE - Best Linear regression model: 887,000
MSE.LR = mean((lr.predict-test$Apps)^2)

par(mfrow = c(1,1))
plot(lr.predict, test$Apps)
abline(0,1)

############################################################

# 2 - Ridge Regularization,  Linear Regression - full model
# Choosing lambda using cross-validation

library(glmnet)
X = model.matrix(Apps~., College) #without names 
y = College$Apps

grid = 10^seq(10,-2,length = 100) # a grid of lambdas

#implement a grid-search for lambda and see how the coeefs change as lambda increases
ridge.model = glmnet(X, y, alpha = 0, lambda = grid) #0 - ridge, 1 - lasso

plot(ridge.model, xvar = "lambda")

dim(coef(ridge.model)) #we have 19 coefficients with ridge model - b0+18.

set.seed(42)
cv.ridge.model = cv.glmnet(X[train.index,], y[train.index], alpha = 0)
plot(cv.ridge.model)

best_lambda = cv.ridge.model$lambda.min #best log(lambda) = 6, best.lambda = 426
log(best_lambda)

ridge.predict = predict(ridge.model, s = best_lambda, newx = X[-train.index,]) #using best lambda

#MSE - Lasso: 880,000
MSE.CV.Ridge.model = mean((ridge.predict- y[-train.index])^2) #MSE over test with the best lambda

plot(ridge.predict, y[-train.index])
abline(0,1)

output = glmnet(X[train.index,],y[train.index], alpha = 0)
bestRidgeCoef = predict(output, type = "coefficients", s = best_lambda) #getting the coeffs with the best lmda

############################################################

# 3 - Lasso Regularization,  Linear Regression - full model
# Choosing lambda using cross-validation

#implement a grid-search for lambda and see how the coeefs change as lambda increases
lasso.model = glmnet(X, y, alpha = 1, lambda = grid) #0 - ridge, 1 - lasso

plot(lasso.model, xvar = "lambda")
#Many of the variables decrease to 0, and we basically perform feature selection.

dim(coef(lasso.model)) #we have 19 coefficients with ridge model - b0+18.

set.seed(42)
cv.lasso.model = cv.glmnet(X[train.index,], y[train.index], alpha = 1)
plot(cv.lasso.model)

best_lambda = cv.lasso.model$lambda.min #best log(lambda) = 19.1, best.lambda = 2.9
log(best_lambda)

lasso.predict = predict(ridge.model, s = best_lambda, newx = X[-train.index,]) #using best lambda

#MSE - Lasso: 824,530
MSE.CV.lasso.model = mean((lasso.predict- y[-train.index])^2) #MSE over test with the best lambda


plot(ridge.predict, y[-train.index])
abline(0,1)

output = glmnet(X[train.index,],y[train.index], alpha = 1)
bestLassoCoef = predict(output, type = "coefficients", s = best_lambda) #getting the coeffs with the best lmda
#many coefs are 0 - this is a sparse model.
dimLasso = sum(bestLassoCoef!=0)

############################################################

# 4 - Vanilla Decision Tree
# Choosing prunning method using Cross Validation

library(tree)
library(rpart)
library(rpart.plot)
library(caret)

treefit = tree(Apps ~ ., data = train, mindev = 0.001)
summary(treefit)

tree.rpart = rpart(Apps~., data = train, control = rpart.control(cp = 0))
summary(tree.rpart, cp=1)

#Train accuracy and Kappa using Caret
postResample(predict(tree.rpart, train), train$Apps)

rpart.plot(tree.rpart, cex = 0.5)

#Pruning

treefit.cv = cv.tree(treefit)

plot(treefit.cv, type = 'b')

pruned.tree = cv.tree(treefit, best = 8)
plot(pruned.tree)
text(pruned.tree, cex=0.7)


#Prediction Acc over the test
pred = predict(prune.tree, test)
pred1 = predict(treefit, test)

postResample(pred, test$Apps)
postResample(pred1, test$Apps)

rpart.cv.model = train(train[,-2], train[,2],
                       method = 'rpart',
                       trControl = trainControl(method = 'cv', number = 10), #10 fold CV
                       tuneGrid = expand.grid(cp = seq(0, 1, length.out = 10)))
rpart.cv.model

plot(rpart.cv.model)
rpart.cv.model$bestTune 
rpart.cv.model$results


plotcp(tree.rpart)

rpart.best = rpart(Apps~., data = train, control = rpart.control(cp = 0))

rpart.plot(rpart.best)

#Pruned model: Accuracy 0.87
postResample(pred = predict(rpart.best, data = train), train$Apps)
#Unpruned model: Accuracy 0.81
postResample(pred = predict(tree.rpart, data = train), train$Apps)


yhat = predict(tree.rpart, newdata = test)
plot(yhat, test$Apps)
abline(0,1)

MSE.DecisionTree = mean((yhat-test$Apps)^2)

#unPruned:

plot(pred1, test$Apps)
abline(0,1)

MSE.DecisionTree.p = mean((pred1-test$Apps)^2)


############################################################

# 5 - Random Forest Regression

#Modeling a regression random forest, with m = p variables (all 13 features)
library(randomForest)
rf = randomForest(Apps~., data = College, subset =  train.index 
                         ,mtry = 18, importance = T)

par(mfrow = c(1,1))
plot(rf ,main = "Number of trees vs training error")

print(rf)

#Prediction - full RF model
rf.full.y = predict(rf, newdata = test)
plot(rf.full.y, test$Apps)
abline(0,1)

cat("RF MSE:" ,mean((rf.full.y-test$Apps)^2))

#MSE Random Forest (All features) = 735,000
MSE.RF.full = mean((rf.full.y-test$Apps)^2)

importance(rf)
varImpPlot(rf, main = "Variable Importance")


#finding the best bagging variable
MSE.rf = rep(NA, 18)

for (b in 1:18){
  rf.bagging = randomForest(Apps~., data = College, subset = train.index,
                            mtry=13, importance = T)
  rf.bagged.y = predict(rf.bagging, newdata = test)
  cat("MSE:" ,mean((rf.bagged.y-test$Apps)^2))
  MSE.rf[b] = mean((rf.bagged.y-test$Apps)^2)
}

#M TRy = 10
MSE.rf[which.min(MSE.rf)]

#best forest:
rf.bagging = randomForest(Apps~., data = College, subset = train.index,
                                 mtry=10, importance = T)
rf.bagged.y = predict(rf.bagging, newdata = test)
cat("MSE:" ,mean((rf.bagged.y-test$Apps)^2))

#MSE Random Forest ~ 710,000 (best so far)
