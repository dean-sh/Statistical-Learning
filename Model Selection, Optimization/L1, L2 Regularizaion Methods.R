# Shrinkage Methods - Lasso and Ridge Regularizaion - Linear Regression

# Alternatively to feature selection, we can produce a model with the full variables,
# and minimize some of the coefficients.

#L1 shrinks variables to 0 -performing actual feature selection!

library(glmnet) #gets X, y vectors, using model.matrix
library(ISLR)

Hitters = Hitters
Hitters = na.omit(Hitters)
attach(Hitters)

X = model.matrix(Salary~., Hitters)[,-1] #without names 
y = Hitters$Salary

############### Using the entire Dataset #################

grid = 10^seq(10,-2,length = 100) # a grid of lambdas

ridge.model = glmnet(X, y, alpha = 0, lambda = grid) #0 - ridge, 1 - lasso

plot(ridge.model, xvar = "lambda")
dim(coef(ridge.model)) #we have 20 coefficients with ridge model.

coef(ridge.model)[,50] #many of the coefs are close to 0, but not 0.

sqrt(sum(coef(ridge.model)[-1,50]^2)) #L1 Error

############### Training and Testing L2 regression #################

# We will chose lambda arbitrary for now.
train_index = sample(1:nrow(Hitters), 0.5*nrow(Hitters))
Y.test = y[-train_index]

ridge.model = glmnet(X[train_index,], y[train_index], alpha = 0, lambda = grid)
ridge.predict = predict(ridge.model, s = 4, newx = X[-train_index,]) #s = lambda

MSE.fullmodel = mean((ridge.predict- Y.test)^2) #MSE over test


############## Choosing Lambda with Cross-Validation ###############
set.seed(42)
cv.out = cv.glmnet(X[train_index,], y[train_index], alpha = 0)
plot(cv.out)

?cv.glmnet
best_lambda = cv.out$lambda.min

ridge.predict = predict(ridge.model, s = best_lambda, newx = X[-train_index,]) #using best lambda
MSE.CV.model = mean((ridge.predict- Y.test)^2) #MSE over test with the best lambda
options(scipen = 999)


output = glmnet(X[train_index,],y[train_index], alpha = 0)
predict(output, type = "coefficients", s = best_lambda) #getting the coeffs with the best lmda



############################ Lasso Regression ############################ 
lasso.model = glmnet(X[train_index,], y[train_index], alpha = 1, lambda = grid)
lasso.predict = predict(ridge.model, s = 4, newx = X[-train_index,]) #s = lambda
plot(lasso.model)
MSE.full.lasso = mean((lasso.predict- Y.test)^2) #MSE over test


################### Lasso CV Regression ################# 

cv.out = cv.glmnet(X[train_index,], y[train_index], alpha = 1)
plot(cv.out)

best_lambda = cv.out$lambda.min

lasso.predict = predict(lasso.model, s = best_lambda, newx = X[-train_index,]) #using best lambda
MSE.CV.model = mean((lasso.predict- Y.test)^2) #MSE over test with the best lambda
options(scipen = 999)


output = glmnet(X[train_index,],y[train_index], alpha = 1)
predict(output, type = "coefficients", s = best_lambda) #getting the coeffs with the best lmda
#here many of the coeffs are 0 !! this is a sparse model.

