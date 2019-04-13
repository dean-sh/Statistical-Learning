#Feature Selection - Hitters

library(ISLR)
library(leaps)

Hitters = Hitters
Hitters = na.omit(Hitters)
attach(Hitters)

#Fitting subset regression on all the data
full.fit = regsubsets(Salary~., Hitters)
full.fit.summary = summary(full.fit) #getting up to 8 variables (nvmax can change that). * indicates selected variables on each model size.

names(full.fit.summary)
full.fit.summary$rsq #R2 for the best model in each size
full.fit.summary$adjr2
full.fit.summary$bic #BIC - smaller is better

#finding the best model:
par(mfrow = c(2,2))
plot(full.fit.summary$cp, xlab = "Number of Variables", ylab = "CP", type = "b")
points(which.min(full.fit.summary$cp), full.fit.summary$cp[which.min(full.fit.summary$cp)], col = "red")
plot(full.fit.summary$adjr2, xlab = "Number of Variables", ylab = "adjr2", type = "b")
points(which.max(full.fit.summary$adjr2), full.fit.summary$adjr2[which.max(full.fit.summary$adjr2)], col = "red")
plot(full.fit.summary$rss, xlab = "Number of Variables", ylab = "rss", type = "b")
points(which.min(full.fit.summary$rss), full.fit.summary$rss[which.min(full.fit.summary$rss)], col = "red")
plot(full.fit.summary$bic, xlab = "Number of Variables", ylab = "bic", type = "b")
points(which.min(full.fit.summary$bic), full.fit.summary$bic[which.min(full.fit.summary$bic)], col = "red")

#Kind off a bad plot, this is not very intuitive but shows the number of vars
par(mfrow = c(2,2)) 
plot(full.fit, scale = "r2")
plot(full.fit, scale = "adjr2")
plot(full.fit, scale = "Cp")
plot(full.fit, scale = "bic")

#best model coefficients (without running LM again)
coef(full.fit, 6)


############################ Training and testing ######################################

train_index = sample(nrow(Hitters), 0.7*nrow(Hitters))
train = Hitters[train_index,]
test = Hitters[-train_index,]

#Best subset regression (max variables)
fit.best = regsubsets(Salary~., data = train, nvmax = 19)
fit.best.summary = summary(fit.best)

#Predictions

#model.matrix creates the X matrix
test.mat = model.matrix(Salary~., data = test)
test.mse = rep(NA, 19)
#for each model in size i, create a vector of the coefficiens and multiply X to get y.
#Y = X * b > {Nx1} = {N x (P+1)} * {(P+1) x 1}
for (i in 1:19){
  coeffs.i = coef(fit.best, id = i)
  pred = test.mat[,names(coeffs.i)] %*% coeffs.i
  test.mse[i] = mean((test$Salary-pred)^2)
}

which.min(test.mse)
coef(fit.best, which.min(test.mse))


par(mfrow = c(2,2))
plot(fit.best.summary$cp, xlab = "Number of Variables", ylab = "CP", type = "b")
points(which.min(fit.best.summary$cp), fit.best.summary$cp[which.min(fit.best.summary$cp)], col = "red")
plot(fit.best.summary$adjr2, xlab = "Number of Variables", ylab = "adjr2", type = "b")
points(which.max(fit.best.summary$adjr2), fit.best.summary$adjr2[which.max(fit.best.summary$adjr2)], col = "red")
plot(fit.best.summary$rss, xlab = "Number of Variables", ylab = "rss", type = "b")
points(which.min(fit.best.summary$rss), fit.best.summary$rss[which.min(fit.best.summary$rss)], col = "red")
plot(fit.best.summary$bic, xlab = "Number of Variables", ylab = "bic", type = "b")
points(which.min(fit.best.summary$bic), fit.best.summary$bic[which.min(fit.best.summary$bic)], col = "red")
