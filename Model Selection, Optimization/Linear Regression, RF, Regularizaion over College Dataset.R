#Model Selection

library(ISLR)
library(ggplot2)

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

MSE.LR = mean((lr.predict-test$Apps)^2)

par(mfrow = c(1,1))
plot(lr.predict, test$Apps)
abline(0,1)

# 2 - Ridge Regularization,  Linear Regression - full model
# Choosing lambda using cross-validation


