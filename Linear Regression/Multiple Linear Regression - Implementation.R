#### Matrices #####
y = c(29,1,33,10,53)
x1 = c(2,3,3,2,4)
x2 = c(6,-2,5,1,10)
x = rep(1,5)
x = c(x,x1,x2)
X = matrix(x,nrow = 5)
Xt = t(X)
XtX = Xt %*% X
Xtx1 = solve(XtX)

XtY = Xt %*% y

beta = Xtx1 %*% XtY


### LM ###
reg1 = lm(y ~ x1 + x2)
confint(reg1)
summary(reg1)
anova(reg1)
d = data.frame(y,x1,x2)
pairs(d)
