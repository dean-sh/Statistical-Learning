#Lab: Support Vector Mahines (ISLR)

# 1- Support Vector Classifiers
library(e1071)
set.seed(640)
x = matrix(rnorm(20*2), ncol=2)
y = c(rep(-1,10),rep(1,10))
x[y==1] = x[y==1] +1

# encoding the class as a factor {-1,1}
y = factor(y)
df = data.frame(x,y)
plot(df$X1~df$X2, col = y)

svmfit = svm(y~., data = df, kernel = "linear", cost = 10, scale = TRUE)
plot(svmfit,data = df)

#support vectors
supvec = svmfit$index
x[supvec,]

#trying a smaller cost - will increase the margin size, therfore the number of s.vectors
svmfit = svm(y~., data = df, kernel = "linear", cost = 0.1, scale = TRUE)
plot(svmfit,data = df)
length(svmfit$index)

summary(svmfit)


# tune function - for cross validaion (could also use the caret library)
tune.svm = tune(svm, y~., data = df, kernel = "linear",
                ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.svm)

plot(tune.svm)
best.svm = tune.svm$best.model

#generating a test set from the same population:
set.seed(1)
x.test = matrix(rnorm(20*2), ncol=2)
y.test = c(rep(-1,10),rep(1,10))
x.test[y.test==1] = x.test[y.test==1] +1

# encoding the class as a factor {-1,1}
y.test = factor(y.test)
df.test = data.frame(x.test,y.test)

table(predict(best.svm,df.test),df.test$y.test)


# 2- Support Vector Machine - using kernel tricks
set.seed(1)
x = matrix(rnorm(200*2), ncol=2)

x[1:100,] = x[1:100,] + 2
x[101:150,] = x[101:150,] - 2

y = c(rep(1,150),rep(2,50))
plot(x,col=y)
y = factor(y)
df = data.frame(x,y)

#Train-test splot
train.idx = sample(1:nrow(df),0.7*nrow(df))
train = df[train.idx,]
test = df[-train.idx,]

svmfit = svm(y~., data = train, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit,train)


tune.svm = tune(svm, y~., data = train, kernel = "radial",
                ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100),
                              gamma = c(0.2, 0.5, 1, 2, 3, 4)))
plot(tune.svm)
tune.svm$best.parameters
best.svm = tune.svm$best.model
summary(tune.svm)
#best model = cost 1, gamme 0.2
table(predictions = predict(best.svm, test), True = test$y)
#almost no misclassifications!
#               True
#    predictions  1  2
#              1 40  1
#              2  1 18

