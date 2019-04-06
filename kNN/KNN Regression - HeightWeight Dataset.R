library(class)
library(readxl)
HW = read_excel("D:/Technion - Data Science/Statistical Learning/HeightWeight.xlsx")

data = HW # backup

temp = names(data) %in% c("Height", "ID")
data = data[!temp]


#choosing training set and testing set
# set.seed(123) 
test = 1:15
train.x = data[test,]
test.x = data[-test,]

# removing the dependent variable from train and test sets
depvar = names(train.x) %in% c("Weight")
train.x = train.x[!depvar]
test.x = test.x[!depvar]

# creating vectors of dependent variable for both sets
train.y = data$Weight[test]
test.y = data$Weight[-test]

# knn
pred_001 = FNN::knn.reg(train = train.x, test = test.x, y = train.y, k = 1)
diff1 = test.y-pred_001$pred
pred_003 = FNN::knn.reg(train = train.x, test = test.x, y = train.y, k = 3)
diff3 = test.y-pred_003$pred
pred_005 = FNN::knn.reg(train = train.x, test = test.x, y = train.y, k = 5)
diff5 = test.y-pred_005$pred
pred_015 = FNN::knn.reg(train = train.x, test = test.x, y = train.y, k = 15)
diff15 = test.y-pred_015$pred

# overfitting
plot(train.y ~ train.x$Age, cex = .8, col = "dodgerblue", main = "k = 1")
lines(test.x$Age, pred_001$pred, col = "darkorange", lwd = 0.25)

plot(train.y ~ train.x$Age, cex = .8, col = "dodgerblue", main = "k = 3")
lines(test.x$Age, pred_003$pred, col = "darkorange", lwd = 0.25)

plot(train.y ~ train.x$Age, cex = .8, col = "dodgerblue", main = "k = 5")
lines(test.x$Age, pred_005$pred, col = "darkorange", lwd = 0.25)

# underfitting
plot(train.y ~ train.x$Age, cex = .8, col = "dodgerblue", main = "k = 15")
lines(test.x$Age, pred_015$pred, col = "darkorange", lwd = 0.25)