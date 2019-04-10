library(MASS)
Boston = Boston

Boston.cor = cor(Boston)

library(corrplot)
corrplot(Boston.cor)
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = Boston.cor, col = palette, symm = TRUE)

plot(Boston$medv)

#choosing training set and testing set
smp_size = floor(0.7 * nrow(Boston))
set.seed(123)
train_index = sample(seq_len(nrow(Boston)), size = smp_size) # sample from seq of row numbers
train.x = Boston[train_index, ]
test.x = Boston[-train_index, ]
colnames(Boston)

fit = lm(medv ~ crim + zn + indus + dis + ptratio + rm + age + nox + rad + tax + black, data=Boston)
summary(fit)
plot(fit)

fit = lm(medv ~ crim + dis + ptratio + rm  + nox + black, data=Boston)
summary(fit)
plot(fit)


b0 = fit$coefficients[1]
b1_crim = fit$coefficients[2]
b1_dis = fit$coefficients[3]
b1_ptratio = fit$coefficients[4]
b1_rm = fit$coefficients[5]
b1_nox = fit$coefficients[6]
b1_black = fit$coefficients[7]

test.x$medv_pred = b0 + b1_crim * test.x$crim + b1_dis * test.x$dis + b1_ptratio * test.x$ptratio + b1_rm * test.x$rm + b1_nox * test.x$nox + b1_black*test.x$black
test.x$residual = test.x$medv - test.x$medv_pred
test.x$residual2 = test.x$residual^2

test.mse = mean(test.x$residual2)
test.mse

influence(fit)
vcov(fit)
fitted(fit)
confint(fit, level=0.95)


#KNN Testing

train.x = Boston[train_index, ]
test.x = Boston[-train_index, ]

train.y = train.x$medv
test.y = test.x$medv

depvar = names(train.x) %in% c("medv")
train.x = train.x[!depvar]
test.x = test.x[!depvar]

library(FNN)

pred_001 = FNN::knn.reg(train = train.x, test = test.x, y = train.y, k = 1)
diff1 = test.y-pred_001$pred
pred_003 = FNN::knn.reg(train = train.x, test = test.x, y = train.y, k = 3)
diff3 = test.y-pred_003$pred
pred_005 = FNN::knn.reg(train = train.x, test = test.x, y = train.y, k = 5)
diff5 = test.y-pred_005$pred
pred_015 = FNN::knn.reg(train = train.x, test = test.x, y = train.y, k = 15)
diff15 = test.y-pred_015$pred

