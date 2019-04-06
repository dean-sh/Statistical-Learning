#CarSeats Linear Regression

carseats = read.csv("carseats.csv")

#Factoring
carseats$ShelveLoc = factor(carseats$ShelveLoc)
carseats$Urban = factor(carseats$Urban)
carseats$US = factor(carseats$US)
attach(carseats)

#choosing training set and testing set
smp_size = floor(0.7 * nrow(carseats))
set.seed(123)
train_index = sample(seq_len(nrow(carseats)), size = smp_size) # sample from seq of row numbers
train.x = carseats[train_index, ]
test.x = carseats[-train_index, ]
colnames(carseats)

#Checking if Sales are NORMAL
plot(Sales)
boxplot(Sales)
qqnorm(Sales);qqline(Sales, col = 2)
shapiro.test(Sales)
hist(Sales, 50, col="black")

#DUMMY variables
contrasts(ShelveLoc)

#Regression with 1 variable each time
regCompPrice = lm(Sales~CompPrice, data = train.x)
summary(regCompPrice) #pval 0.333

regIncome = lm(Sales~Income, data = train.x)
summary(regIncome) #pval 0.0125

regADS = lm(Sales~Advertising, data = train.x)
summary(regADS) #pval 8.41e-06

regShelv = lm(Sales~ShelveLoc, data = train.x)
summary(regShelv) #pval 2e-16, 8.41e-06
plot(regShelv)

regAge = lm(Sales~Age, data = train.x)
summary(regAge) #pval 5.45e-07

regEducation = lm(Sales~Education, data = train.x)
summary(regEducation) #pval 0.59

regUrban = lm(Sales~Urban, data = train.x)
summary(regUrban) #pval 0.86  

regUS = lm(Sales~US, data = train.x)
summary(regUS) #pval  0.00578

#Multiple Regression with the best variables
MultReg = lm(Sales~CompPrice+Advertising+ShelveLoc+US+Age+ShelveLoc+Income, data = train.x)
summary(MultReg)

MultReg2 = lm(Sales~Education+Urban+Advertising+ShelveLoc+US+Age+ShelveLoc+Income, data = train.x)
summary(MultReg2)

MultReg3 = lm(Sales~ Advertising+ShelveLoc+Age+Income + Price, data = train.x)
summary(MultReg3)

#Adding interactions:
InteractionReg1 = lm(Sales~Advertising + ShelveLoc + Age + Income + Price, data = train.x)
summary(InteractionReg1)
plot(MultReg3)

predict.MultReg3 = predict(object = MultReg3, newdata = test.x, interval="confidence", level=.95)

MSE.MultReg3 = mean((predict.MultReg3-test.x$Sales)^2)
?predict


ggplot(test.x, aes(x=QUET, y=SBP))+
  geom_point()+
  geom_smooth(method=lm, se=TRUE)


plot(MultReg3)


require(ggplot2)
require(reshape2)
meltData = melt(train.x, id.vars='Sales')
ggplot(meltData) +
  geom_jitter(aes(value, Sales, colour=variable)) + geom_smooth(aes(value, Sales,colour=variable), method='lm', se=FALSE) +
  facet_wrap(~variable, scales="free_x") +
  labs(x = "Percentage cover (%)", y = "Number of individuals (N)")

ggplot(train.x, aes(x = CompPrice, y = Sales, size = 2)) +
  geom_point() + geom_smooth(method = "lm", fill=NA, size = 1)
  
  
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
ggplotRegression(MultReg3)

  