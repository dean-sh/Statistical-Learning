############################################
#     SIMPLE LOGISTIC REGRESSION          
#   Heart Disease Classification by Age
############################################
df = read.csv("chdage.csv")
attach(df)

summary(df)
pairs(df)
plot(age~chd)
plot(chd~agegrp)

df$id = NULL
chd = factor(chd)
##############################################
#   choosing training set and testing set
##############################################

smp_size = floor(0.7 * nrow(df))
set.seed(42)
train_index = sample(seq_len(nrow(df)), size = smp_size) # sample from seq of row numbers
train = df[train_index, ]
test = df[-train_index, ]
colnames(df)


logReg = glm(chd ~ age , data = train, family = binomial)
summary(logReg)

##############################################
#            Test Prediction
##############################################
test.prediction = predict(object = logReg, newdata = test, type = 'response')

predictions = rep("No", length(test$chd))
predictions[test.prediction>0.5] = "Yes" #Cutoff probability

CM =  table(predictions, test$chd) #Confusion Matrix

source("Confusion Matrix.R")

TP = CM[2,2]
FN = CM[2,1]
TN = CM[1,1]
FP = CM[1,2]

Metrics = ConfusionMatrixStatistics(TP, TN, FP, FN) #Statistics - Accuracy, Recall, Precs.

##############################################
#                Plotting
##############################################
library(ggplot2)

plot(test$age, test$chd, pch = 16, xlab = "age", ylab = "CHD")
curve(test.prediction, add=TRUE)

plot(age, chd)

levels(test$chd) = c(0,1)
test$chd = as.numeric(test$chd)


ggplot(test) +
  geom_point(aes(x=test$age, test$chd)) +
  geom_point(aes(x=test$age, y=test.prediction), color="blue") 


summary(logReg)
plot(test$age,test.prediction,  col="blue")

##############################################
#Likelihood ratio test - ?????? ?????????????? ?????????????
##############################################
library(lmtest)

emptymodel = glm(data = train, chd~1, family= binomial)
lr.test = lrtest(emptymodel,logReg)

LR.Statistic = -2*(lr.test$LogLik[1]-lr.test$LogLik[2])
#OR
LR.Statistic = logReg$null.deviance-logReg$deviance

LR.Pvalue = 1- pchisq(LR.Statistic,1)

##############################################
#                   ROC
##############################################

library(pROC)

#ROC over all the data:
logReg2 = glm(chd ~ age , data = df, family = binomial)
test.prediction2 = predict(object = logReg2, type = 'response')

roc(response = df$chd, predictor = test.prediction2, auc = T, plot = T)


#ROC over test data only:
logReg1 = glm(chd ~ age , data = train, family = binomial)
test.prediction1 = predict(object = logReg1, newdata = test, type = 'response')

roc(response = test$chd, predictor = test.prediction1, auc = T, plot = T)



##############################################
#   Simple lib for 
##############################################

library(rattle)

install.packages("RGtk2")

