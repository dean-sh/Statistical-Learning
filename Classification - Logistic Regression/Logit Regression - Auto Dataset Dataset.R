####################################################
#           Auto Dataset Classification            #
####################################################


df = read.csv("auto.csv")


mgp.mean = mean(df$mpg)

df$binaryMpg = rep(0, length(df$mpg))
df$binaryMpg[df$mpg>mgp.mean] = 1

df$binaryMpg = factor(df$binaryMpg)
attach(df)

pairs(df)

plot(binaryMpg~weight)

############################################
#           Descriptive Stats              #
############################################

source("Descriptive Stats.R")
DescStats(mpg)
DescStats(displacement)


############################################
#          Correlation Matrix              #
############################################

library(corrplot)

names(df)
cordf = data.frame(mpg , displacement, horsepower, weight, acceleration, year)
cor(cordf)

d = read.table(text = cordf, header = T)

library(ggcorrplot)
ggcorrplot(df)
p.mat <- cor_pmat(cordf)
p.mat

cor.test(binaryMpg, horsepower, method = "spearman")

############################################
#             GGPLOT BoxPlot               #
############################################
library(ggplot2)
library(reshape2)


names(Filter(is.factor, df))
df.m = df %>% select(-name)

 
df.m <- melt(df.m, id.var = "binaryMpg")
df.c <- melt(df.c, id.var = "binaryMpg")


ggplot(df.m, aes(x=binaryMpg, y=value)) + 
  geom_boxplot(aes(fill=binaryMpg))+
  stat_summary(fun.y = "mean", geom = "point", shape= 23, size= 3, fill= "red") +
  facet_wrap(~variable, scales="free")

############################################
#             GGPLOT Density               #
############################################

ggplot(df.m,aes(x=value, fill=binaryMpg)) + 
  geom_density(alpha = 0.5)+
  facet_wrap(~variable, scales="free")


############################################
#             Train-Test Split             #
############################################
df = df %>% select(-name, -mpg)

df.c = df
df.c$cylinders = factor(df.c$cylinders)
df.c$year = factor(df.c$year)
df.c$origin = factor(df.c$origin)

smp_size = floor(0.7 * nrow(df))
set.seed(42)
train_index = sample(seq_len(nrow(df)), size = smp_size) # sample from seq of row numbers
train = df[train_index, ]
test = df[-train_index, ]
train.c = df.c[train_index, ]
test.c = df.c[-train_index, ]


colnames(df)


############################################
#           Logistic Regression            #
############################################


lgr = glm(binaryMpg~displacement+weight+horsepower , data = train, family = binomial)
lgr.c = glm(binaryMpg~displacement+weight+horsepower+cylinders , data = train.c, family = binomial)

summary(lgr.c)

############################################
#               Prediction                 #
############################################

test.prediction = predict(object = lgr, newdata = test, type = 'response')
test.prediction.c = predict(object = lgr.c, newdata = test.c, type = 'response')

contrasts(df$binaryMpg)

cutoffs = seq(0.4,0.6, 0.01)
for (cutoff in cutoffs){
  predictions = rep("Down", length(test$Direction))
  predictions[test.prediction>cutoff] = "Up" #Cutoff probability
  print(cutoff)
  CM = table(predictions, test$Direction)
  
  print((TP + TN)/(P+N))
}

cutoff=0.5
predictions = rep(0, length(test$binaryMpg))
predictions[test.prediction>cutoff] = 1 #Cutoff probability
CM = table(predictions, test$binaryMpg)

predictions.c = rep(0, length(test.c$binaryMpg))
predictions.c[test.prediction.c>cutoff] = 1 #Cutoff probability
CM.c = table(predictions.c, test.c$binaryMpg)


source("Confusion Matrix.R")
TP = CM[2,2]
FN = CM[2,1]
TN = CM[1,1]
FP = CM[1,2]

TP.c = CM.c[2,2]
FN.c = CM.c[2,1]
TN.c = CM.c[1,1]
FP.c = CM.c[1,2]

Metrics = ConfusionMatrixStatistics(TP, TN, FP, FN) #Statistics - Accuracy, Recall, Precs.
Metrics.c = ConfusionMatrixStatistics(TP.c, TN.c, FP.c, FN.c)
levels(test$Direction) = c(0,1)

plot(test$Lag5, test$Direction, pch = 16, xlab = "Lag5", ylab = "Direction")
curve(test.prediction)

plot(lgr)


ggplot(test) +
  geom_point(aes(x=test$Lag5, test$Direction)) +
  geom_point(aes(x=test$Lag5, y=test.prediction), color="blue") 
plot(test$Lag5,test.prediction,  col="blue")


############################################
#        Likelihood ratio test             #
############################################

library(lmtest)

emptymodel = glm(data = train, binaryMpg~1, family= binomial)
lr.test = lrtest(emptymodel,lgr)

LR.Statistic = -2*(lr.test$LogLik[1]-lr.test$LogLik[2])
#OR
LR.Statistic = lgr$null.deviance-lgr$deviance

LR.Pvalue = 1- pchisq(LR.Statistic,1)


############################################
#                   ROC                    #
############################################

library(pROC)
#ROC over all the data:
logReg2 = glm(Direction ~Volume + Lag1 + Lag2 + Lag3 + Lag4 + Lag5 , data = df, family = binomial)
test.prediction2 = predict(object = logReg2, type = 'response')

roc(response = df$Direction, predictor = test.prediction2, auc = T, plot = T)


#ROC over test data only:
roc(response = test.c$binaryMpg, predictor = test.prediction.c, auc = T, plot = T)


############################################
#               K   N   N                  #
############################################

library(class)

depvars =  names(train) %in% c("names", "mpg", "origin")
train.x = train[!depvars]
test.x = test[!depvars]

train.y = train$binaryMpg
test.y = test$binaryMpg

train.x = scale(train.x)
test.x = scale(test.x)


kvec = seq(1, 250, by=1)
probs = c()
for (k in kvec){
  prob = 100 * sum(test.y == knn(train.x, test.x, train.y, k))/length(test.y)
  probs = append(probs, prob)
}

kvec = data.frame(kvec,probs)

plot(kvec)

best.k = kvec[which.max(kvec$probs),]

############################################
#                 Rattle                   #
############################################

library(rattle)

rattle()

