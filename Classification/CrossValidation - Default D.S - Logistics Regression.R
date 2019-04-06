# Cross Validation for the default Dataset
library(ggplot2)
library(ISLR)
library(boot)
D = Default

set.seed(1)

# Plotting the data
ggplot(data = D) + geom_point(mapping = aes(x = balance, y = income, col = default))
boxplot(D$balance ~ D$student)

# Logistic Model Using all Observations
glm.fit = glm(default ~ income + balance, data = D, family = binomial)
glm.fit2 = glm(default ~ income + balance +student, data = D, family = binomial)
glm.fit3 = glm(default ~ student, data = D, family = binomial)

# Looking at the plot below, the data suggests that balance and student status are
# correlated. Therefore, it might be appropriate to offer the following interpretation:
# students tend to have higher balances than nonstudents, so even though a given
# student has a lesser probablity of default than a non student,
# (for a fixed balance) because students tend to carry higher balances overall,
# students tend to have higher, overall default rates.


# Validation Set Approach (dividing training into two sets, 50% in each)
validationSet = function()
{
  # i.
  train = sample(dim(Default)[1], dim(Default)[1]/2)
  # ii.
  glm.fit = glm(default ~ income + balance, data = D, family = binomial, subset = train)
  # iii.
  glm.pred = rep("No", dim(Default)[1]/2)
  glm.probs = predict(glm.fit, D[-train, ], type = "response")
  glm.pred[glm.probs > 0.5] = "Yes"
  # iv.
  return(mean(glm.pred != D[-train, ]$default))
}
validationSet()

##### K Fold Cross Validation ####
set.seed(2)

mycost <- function(r, pi)
{
  weight1 = 1 #cost for getting 1 wrong
  weight0 = 1 #cost for getting 0 wrong
  c1 = (r==1)&(pi<0.5) #logical vector - true if actual 1 but predict 0
  c0 = (r==0)&(pi>=0.5) #logical vector - true if actual 0 but predict 1
  return(mean(weight1*c1+weight0*c0))
}

glm.fit1 = glm(default~balance + student, data = D, family = binomial)

cv.error = rep(0,3)

cv.error[1] = cv.glm(D, glm.fit1)$delta[1]  # leave one out
cv.error[2] = cv.glm(D, glm.fit1, K=5)$delta[1]  # 5 fold cross validation
cv.error[3] = cv.glm(D, glm.fit1, K=10)$delta[1]  # 10 fold cross validation

# Leave One Out manually
count = rep(0, dim(D)[1])
for (i in 1:(dim(D)[1]))
{
  glm.fit = glm(default ~ balance + student, data = D[-i, ], family = binomial)
  is_Defualt = predict.glm(glm.fit, D[i, ], type = "response") > 0.5
  is_true_Defualt = D[i, ]$default == "Yes"
  if (is_Defualt != is_true_Defualt) 
    count[i] = 1
}
sum(count)
sum(count/dim(D)[1])


plot(balance~income)
plot(default,income)
plot(default,balance)
plot(student, balance)

contrasts(student)


smp_size = floor(0.7 * nrow(df))
set.seed(42)
train_index = sample(seq_len(nrow(df)), size = smp_size) # sample from seq of row numbers
train = df[train_index, ]
test = df[-train_index, ]
colnames(df)


logReg = glm(default ~ student , data = train, family = "binomial")
summary(logReg)


logReg2 = glm(default ~ student+income+balance , data = train, family = "binomial")
summary(logReg2)

logReg3 = glm(default ~ income+balance , data = train, family = "binomial")
summary(logReg3)

library(vcd)

Table = table(student, default)
mosaic(Table, shade=FALSE, legend=FALSE, cex.main=.3)

#Student Alone, is dependant on the balance variable. students have higher balance.
#Therefore, a student vs  balance model  showed significance.

#However, a model with balance in addition to student showed that
#students are more likely to default, but this is not because they are students,
#it's because they have a higher balance.

library(lmtest)

lr.test = lrtest(logReg3,logReg2)

LR.Statistic = -2*(lr.test$LogLik[1]-lr.test$LogLik[2])
#OR
LR.Statistic = logReg$null.deviance-logReg$deviance

LR.Pvalue = 1- pchisq(LR.Statistic,1)




# K FOLD CROSS VALIDATION

fold = 2
folds <- cut(seq(1,nrow(df)),breaks=fold,labels=FALSE)

for(i in 1:fold){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df[testIndexes, ]
  trainData <- df[-testIndexes, ]
  model = glm(default ~ student+balance , data = trainData, family = "binomial")
  test.prediction = predict(object = model, newdata = testData, type = 'response')
  predictions = rep("No", length(testData$default))
  predictions[test.prediction>0.5] = "Yes" #Cutoff probability
  #
}
roc(response = testData$default, predictor = test.prediction, auc = T, plot = T)
