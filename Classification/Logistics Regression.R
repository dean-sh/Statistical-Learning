#            SIMPLE LOGISTIC REGRESSION             #

#Default Variable - did the person returned it's loan?
#what is the probability that a person will return it's loan?s
df = read.csv("Default.csv")
attach(df)

D.test =  df[1:1000,]
D.train = df[1001:10000,]

default = factor(default)
plot(balance~income)

logReg = glm(default ~ balance , data = D.train, family = "binomial")
#summary(logReg)
#confint(logReg)
#exp(coef(logReg)) #Odds Ratio

#glm.prediction = predict(object = logReg, newdata = D.test, type = 'response')

#for one variable:
glmPredict = function(x){
  P = (exp(as.numeric(coef(logReg)[1])+as.numeric(coef(logReg)[2]*x)))/(1+exp(as.numeric(coef(logReg)[1])+as.numeric(coef(logReg)[2]*x)))
  return(P)
}



#Multiple GLM
multiple.glm = glm(default ~ balance + student + income , data = D.train, family = binomial)
summary(multiple.glm)


#Test Prediction
test.prediction = predict(object = multiple.glm, newdata = D.test, type = 'response')

predictions = rep("No", length(D.test$default))
predictions[test.prediction>0.5] = "Yes" #Cutoff probability

table(predictions, D.test$default) #Confusion Matrix
