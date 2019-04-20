#Cancer Risk Classification
library(mice)

risk = read.csv("risk_factors_cervical_cancer.csv")
attach(risk)
risk[risk=="?"] = NA

#number of NA's in each feature
colSums(is.na(risk))

#The dataset is not set-up correctly (all is integers)
for (row in 1:ncol(diluted_risk)){
  cat(class(row), names(risk)[row],"\n")
}
names(risk)
names <- c('Smokes' ,'IUD', 'Hormonal.Contraceptives', "STDs",
           'Dx.Cancer', 'Dx.CIN', 'Dx', 'Dx.HPV', 'Hinselmann',
           'Schiller', 'Citology', 'Biopsy')
risk[,names] <- lapply(risk[,names] , factor)

library(dplyr)
features = c('Age','Num.of.pregnancies','First.sexual.intercourse',
             'Smokes' ,'Smokes..packs.year.','Smokes..years.',
             'Hormonal.Contraceptives', 'Hormonal.Contraceptives..years.',
             'IUD', 'IUD..years.', 'STDs', 'STDs..number.',
             'STDs..Number.of.diagnosis', 'STDs..Time.since.first.diagnosis',
             'STDs..Time.since.last.diagnosis', 'Dx',
             'Dx.Cancer', 'Dx.CIN', 'Dx.HPV', 'Hinselmann',
             'Schiller', 'Citology', 'Biopsy')
diluted_risk = select(risk, features)

imputed.data = mice(diluted_risk, m =1, maxit = 10, method = "pmm", seed=42)

for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

completeData <- complete(imputed.data,1)

colSums(is.na(completeData))
#No NA's!

library(e1071)

train.index = sample(1:nrow(completeData), 0.7*nrow(completeData))
train = completeData[train.index,]
test = completeData[-train.index,]

svmfit= svm(Biopsy~., data = train, kernel = "linear", cost = 10, scale = TRUE)
svm.rbf= svm(Biopsy~., data = train, kernel = "radial", cost = 10, scale = TRUE)

y_pred = predict(svmfit, newdata = test)
y_pred = predict(svm.rbf, newdata = test)

table(pred = y_pred, data = test$Biopsy)

#cannot simply ploy multi-variable SVM...
plot(svmfit, data = train) #Error

#Try PCA ?
#plot only important  features
#cross validation on kernal

acc = c()
for (c in seq(0.1,20,0.5)){
  svmfit= svm(Biopsy~., data = train, kernel = "radial", cost = c, scale = TRUE)
  y_pred = predict(svmfit, newdata = test)
  CM = table(pred = y_pred, data = test$Biopsy)
  TP = CM[2,2]
  FN = CM[2,1]
  TN = CM[1,1]
  FP = CM[1,2]
  P = TP + FN
  N = TN + FP
  Accuracy = (TP + TN)/(P+N)
  F1.Score = (2*TP)/(2*TP + FP + FN)
  acc = append(acc,Accuracy)
}
plot(acc)

bestfit = svm(Biopsy~., data = train, kernel = "radial", cost = 14, scale = TRUE)
y_pred = predict(bestfit, newdata = test)
CM = table(pred = y_pred, data = test$Biopsy)
