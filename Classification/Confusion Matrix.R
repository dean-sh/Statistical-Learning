ConfusionMatrixStatistics = function(TP, TN, FP, FN){
  P = TP + FN
  N = TN + FP
  Sensitivity.recall = TP/P
  Specifity = TN/N
  
  Precision = TP/(TP+FP)
  Negative.Pred.Value = TN/(TN+FN)
  
  MissRate.FalsePosRate  = FN/P
  FallOut.FalseNegRate = FP/N
  
  FalseDiscoveryRate = FP/(FP+TP)
  
  Accuracy = (TP + TN)/(P+N)
  
  F1.Score = (2*TP)/(2*TP + FP + FN)
  
  MatthewsCorrCoef = (TP*TN - FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  
  ExpectedAccuracy = ((TP + FN)*(TP+FP)/(P+N) +  (FP+TN)*(TN+FN)/(P+N))/(P+N)
  
  Cohen.Kappa = (Accuracy - ExpectedAccuracy)/(1 - ExpectedAccuracy)
  
  return(t(data.frame(Sensitivity.recall, Specifity, Precision, Negative.Pred.Value, MissRate.FalsePosRate, FallOut.FalseNegRate, FalseDiscoveryRate, Accuracy, F1.Score, MatthewsCorrCoef, Cohen.Kappa)))
  
}


TP = 100
FP = 10
TN = 50
FN = 5

AccuracyStatistics = t(ConfusionMatrixStatistics(TP, TN, FP, FN))
