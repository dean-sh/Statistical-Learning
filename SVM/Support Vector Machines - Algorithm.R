#Dataset:
#Assuming the dataset is in a dataframe "df".
library(ISLR)

df = Default
attach(df)

n = nrow(df)
df$default = as.numeric(df$default)
df$default = ifelse(df$default==2,1,-1)
df$default = factor(df$default)

df$student = as.numeric(df$student)
df$student = ifelse(df$student==2,1,-1)
df$student = factor(df$student)

train = df[1:8000,]
test = df[8001:10000,]

C = 0.5 #SVM Cost
p = ncol(df)-1 #Number of parameters/features in the dataset

#Quadratic Programming Solution
library(quadprog)

svm.fit = function(X, y, C=NULL) {
  X = cbind(1,M)
  y = as.vector(sapply(df[,1], as.numeric))
  n.samples = nrow(X)
  n.features = ncol(X)
  K = matrix(rep(0, n.samples*n.samples), nrow=n.samples)
  for (i in 1:n.samples){
    for (j in 1:n.samples){
      K[i,j] = X[i,] %*% X[j,] }}
  Dmat = outer(y,y) * K
  Dmat = as.matrix(Matrix::nearPD(Dmat)$mat)
  dvec = rep(1, n.samples)
  Amat = rbind(y, diag(n.samples), -1*diag(n.samples))
  bvec = c(0, rep(0, n.samples), rep(-C, n.samples))
  res = solve.QP(Dmat,dvec,t(Amat),bvec=bvec, meq=1)
  a = res$solution 
  bomega = apply(a*y*X,2,sum)
  return(bomega)
}

df = df[1:1000,]
M = as.matrix(sapply(df[,1:p+1], as.numeric))
# center = function(z){
#   z = as.numeric(z)
#   (z-mean(z))/sd(z)
# }
# for(i in 1:p){
#   M[,i] = center(M[,i])
# }

bomega = svm.fit(cbind(1,M),df[1],C=.5)

Mtest = as.matrix(sapply(test[,1:p+1], as.numeric))

y_predict = 2*((cbind(1,Mtest)%*%bomega)>0)-1

sum(y_predict)
table(test$default, y_predict)
