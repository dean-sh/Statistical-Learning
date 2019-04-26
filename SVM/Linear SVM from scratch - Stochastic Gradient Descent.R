#SVM from scratch using gradient descent
library(ggplot2)

set.seed(4)
x1=c(rnorm(n = 21,mean = 0),rnorm(n = 21,mean = -1.5))
x2=c(rnorm(n = 21,mean = 1.5),rnorm(n = 21,mean = 0))
y=c(rep(1,21),rep(-1,21))

lin=data.frame(x1, x2 ,y)
lin$y=factor(lin$y)

ggplot(lin)+ geom_point(aes(x=x1,y=x2,color=y)) + theme_bw()

#Shuffle data
df = lin[sample(nrow(lin), nrow(lin)), ]

X = array(df)
X[,3]=-1 #bias
y = c(df$y)
X = mapply(X, FUN=as.numeric)

SVM_SGD = function(X, y){
  #Initialize our SVMs weight vector with zeros
  w = array(rep(0,ncol(X)))
  l.rate = 1 #learning rate
  ephcs = 10
  errors = array(0)
  for (epoch in 1:ephcs){
    err = 0
    i = 1
    for (row in 1:nrow(X)){
      #in case of missclassification, the weight changes accourding to the derivative of the inner product and regularization:
      if (y[i]*(X[i,]%*%w) < 1){
        w = w + l.rate * ((X[i,]*y[i]) + (-2*(1/epoch)*w))
        print("updated w")
        print(w)
        err = 1
      }
      #in case of correct classification, only the regularization derivative changes the weight:
      else{
        w = w + l.rate * (-2*(1/epoch)*w)
        print("updated w")
        print(w)
      }
    }
    i = i + 1
    errors = append(errors, err)
  }
  return(w)
}

w = SVM_SGD(X,y)

x2=c(w[1],w[2],-w[2],w[1])
x3=c(w[1],w[2],w[2],-w[1])

x2x3 = t(cbind(x2,x3))
X. = x2x3[,1]
Y. = x2x3[,2]
U  = x2x3[,3]
V  = x2x3[,4]

install.packages("pracma")
library(pracma)

plot(X, col=y)
quiver(X., Y., U, V, scale = 1)
