#Logistic Regression From scratch

df = read.csv("Datasets/auto.csv")


mgp.mean = mean(df$mpg)

df$binaryMpg = rep(0, length(df$mpg))
df$binaryMpg[df$mpg>mgp.mean] = 1

df$binaryMpg = factor(df$binaryMpg)
attach(df)

# The Sigmoid Function
#####################################
# z = w0 + w1x1 + w2x2 + ....
# h = g(x) = 1/(1 + exp(-z))

lin = seq(-5, 5, by = 0.1)
h = 1/(1 + exp(-lin))
plot(x = lin, y = h, type = "l",main = "Sigmoid Function", col = "blue")

# The Likelihood Function
#####################################

#P(Y=y | X=x) = 1 / (1 + exp(y*Theta %*% X))
#We will use gradient descent to update theta values.

update.Theta = function(theta, d, y, x, n, alpha){
  #theta(j) <- theta(j-1) - alpha*a/b
  a = t(replicate(d,y)) * x
  
  theta.x = t(replicate(n, theta)) * t(x)
  theta.x = rowSums(theta.x)

  b = 1 + exp(-y * theta.x)
  
  theta = theta - alpha*rowSums(a/b)
  
  return(theta)
}

Theta.Optimization = function(theta, x, y, threshold, alpha = 0.2, max_iter = 100){
  
  n = ncol(x) #number of examples
  d = length(theta) #number of parameters
  
  initial = TRUE
  number.of.iter = 0
  
  new.Theta = theta
  
  while(number.of.iter <= max_iter){
    
    initial = FALSE
    
    previous.Theta = new.Theta
    new.Theta = update.Theta(new.Theta, d, y, x, n, alpha)
    
    theta.difference = sum(abs(new.Theta - previous.Theta))
    
    if (theta.difference < threshold) break
    
    number.of.iter = number.of.iter + 1
  }
  
  return(new.Theta)  
}

logistic = function(x, theta){
  
  model = sapply(1:ncol(x_arg) , function(index) 1 / (1 + exp(sum(theta_arg * x_arg[,index]))))
  return(model)
  
}

p = ncol(df)-1
theta = rep(0.1, p)

alpha = 0.5
threshold <- 1
max_iter <- 100

library(dplyr)
df = select(df, -c(mpg, name))
X = as/(binaryMpg~., data = df)

finaltheta = Theta.Optimization()


