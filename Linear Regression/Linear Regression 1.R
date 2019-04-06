sim_b0 = 5
sim_b1 = 3

sim_x = seq(1,100,3)

Noisify <- function(data) {
  
  if (is.vector(data)) {
    #noise <- runif(length(data), -50, 50)
    noise = rnorm(length(data), 0, 15)
    noisified <- data + noise
  } else {
    length <- dim(data)[1] * dim(data)[2]
    noise <- matrix(runif(length, -0.0001, 0.00001), dim(data)[1])
    noisified <- data + noise
  }
  return(noisified)
}
sim_y = sim_b0+ Noisify(sim_x)*sim_b1



#x = c(59,49,75,65,89,70)
#y = c(209,180,195,186,200,204)

x = sim_x
y = sim_y

sum_xy = sum(x*y)

xbar = mean(x)
ybar = mean(y)

S_XY = sum_xy-length(x)*xbar*ybar
sum_x2 = sum(x*x)
S_XX = sum_x2 - length(x)*(xbar^2)
sum_y2 = sum(y*y)
S_YY = sum_y2 - length(y)*(ybar^2)


beta1hat = S_XY/S_XX
beta0hat = ybar - beta1hat*xbar

data = data.frame(sim_x,sim_y)

#ggplot(data, aes(sim_x, sim_y)) + geom_point(color='blue') +
#  geom_abline(slope = beta1hat, intercept = beta0hat, color = "red") + 
#  geom_abline(slope = sim_b1, intercept = sim_b0, color = "purple")


  
plot(x,y, col = c("blue"), pch=16)
abline(a = beta0hat, b = beta1hat, col = c("red"))
abline(a = sim_b0, b = sim_b1, col = c("green"))
sprintf("Y = %f + %f *X ", beta0hat, beta1hat)


SST = S_YY
SSR = (S_XY^2)/S_XX
SSE = (S_YY-(S_XY^2))/S_XX

R2 = SSR/SST
print(round(100*R2,2))

r_xy = sqrt(R2)

MSE = SSE/(length(x)-2)

pred = function(beta0hat,beta1hat,x){
  return(beta0hat+x*beta1hat)
}

#pred(beta0hat,beta1hat,60)
print(sqrt(abs(MSE)))
print(round(100*R2,2))

semech_b1 = beta1hat

#############################################
reg1 = lm(sim_y~sim_x)
coefficients(reg1)

confint(reg1)
summary(reg1)

anova(reg1)
