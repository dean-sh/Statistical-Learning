#Create sample data - Maximal Margin Classifier
set.seed(4)
x1=c(rnorm(n = 21,mean = 1.5),rnorm(n = 21,mean = -1.5))
x2=c(rnorm(n = 21,mean = 1.5),rnorm(n = 21,mean = -1.5))
y=c(rep(1,21),rep(-1,21))

# x = matrix(x1,x2)
lin=data.frame(x1, x2 ,y)
lin$y=factor(lin$y)

logit = glm(y~ ., data = lin, family = "binomial", maxit = 50)

slope <- coef(logit)[2]/(-coef(logit)[3])
intercept <- coef(logit)[1]/(-coef(logit)[3]) 

library(ggplot2)
ggplot(lin)+ geom_point(aes(x=x1,y=x2,color=y)) + theme_bw() + geom_abline(slope = slope, intercept = intercept)
