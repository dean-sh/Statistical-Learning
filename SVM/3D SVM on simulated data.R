library(e1071)
library(rgl)
library(misc3d)

n    = 200
nnew = 50


# Simulate some data
set.seed(1)
group = sample(2, n, replace=T)
dat   = data.frame(group=factor(group), matrix(rnorm(n*3, rep(group, each=3)), ncol=3, byrow=T))

plotsvm = function(kernal, c){
  # Fit SVM
  fit = svm(group ~ ., kernel = kernal, cost = c, data=dat)

  # Plot original data
  plot3d(dat[,-1], col=dat$group, xlab = c)
  
  # Get decision values for a new data grid
  newdat.list = lapply(dat[,-1], function(x) seq(min(x), max(x), len=nnew))
  newdat      = expand.grid(newdat.list)
  newdat.pred = predict(fit, newdata=newdat, decision.values=T)
  newdat.dv   = attr(newdat.pred, 'decision.values')
  newdat.dv   = array(newdat.dv, dim=rep(nnew, 3))
  
  # Fit/plot an isosurface to the decision boundary
  contour3d(newdat.dv, level=0, x=newdat.list$X1, y=newdat.list$X2, z=newdat.list$X3, add=T, color = "cyan")
}

mfrow3d(mfrow3d(nr = 1, nc = 2, sharedMouse = TRUE))
plotsvm("polynomial", c=4)
plotsvm("radial", c=4)
