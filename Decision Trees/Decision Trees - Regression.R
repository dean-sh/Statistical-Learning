############################################################################
# Decision Tree Algorithm          

# 1. binary splitting for creating a big tree over the train tracing data
# 2. cost complexity pruning
# 3. using kfolds CV to choose alpha 
# 4. prunning accourding to the chosen alpha
# 5. testing the tree

############################################################################

library(ISLR)
library(ggplot2)
library(tree)

H = Hitters

ggplot(data = H) + 
  geom_point(mapping = aes(x = Years, y = Hits, col = Salary), size = 2) +
  scale_color_gradientn(colours = rainbow(10))

H = na.omit(H)

ggplot(data = H) + 
  geom_point(mapping = aes(x = Years, y = Hits, col = Salary), size = 2) +
  scale_color_gradientn(colours = rainbow(10))

############################################################################
#                             Tree Fit                                     #
############################################################################

treefit = tree(Salary~Years + Hits, data = H)
summary(treefit)

plot(treefit)
text(treefit)

############################################################################
#                             GG plot fit                                  #
############################################################################


gg.partition.tree <- function (tree, label = "yval", ordvars, ...) 
{
  ptXlines <- function(x, v, xrange, xcoord = NULL, ycoord = NULL, 
                       tvar, i = 1L) {
    if (v[i] == "<leaf>") {
      y1 <- (xrange[1L] + xrange[3L])/2
      y2 <- (xrange[2L] + xrange[4L])/2
      return(list(xcoord = xcoord, ycoord = c(ycoord, y1, 
                                              y2), i = i))
    }
    if (v[i] == tvar[1L]) {
      xcoord <- c(xcoord, x[i], xrange[2L], x[i], xrange[4L])
      xr <- xrange
      xr[3L] <- x[i]
      ll2 <- Recall(x, v, xr, xcoord, ycoord, tvar, i + 
                      1L)
      xr <- xrange
      xr[1L] <- x[i]
      return(Recall(x, v, xr, ll2$xcoord, ll2$ycoord, tvar, 
                    ll2$i + 1L))
    }
    else if (v[i] == tvar[2L]) {
      xcoord <- c(xcoord, xrange[1L], x[i], xrange[3L], 
                  x[i])
      xr <- xrange
      xr[4L] <- x[i]
      ll2 <- Recall(x, v, xr, xcoord, ycoord, tvar, i + 
                      1L)
      xr <- xrange
      xr[2L] <- x[i]
      return(Recall(x, v, xr, ll2$xcoord, ll2$ycoord, tvar, 
                    ll2$i + 1L))
    }
    else stop("wrong variable numbers in tree.")
  }
  if (inherits(tree, "singlenode")) 
    stop("cannot plot singlenode tree")
  if (!inherits(tree, "tree")) 
    stop("not legitimate tree")
  frame <- tree$frame
  leaves <- frame$var == "<leaf>"
  var <- unique(as.character(frame$var[!leaves]))
  if (length(var) > 2L || length(var) < 1L) 
    stop("tree can only have one or two predictors")
  nlevels <- sapply(attr(tree, "xlevels"), length)
  if (any(nlevels[var] > 0L)) 
    stop("tree can only have continuous predictors")
  x <- rep(NA, length(leaves))
  x[!leaves] <- as.double(substring(frame$splits[!leaves, "cutleft"], 
                                    2L, 100L))
  m <- model.frame(tree)
  if (length(var) == 1L) {
    x <- sort(c(range(m[[var]]), x[!leaves]))
    if (is.null(attr(tree, "ylevels"))) 
      y <- frame$yval[leaves]
    else y <- frame$yprob[, 1L]
    y <- c(y, y[length(y)])
    if (add) 
      lines(x, y, type = "s", ...)
    else {
      a <- attributes(attr(m, "terms"))
      yvar <- as.character(a$variables[1 + a$response])
      xo <- m[[yvar]]
      if (is.factor(xo)) 
        ylim <- c(0, 1)
      else ylim <- range(xo)
      plot(x, y, ylab = yvar, xlab = var, type = "s", ylim = ylim, 
           xaxs = "i", ...)
    }
    invisible(list(x = x, y = y))
  }
  else {
    if (!missing(ordvars)) {
      ind <- match(var, ordvars)
      if (any(is.na(ind))) 
        stop("unmatched names in vars")
      var <- ordvars[sort(ind)]
    }
    lab <- frame$yval[leaves]
    if (is.null(frame$yprob)) 
      lab <- format(signif(lab, 3L))
    else if (match(label, attr(tree, "ylevels"), nomatch = 0L)) 
      lab <- format(signif(frame$yprob[leaves, label], 
                           3L))
    rx <- range(m[[var[1L]]])
    rx <- rx + c(-0.025, 0.025) * diff(rx)
    rz <- range(m[[var[2L]]])
    rz <- rz + c(-0.025, 0.025) * diff(rz)
    xrange <- c(rx, rz)[c(1, 3, 2, 4)]
    xcoord <- NULL
    ycoord <- NULL
    xy <- ptXlines(x, frame$var, xrange, xcoord, ycoord, 
                   var)
    xx <- matrix(xy$xcoord, nrow = 4L)
    yy <- matrix(xy$ycoord, nrow = 2L)
    return(
      list(
        annotate(geom="segment", x=xx[1L, ], y=xx[2L, ], xend=xx[3L, ], yend=xx[4L, ]),
        annotate(geom="text", x=yy[1L, ], y=yy[2L, ], label=as.character(lab), ...)
      )
    )
  }
}


############################################################################
#                             Prunning                                     #
############################################################################

#prunning with best = 5

prune1 = prune.tree(treefit, best = 5)
plot(prune1)
text(prune1)


############################################################################
#                         Train-Test Split                                 #
############################################################################

fold = floor(runif(nrow(H),1,11))
H$fold = fold

test.set = H[H$fold ==1,]
train.set = H[H$fold!=1,]

my.tree = tree(log(Salary)~Years + Hits, data = train.set, mindev = 0.00001)
#the within-node deviance must be at least this times part of the mindev
#the tree will continue to develop until this is the minimum of the error.
#you can also declare the min-size of points inside each separation.
set.seed(42)
train.tree = prune.tree(my.tree, best = 5)

plot(train.tree)
text(train.tree)

summary(train.tree)

############################################################################
#                             CV on trees                                  #
############################################################################
my.tree = tree(log(Salary)~Years + Hits, data = train.set, mindev = 0.001)
my.tree.cv = cv.tree(my.tree)

plot(my.tree.cv, type = 'b')

cv.tree(my.tree, best = 5)

############################################################################
#                      Prediction and MSE                                  #
############################################################################


yhat = predict(my.tree, newdata = test.set)
hitters.test = log(test.set$Salary)
plot(yhat, hitters.test)
abline(0,1)

exp(mean((yhat-hitters.test)^2))
