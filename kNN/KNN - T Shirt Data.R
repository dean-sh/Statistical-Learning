# In this example we have 18 data points of heights and weights of people
# and their shirt size. We get a new observation with height 161 and weight 61
# We create a KNN classification model "by hand"

library(readxl)
library(ggplot2)
Shirts = read_excel("C:/Users/user/Desktop/Statistical Learning/T Shirt.xlsx")

# Standatization to maintain the same scale for both predictors
Shirts$ZHeight = scale(Shirts$Height)
Shirts$ZWeight = scale(Shirts$Weight)

# When independent variables in training data are measured in different units,
# it is important to standardize variables before calculating distance.
# Otherwise one variabe will influence more than another.

ZH = (161-mean(Shirts$Height))/sd(Shirts$Height)
ZW = (61-mean(Shirts$Weight))/sd(Shirts$Weight)

Shirts$Distance = sqrt((Shirts$ZHeight-ZH)^2+(Shirts$ZWeight-ZW)^2)

Shirts = Shirts[order(Shirts$Distance),] 

k = 5
M = 0
L = 0
for (i in 1:k)
{
    if (Shirts$ShirtSize[i] == 'M')
    {
      M = M + 1
    }
   else
   {
      L = L +1
    }
}

ggplot(Shirts, aes(x=ZHeight, y=ZWeight, shape=ShirtSize, color=ShirtSize, size = 2)) + geom_point()

new = c(161,61,"X",ZH,ZW,0)
Shirts2 = rbind(Shirts,new)

Shirts2$ZHeight = as.numeric(Shirts2$ZHeight)
Shirts2$ZWeight = as.numeric(Shirts2$ZWeight)
Shirts2$Distance = as.numeric(Shirts2$Distance)

Shirts2$ZHeight = round(Shirts2$ZHeight,2)
Shirts2$ZWeight = round(Shirts2$ZWeight,2)
Shirts2$Distance = round(Shirts2$Distance,2)

ggplot(Shirts2, aes(x=ZHeight, y=ZWeight, shape=ShirtSize, color=ShirtSize, size = 2)) +
  geom_point() + geom_text(aes(label=Distance),hjust=0.5, vjust=1.5)

######################################################################
library(caret)
library(class)
library(e1071)
ShirtsE = read_excel("C:/Users/user/Desktop/Statistical Learning/T Shirt Extended.xlsx")

data = ShirtsE # backup


data$ShirtSize = factor(data$ShirtSize) #factor of y

# normalizing numeric variables
num.vars = sapply(data, is.numeric)
data[num.vars] = lapply(data[num.vars], scale)
summary(data)

#choosing training set and testing set
set.seed(123) 
test = 1:18
train.x = data[test,]
test.x = data[-test,]

# removing the dependent variable from train and test sets
depvar = names(train.x) %in% c("ShirtSize")
train.x = train.x[!depvar]
test.x = test.x[!depvar]

# creating vectors of dependent variable for both sets
train.y = data$ShirtSize[test]
test.y = data$ShirtSize[-test]

# knn
library(class)
knn.1 =  knn(train.x, test.x, train.y, k=1)
knn.3 =  knn(train.x, test.x, train.y, k=3)
knn.5 =  knn(train.x, test.x, train.y, k=5)

# Create Confusion matrix
table(knn.1 ,test.y)
table(knn.3 ,test.y)
table

# proportion of correct classification for k = 1, 3, 5
100 * sum(test.y == knn.1)/length(test.y)  # For knn = 1
100 * sum(test.y == knn.3)/length(test.y)  # For knn = 1
100 * sum(test.y == knn.5)/length(test.y)  # For knn = 1

kvec = seq(1, 39, by=2)
probs = c()
for (k in kvec){
  prob = 100 * sum(test.y == knn(train.x, test.x, train.y, k))/length(test.y)
  append(probs, prob)
}