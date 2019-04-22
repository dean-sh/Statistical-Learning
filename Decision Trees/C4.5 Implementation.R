# Decision Tree Algorithm From Scratch
library(dplyr)

df = read.csv("breast-cancer-wisconsin.data")

names = c("ID", "Clump Thickness", "Uniformity Cell Size", "Uniformity Cell Shape",
          "Marginal Adhesion", "Single Epithelial size", "Bare Nuclei",
          "Bland Chromatin", "Normal Nucleoli", "Mitoses", "Class")
colnames(df) = names

df$`Bare Nuclei` = as.numeric(df$`Bare Nuclei`)
df[df=="?"] = NA
df$Class1 = (df$Class=="2")*1
df = select(df, -Class)

levels(df$Class) = c("benign","malignant")

df = na.omit(df)
df = select(df, -ID)

entropy = function(data){
  if (!is.factor(data)) data = factor(data)
  probs = prop.table(table(data))
  entropy = -sum(probs*log2(probs))
  return(entropy)
}

gini = function(y,class){
  Table = table(y,class)
  nx = apply(Table,2,sum)
  num = sum(Table)
  pxy = Table/matrix(rep(nx,each=2),nrow=2)
  omega = matrix(rep(nx,each=2),nrow=2)/num
  gini = -sum(omega*pxy*(1-pxy))
  return(gini)
}


#To divide or not divide?
#Net Gain by splitting:
gini(y=df$Class1,class=(df[,1]>2)) - gini(y=df$Class1,class=df[,1]>-Inf)

#How to split? we can run over each of the possible values to each feature,
sort(unique(df[,1]))
#we can also try ~100 cut-off places to try the split, for each variable, in equal distances:
#This way, we will compute 11*101 gini indices
seq(min(df[,1]),max(df[,1]),length=101)


# install.packages("data.tree")
library(data.tree)

pure = function(data){
  length(unique(data$Class))==1
}

p = ncol(df)-1

C4.5 = function(node, data){
  node$Num.Observations = nrow(data)
  
  #stopping the algorithm if there are no more features to classify with
  #if the data is pure, create a node with the class name 
  
  if (pure(data)){
    child = node$AddChild(unique(data$Class))
    node$features = "Class"
    child$Num.Observations = nrow(data)
    child$features = ""
  }
  
  
  else {
    #Find the feature that has the highest entropy gain
    p = ncol(data)-1
    mat_gini = mat_v=matrix(NA,p,101)
    for(v in 1:p)
    {
      variable=df[,v]
      v_seq=seq(quantile(df[,v],6/length(df[,v])), quantile(df[,v],1-6/length(df[,v])),length=101)
      mat_v[v,]=v_seq
      for(i in 1:101)
      {
        CLASS=variable>=v_seq[i]
        mat_gini[v,i]=
          gini(y=df$Class1,class=CLASS)
      }
    }
    
    best_feature = names(df)[which(mat_gini==max(mat_gini),arr.ind = TRUE)[1]]
    node$feature = best_feature
    split.value = median(mat_v[which(mat_gini==max(mat_gini),arr.ind = TRUE)])
    
    
    #subset the remaining data
    childSubset = split(data[,!(names(data) %in% best_feature)], data[,best_feature], drop = TRUE)
    
    for(i in 1:length(childSubset)) {
      child <- node$AddChild(names(childSubset)[i])
      
      #call the algorithm recursively on the child and the subset      
      RBS(child, childSubset[[i]])
    }
  }
}
  

par(mfrow=c(3,3))
for(v in 1:p){
  plot(mat_v[v,],mat_gini[v,],type="l",
       ylim=range(mat_gini),xlab="",ylab="",
       main=names(df)[v]) 
  abline(h=max(mat_gini),col="blue")
}
