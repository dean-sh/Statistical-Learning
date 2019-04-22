# Decision Tree Algorithm From Scratch
library(dplyr)

df = read.csv("breast-cancer-wisconsin.data")

names = c("ID", "Clump Thickness", "Uniformity Cell Size", "Uniformity Cell Shape",
          "Marginal Adhesion", "Single Epithelial size", "Bare Nuclei",
          "Bland Chromatin", "Normal Nucleoli", "Mitoses", "Class")
colnames(df) = names

df[df=="?"] = NA

levels(df$Class) = c("benign","malignant")

df = na.omit(df)
df = select(df, -ID)

entropy = function(data){
  if (!is.factor(data)) data = factor(data)
  probs = prop.table(table(data))
  entropy = -sum(probs*log2(probs))
  return(entropy)
}

# install.packages("data.tree")
library(data.tree)

pure = function(data){
  length(unique(data$Class))==1
}

#Recursive Binary Splitting ID3
RBS = function(node, data) {
  
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
    entropy.val = sapply(colnames(df)[-ncol(df)],
                         function(x) entropy(
                           table(data[,x],data[,ncol(data)])))
    
    feature = names(entropy.val)[entropy.val == max(entropy.val)][1]
    node$feature = feature
    
    #subset the remaining data
    childSubset = split(data[,!(names(data) %in% feature)], data[,feature], drop = TRUE)
    
    for(i in 1:length(childSubset)) {
      child <- node$AddChild(names(childSubset)[i])
      
      #call the algorithm recursively on the child and the subset      
      RBS(child, childSubset[[i]])
    }
  }
}

Tree = Node$new("Bare Nuclei")
plot(Tree)  
RBS(Tree, df)

plot(Tree)
