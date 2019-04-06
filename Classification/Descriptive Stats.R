DescStats = function(x) {
  descriptiveStats = c(sum(!is.na(x)),mean(x),sd(x),quantile(x, 0.25),median(x),quantile(x, 0.75),min(x),max(x),max(x)-min(x))
  descriptiveStats = round(descriptiveStats,2)
  descriptiveMeasure = c("Valid Sample", "Mean", "STD", "Q1",  "Median", "Q3", "Min", "Max", "Range")
  descriptives = data.frame(descriptiveMeasure, descriptiveStats)
  colnames(descriptives) = c("Statistics", "Value")
  return (descriptives)
}

addDesc = function(x, prec){
  library(moments)
  adddescStats = c(mean(x,trim = prec/100),skewness(x),kurtosis(x),sd(x)/mean(x))
  adddescStats = round(adddescStats,2)
  adddescMeasure = c(sprintf("%s Trimmed Mean", prec),"Skewness","Kurtosis","CV")
  adddesc = data.frame(adddescMeasure,adddescStats)
  colnames(adddesc) = c("Statistic", "Value")
  return (adddesc)
}
print(DescStats(Age))
print(addDesc(Age, 2))
