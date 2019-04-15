m = 1000 #samples
n = 30 #Sample size

PV1 = rep(1,m) #Vector of p values
PV2 = rep(1,m) #Vector of p values

rejectedH0 = rep(0,m)

for (j in 1:m){
  x = rnorm(n,0,1)
  y = rnorm(n,0,1)
  
  ttest = t.test(x,y,var.equal = T)
  PV1[j] = ttest$p.value
  if (ttest$p.value<= 0.05)
  {
    rejectedH0[j] = 1
  }
}

rejectedH0 = rep(0,m)

for (j in 1:m){
  x = rnorm(n,0,1)
  y = rnorm(n,1,1)
  
  ttest = t.test(x,y,var.equal = T)
  PV2[j] = ttest$p.value
  if (ttest$p.value<= 0.05)
  {
    rejectedH0[j] = 1
  }
}

PV = c(PV1, PV2)
sum(PV<0.05)
sum(PV<0.05/2000)

P.adj = p.adjust(PV, method = "BH")

sum(P.adj<0.05)

# counts = data.frame(table(rejectedH0))
# counts$Perc = counts$Freq / m
# counts
