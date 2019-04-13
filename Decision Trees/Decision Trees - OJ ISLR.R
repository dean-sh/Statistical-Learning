library(ISLR)

data(OJ)
text(OJ)
attach(OJ)

plot(SpecialCH~SpecialMM)

plot(OJ$Purchase,OJ$PriceCH)
