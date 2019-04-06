#               LOGISTIC REGRESSION                #
#     Stock Market Prediction - Smarket.csv        #

library(ISLR)
library(rattle)

df = Smarket
attach(df)

############################################
#           Descriptive Stats              #
############################################

source("Descriptive Stats.R")
DescStats(Lag3)
DescStats(Today)
DescStats(Volume)

?Smarket

############################################
#          Correlation Matrix              #
############################################

pairs(df)
library(corrplot)

head(df)
cor(Today, Volume,Lag1,Lag2, Lag3, Lag4, Lag5)

cordf = data.frame(Volume,Lag1,Lag2, Lag3, Lag4, Lag5, Direction)
d = read.table(text = cordf, header = T)

library(ggcorrplot)

ggcorrplot(d)
p.mat <- cor_pmat(cordf)
p.mat
#Seams that there's no correlation?

############################################
#             GGPLOT BoxPlot               #
############################################
library(ggplot2)
library(reshape2)

df.m <- melt(cordf, id.var = "Direction")

ggplot(df.m, aes(x=Direction, y=value)) + 
  geom_boxplot(aes(fill=Direction))+
  stat_summary(fun.y = "mean", geom = "point", shape= 23, size= 3, fill= "red") +
  facet_wrap(~variable, scales="free")

############################################
#             GGPLOT Density               #
############################################

ggplot(df.m,aes(x=value, fill=Direction)) + 
  geom_density(alpha = 0.5)+
  facet_wrap(~variable, scales="fixed")



############################################
#             GGPLOT BoxPlot               #
############################################
