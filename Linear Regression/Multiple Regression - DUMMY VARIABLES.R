#DUMMY VARIABLES - When we have categorical variables.



###################################################
# Insurance Data

#This method, assumes that both company types has the same slope.
library(readxl)
library(ggplot2)
df = read_xlsx("InsuranceDemo.xlsx")

df$CompanyType = factor(df$CompanyType)

ggplot(df, aes(x = CompanySize, y = Months, shape = CompanyType, color = CompanyType, size = 2)) +
  geom_point()

ggplot(df, aes(x = CompanySize, y = Months, shape = CompanyType, color = CompanyType, size = 2)) +
  geom_point() + geom_smooth(method = "lm", fill=NA, size = 1)

reg1 = lm(df$Months~df$CompanySize+df$CompanyType)
summary(reg1)
plot(reg1)

###################################################
# Insurance Data WITH interaction

reg2 = lm(df$Months~df$CompanySize * df$CompanyType)
summary(reg2)
plot(reg2)


###################################################
# checking if our data can be described using linear regression?
plot(reg1)

#first graph should explain the linearity, and should not have a trend.
#the points should be scattered arround 0.

#Second graph checks the normality of the residuals.

#third graph Scale Locations explains if the variance is constant.
# a horizontal line tells a const. variance.

# fourth - Leverage and Outliers
