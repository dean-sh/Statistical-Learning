#     Model Optimization and selection

# Feature\Variable Selection
# improving the data interpertation by:
# 1. Shrinking the size of the model parameters
# 2. dropping unwanted variables and estimators

# - Methods: Subset Selection (Back-fwd), 
# - Shrinkage (Ridge, Lasso), 
# - Dimension Reduction (PCA, high correlation filter)


#############################################################################
#Best Subset Selection: modeling a linear regression for each of p combinations.

# 1. for each k in 1:p, choose(p,k) models, and finding the best one using R2.
# 2. compare each best model Mk, using  adjR2, AIC, BIC, Cp etc.
# 3. Testing the best model on the test set.

p = 20 #variables
num.models = 0

for (c in 1:p){
  num.models = num.models +  choose(p,c)
}
print(num.models) #We get 1M models for 20 variables. That's Huge!


#############################################################################
# Forward Selection

# 1. Start with empty model
# 2. In each step, model with the variable that will increase R2 the most. ~ O(p*n)
# 3. Compare the best model in each size, and choose using CV and other estimators.


#############################################################################
# Stepwise Selection

# 1. Start with empty model
# 2. In each step, model with the variable that will increase R2 the most. ~ O(p*n)
# 3. Compare the best model in each size, and choose using CV and other estimators.



