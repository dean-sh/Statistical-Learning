
## Random Forest and bagging of Boston Dataset


```R
library(MASS)
library(randomForest)
set.seed(42)

df = Boston
```


```R
head(df)
```


<table>
<thead><tr><th scope=col>crim</th><th scope=col>zn</th><th scope=col>indus</th><th scope=col>chas</th><th scope=col>nox</th><th scope=col>rm</th><th scope=col>age</th><th scope=col>dis</th><th scope=col>rad</th><th scope=col>tax</th><th scope=col>ptratio</th><th scope=col>black</th><th scope=col>lstat</th><th scope=col>medv</th></tr></thead>
<tbody>
	<tr><td>0.00632</td><td>18     </td><td>2.31   </td><td>0      </td><td>0.538  </td><td>6.575  </td><td>65.2   </td><td>4.0900 </td><td>1      </td><td>296    </td><td>15.3   </td><td>396.90 </td><td>4.98   </td><td>24.0   </td></tr>
	<tr><td>0.02731</td><td> 0     </td><td>7.07   </td><td>0      </td><td>0.469  </td><td>6.421  </td><td>78.9   </td><td>4.9671 </td><td>2      </td><td>242    </td><td>17.8   </td><td>396.90 </td><td>9.14   </td><td>21.6   </td></tr>
	<tr><td>0.02729</td><td> 0     </td><td>7.07   </td><td>0      </td><td>0.469  </td><td>7.185  </td><td>61.1   </td><td>4.9671 </td><td>2      </td><td>242    </td><td>17.8   </td><td>392.83 </td><td>4.03   </td><td>34.7   </td></tr>
	<tr><td>0.03237</td><td> 0     </td><td>2.18   </td><td>0      </td><td>0.458  </td><td>6.998  </td><td>45.8   </td><td>6.0622 </td><td>3      </td><td>222    </td><td>18.7   </td><td>394.63 </td><td>2.94   </td><td>33.4   </td></tr>
	<tr><td>0.06905</td><td> 0     </td><td>2.18   </td><td>0      </td><td>0.458  </td><td>7.147  </td><td>54.2   </td><td>6.0622 </td><td>3      </td><td>222    </td><td>18.7   </td><td>396.90 </td><td>5.33   </td><td>36.2   </td></tr>
	<tr><td>0.02985</td><td> 0     </td><td>2.18   </td><td>0      </td><td>0.458  </td><td>6.430  </td><td>58.7   </td><td>6.0622 </td><td>3      </td><td>222    </td><td>18.7   </td><td>394.12 </td><td>5.21   </td><td>28.7   </td></tr>
</tbody>
</table>



#### Train-Test Split


```R
train.index = sample(1:nrow(Boston), 0.7*nrow(Boston))
train = Boston[train.index,]
test = Boston[-train.index,]

cat("train size:", dim(train))
cat("\ntest size:", dim(test))

```

    train size: 354 14
    test size: 152 14

### Modeling a regression random forest, with m = p variables (all 13 features) 


```R
rf.Boston = randomForest(medv~., data = Boston, subset =  train.index 
                         ,mtry = 13, importance = T)

par(mfrow = c(1,1))
plot(rf.Boston,main = "Number of trees vs training error")
        
```


![png](Random%20Forest%20and%20bagging%20of%20Boston_files/Random%20Forest%20and%20bagging%20of%20Boston_6_0.png)


### Prediction:


```R
rf.y = predict(rf.Boston, newdata = test)
plot(rf.y, test$medv)
abline(0,1)

#Test MSE:
cat("Test MSE:" ,mean((rf.y-test$medv)^2))
```

    Test MSE: 10.51674


![png](Random%20Forest%20and%20bagging%20of%20Boston_files/Random%20Forest%20and%20bagging%20of%20Boston_8_1.png)


#### Now, I will use only 6 variables, "Feature Bagging", to build each tree.


```R
rf.bagging.Boston = randomForest(medv~., data = Boston, subset = train.index,
                                 mtry=6, importance = T)
rf.y = predict(rf.bagging.Boston, newdata = test)
print(paste0("Test MSE: " ,mean((rf.y-test$medv)^2)))
print("We got a MSE which is a bit smaller")
```

    [1] "Test MSE: 9.79413446036241"
    [1] "We got a MSE which is a bit smaller"
    

## Variable importance

Calling "importance()" shows the importance of each variable, calculated by permuting the Out-of-bag examples - for every tree, there are examples which were not used to create it. This sample is used to calculate the importance measure.
The prediction accuracy is calculated from those OOB examples.
Then, one variable on the OOB examples is randomly shuffled, effectivly removing its predictive power, while keeping other variables fixed. 
Then, the difference between the accuracies is measured.

This is averaged on all the trees and variables, and normalized by the std.



```R
importance(rf.bagging.Boston)
varImpPlot(rf.bagging.Boston, main = "Variable Importance") 
```


<table>
<thead><tr><th></th><th scope=col>%IncMSE</th><th scope=col>IncNodePurity</th></tr></thead>
<tbody>
	<tr><th scope=row>crim</th><td>15.568419  </td><td> 1672.67672</td></tr>
	<tr><th scope=row>zn</th><td> 2.389154  </td><td>   99.86424</td></tr>
	<tr><th scope=row>indus</th><td>10.261993  </td><td> 1573.10496</td></tr>
	<tr><th scope=row>chas</th><td> 3.537602  </td><td>  167.78971</td></tr>
	<tr><th scope=row>nox</th><td>16.118009  </td><td> 1534.60671</td></tr>
	<tr><th scope=row>rm</th><td>39.556305  </td><td>10959.44630</td></tr>
	<tr><th scope=row>age</th><td>12.768607  </td><td>  761.84978</td></tr>
	<tr><th scope=row>dis</th><td>14.837758  </td><td> 1476.95458</td></tr>
	<tr><th scope=row>rad</th><td> 5.123607  </td><td>  223.31037</td></tr>
	<tr><th scope=row>tax</th><td>11.949692  </td><td>  585.83739</td></tr>
	<tr><th scope=row>ptratio</th><td>13.940940  </td><td> 1451.31262</td></tr>
	<tr><th scope=row>black</th><td>11.347194  </td><td>  525.97600</td></tr>
	<tr><th scope=row>lstat</th><td>31.982485  </td><td>10295.83683</td></tr>
</tbody>
</table>




![png](Random%20Forest%20and%20bagging%20of%20Boston_files/Random%20Forest%20and%20bagging%20of%20Boston_13_1.png)



```R

```
