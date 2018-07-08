install.packages('rpart');
install.packages('randomForest');
install.packages('xgboost');
install.packages('party');
install.packages("caret")
require("caret")
require('rpart');
require('randomForest');
require('xgboost');
require("party")
source("main.R")

##data=train for training
createTreeModel = function(dataset){
  return(rpart_model <- rpart(r ~ col1 + col2, data=dataset, method="anova"))
} 

##Plot the full tree
##plot(rpart_model);text(rpart_model);
plot(rpart_model, uniform = TRUE, main="Decision Tree for train");
text(rpart_model, use.n = TRUE, all=TRUE, cex=.8);

##Cost-complexity pruning
printcp(rpart_model);
plotcp(rpart_model);
summary(rpart_model);
opt <- which.min(rpart_model$cptable[,"xerror"])
cp <- rpart_model$cptable[opt, "CP"]
pruned_model<-prune(rpart_model,cp)

##output predictions as vector - 1 prediction per record in dataset
test_predict <- predict(rpart_model, test, type = "vector");

##calculate root-mean-square error by comparing test_predict vector with test
rmsqe <- sqrt(mean((test_predict-test)^2))

##Function to do multiple runs 
##not sure if it makes sense to do this on train data, the rmsqe just reduces by 0.01


## RF <- randomForest(r ~ col1 + col2, train, ntree=50)


###########################################################################
##################### 4D RandomForrest Regression #########################
###########################################################################


dataset = getData(getGridData4D(0,1,0,1,0,1,0,1,4),token)

index <- splitData(dataset, 0.7)

train = dataset[index,]
test = dataset[-index,]

rf = randomForest(r ~ col1 + col2 + col3 + col4, data = train)
rf
#predict the values for the test set
test_predict_rf = getPredictionDataFrame4D(rf, test[,-5])
#predict the values for the whole initial dataset
predicted_rf = getPredictionDataFrame4D(rf, dataset[,-5])
#sort those predictions by the dependent variable and compare the minima
test_predict_rf = sort(test_predict_rf[,5], decreasing = TRUE)
predicted_rf = sort(predicted_rf[,5], decreasing = TRUE)

min(predicted_rf$r)
min(dataset$r)

#compute the mse for the trained model according to the holdout-test set
rf_error = test_predict_rf$r - test$r
rf_error = sqrt(mean(rf_error^2))
rf_error

#Applying the GA to find the minimum value predicted by the RandomForest
GA <- ga(type = "real-valued", fitness = function (x) {}, lower = c(0,0,0,0), upper = c(1,1,1,1), maxiter = 1000, run = 50)

fun_NN_4D(0.97,0.84,0.42,0.69)

summary(GA)
plot(GA)
GA@solution
compute(NN, GA@solution)



#Tune the trained forrest using the caret package

