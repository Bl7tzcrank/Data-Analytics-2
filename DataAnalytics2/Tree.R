install.packages('rpart');
install.packages('randomForest');
install.packages('xgboost');
install.packages('party');
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
scaledDataset = scalingData(dataset)
train = dataset[index,]
test = dataset[-index,]

rf = randomForest(r ~ col1 + col2 + col3 + col4, data = train)
rf
test_predict_rf = getPredictionDataFrame4D(rf, test[,-5])
predicted_rf = getPredictionDataFrame4D(rf, dataset[,-5])
test_predict_rf = sort(test_predict_rf[,5], decreasing = TRUE)
predicted_rf = sort(predicted_rf[,5], decreasing = TRUE)
?sort
min(predicted_rf$r)
min(dataset$r)



rf_error = test_predict_rf$r - test$r
rf_error = sqrt(mean(rf_error^2))
rf_error

?randomForest
