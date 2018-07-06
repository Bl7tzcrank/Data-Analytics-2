install.packages('rpart');
install.packages('randomForest');
install.packages('xgboost');
install.packages('party');
require('rpart');
require('randomForest');
require('xgboost');
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