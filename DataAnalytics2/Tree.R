install.packages('rpart');
install.packages('randomForest');
install.packages('xgboost');
install.packages('party');
install.packages("caret")
install.packages("nloptr")
require("nloptr")
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

rf = randomForest(r ~ col1 + col2 + col3 + col4, data = train, ntree = 50)
rf
plot(rf)
#predict the values for the test set
test_predict_rf = getPredictionDataFrame4D(rf, test[,-5])
#predict the values for the whole initial dataset
predicted_rf = getPredictionDataFrame4D(rf, getGridData4D(0,1,0,1,0,1,0,1,10))
min(test_predict_rf$r)
min(dataset$r)

#compute the mse for the trained model according to the holdout-test set
rf_error = test_predict_rf$r - test$r
rf_error = sqrt(mean(rf_error^2))
rf_error


#function to predict value from the RandomForest (to be used as fitness function for GA)
fun_rf4D = function(tree, x1, x2, x3, x4) {
  t = data.frame("col1"= x1, "col2"=x2, "col3"= x3, "col4"=x4)
  return(predict(tree, t))
}

#plot predicted Data in "4D" way
scatter3D(bty = "b2", x = predicted_rf[,1], xlab = "col1", y = predicted_rf[,2], ylab = "col2", z = predicted_rf[,5], zlab = "r", main = "text = col3; color = col4", cex = 1, pch = 19, theta = 10, phi = 10, colvar = predicted_rf[,4],ticktype = "detailed")
text3D(x= predicted_rf[,1], y = predicted_rf[,2], z = predicted_rf[,5],  labels = round(predicted_rf[,3],2),add = TRUE, colkey = FALSE, cex = 1)
plotrgl()

#Applying the GA to find the minimum value predicted by the RandomForest
GA_tree <- ga(type = "real-valued", fitness = function (x) {1-fun_rf4D(rf,x[1],x[2],x[3],x[4])}, lower = c(0,0,0,0), upper = c(1,1,1,1), maxiter = 1000, run = 50, popSize = 100)

#Investigate the result of the GA
summary(GA_tree)
plot(GA_tree)
GA_tree@solution

#Investigate what values the Forest predicts for the best solutions of the GA
predict(rf,data.frame("col1" = GA_tree@solution[,1], "col2" = GA_tree@solution[,2], "col3" = GA_tree@solution[,3], "col4" = GA_tree@solution[,4]))

#Validate the best solutions from the randomForest + GA by looking at the actual values provided by the API
dataset_2 = getData(data.frame("col1" = GA_tree@solution[,1], "col2" = GA_tree@solution[,2], "col3" = GA_tree@solution[,3], "col4" = GA_tree@solution[,4]), token)
min(dataset_2$r)
cov(dataset_2)
pairs(dataset_2[-5])

#   The variance of the coordinates is very! low ==> Assumption: They are very close together and form a "plateau/valley" in the Model, The r values from the API
#   are also quite close to each other

#Applying the subplex Algorithm (once) to the trained Forest
sp_tree = c(runif(1,0,0.5),runif(1,0,1),runif(1,0,1),runif(1,0,1))
sp_result = sbplx(sp_tree,fn = function (x) {1 - fun_rf4D(rf, x[1],x[2],x[3],x[4])},lower = c(0,0,0,0), upper = c(1,1,1,1))
#validate the result by predicting the r with the Forest
predict(rf,data.frame("col1" = sp_result$par[1], "col2" = sp_result$par[2], "col3" = sp_result$par[3], "col4" = sp_result$par[4]))
#validate the result by using the API
dataset_3 = getData(data.frame("col1" = sp_result$par[1], "col2" = sp_result$par[2], "col3" = sp_result$par[3], "col4" = sp_result$par[4]), token)


#function to apply the subplex algorithm a number of times with startingpoints on a 4D-Grid
subplex_iterations = function(interval) {
  g = getGridData4D(0,1,0,1,0,1,0,1,interval)
  g2 = data.frame("x1" = c(rep(0,nrow(g))),"x2" = c(rep(0,nrow(g))),"x3" = c(rep(0,nrow(g))),"x4" = c(rep(0,nrow(g))),"value" = c(rep(0,nrow(g))))
  for (i in 1:nrow(g)) {
    temp = sbplx(x0 = as.numeric(g[i,]), fn = function (x) {fun_rf4D(rf, x[1],x[2],x[3],x[4])},lower = c(0,0,0,0), upper = c(1,1,1,1))
    g2[i,1] = temp$par[1]
    g2[i,2] = temp$par[2]
    g2[i,3] = temp$par[3]
    g2[i,4] = temp$par[4]
    g2[i,5] = temp$value
  }
  g3 = data.frame(g,g2)
  return(g3)
}

subplex_result = subplex_iterations(3)
subplex_result = sbplx(x0 = c(1,0.75,0.75,0), fn = function (x) {fun_rf4D(rf, x[1],x[2],x[3],x[4])},lower = c(0,0,0,0), upper = c(1,1,1,1))
subplex_result$par
getData(data.frame("col1" = subplex_result$par[1], "col2" = subplex_result$par[2], "col3" = subplex_result$par[3], "col4" = subplex_result$par[4]),token)
predict(rf,data.frame("col1" = subplex_result$par[1], "col2" = subplex_result$par[2], "col3" = subplex_result$par[3], "col4" = subplex_result$par[4]))
#Run the subplex algorithm with starting points on a grid with interval = 3
test = subplex_iterations(3)

#Row 79 has the highest value i.e. the lowest predicted value (=1-value)
predict(rf,data.frame("col1" = subplex_result[which.max(subplex_result$value),5], "col2" = subplex_result[which.max(subplex_result$value),6], "col3" = subplex_result[which.max(subplex_result$value),7], "col4" = subplex_result[which.max(subplex_result$value),8]))
1-subplex_result[which.max(subplex_result$value),9]
#Validation with the API shows that the point is actually quite "bad" (expected from bad predicted value)
getData(data.frame("col1" = subplex_result[which.min(subplex_result$value),5], "col2" = subplex_result[which.min(subplex_result$value),6], "col3" = subplex_result[which.min(subplex_result$value),7], "col4" = subplex_result[which.min(subplex_result$value),8]),token)
#Tune the trained forrest using the caret package


