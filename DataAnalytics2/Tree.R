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


###########################################################################
##################### 4D RandomForrest Regression #########################
###########################################################################

data_prod = read.csv(file = "prod.csv")
data_prod = data_prod[,-1]
dataset = data_prod
dataset = getData(getGridData4D(0,1,0,1,0,1,0,1,3),token)
index <- splitData(dataset, 0.8)

train = dataset[index,]
test = dataset[-index,]


# First step: Determine the best number of predictors to choose at each split while training the randomForest
#     For this consider all possible values 1:4 (default is number of available predictors / 3)

#out of bag error (mse)
oob.error=double(4)
#test error (mse)
test.error=double(4)

#mtry is no of Variables randomly chosen at each split
for(x in 1:4)  {
  rf=randomForest(r ~ col1 + col2 + col3 + col4 , data = train, mtry=x, ntree=100) 
  oob.error[x] = rf$mse[100] 
  
  #Predictions on Test Set for each Tree
  pred = predict(rf,test[-5]) 
  test.error[x]= with(test, mean( (r - pred)^2))
  
  #print to console
  cat(x, oob.error[x])
  cat(x, test.error[x]) 
  
}

#plot the results visually to identify the best value for mtry
matplot(1:x , cbind(oob.error,test.error), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

#results indicate that mtry = 2 or mtry = 3 is a reasonable parameter (even though the differences in mse are very small)
# number of trees is not really important, just needs to be "large enough" so documentation for reasoning & sources


#find the best training parameters, using the caret package with 10-fold cross validation on the training dataset
fitControl = trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 10)

newrf = train (r ~ col1 + col2 + col3 + col4, data = dataset, method = "rf", trControl = fitControl)
#assign the best fitted model
best_rf=newrf$finalModel
best_rf

#predict the values for the test set
test_predict_rf = getPredictionDataFrame4D(best_rf, test[,-5])
#predict the values for the whole initial dataset
predicted_rf = getPredictionDataFrame4D(best_rf, getGridData4D(0,1,0,1,0,1,0,1,10))
min(predicted_rf$r)
min(dataset$r)

#compute the mse for the trained model according to the holdout-test set
rf_error = test_predict_rf$r - test$r
rf_error = sqrt(mean(rf_error^2))
rf_error

#plot predicted Data in "4D" way
#scale first to make interpretation easier

predicted_rf = scalingData(predicted_rf)
scatter3D(bty = "b2", x = predicted_rf[,1], xlab = "col1", y = predicted_rf[,2], ylab = "col2", z = predicted_rf[,5], zlab = "r", main = "text = col3; color = col4", cex = 1, pch = 19, theta = 10, phi = 10, colvar = predicted_rf[,4],ticktype = "detailed")
text3D(x= predicted_rf[,1], y = predicted_rf[,2], z = predicted_rf[,5],  labels = round(predicted_rf[,3],2),add = TRUE, colkey = FALSE, cex = 1)
plotrgl()

#Visual Investigation shows the following ranges are promising (lowest valley):
# col1: 0.7 - 1
# col2: 0.8 - 1
# col3: 0.2 - 0.5
# col4: 0.5 - 0.8

#function to predict value from the RandomForest (to be used as fitness function for GA)
fun_rf4D = function(tree, x1, x2, x3, x4) {
  t = data.frame("col1"= x1, "col2"=x2, "col3"= x3, "col4"=x4)
  return(predict(tree, t))
}

#Applying the GA to find the minimum value predicted by the RandomForest
GA_tree <- ga(type = "real-valued", fitness = function (x) {1-fun_rf4D(best_rf,x[1],x[2],x[3],x[4])}, lower = c(0,0,0,0), upper = c(1,1,1,1), maxiter = 1000, run = 50, popSize = 100)
GA_tree_focus = ga(type = "real-valued", fitness = function (x) {1-fun_rf4D(best_rf,x[1],x[2],x[3],x[4])}, lower = c(0.5,0.5,0.15,0.5), upper = c(0.8,0.8,0.45,0.8), maxiter = 1000, run = 50, popSize = 100)
#Investigate the result of the GA
summary(GA_tree_focus)
plot(GA_tree_focus)
GA_tree_focus@solution

#Investigate what values the Forest predicts for the best solutions of the GA
predict(rf,data.frame("col1" = GA_tree_focus@solution[,1], "col2" = GA_tree_focus@solution[,2], "col3" = GA_tree_focus@solution[,3], "col4" = GA_tree_focus@solution[,4]))

#Validate the best solutions from the randomForest + GA by looking at the actual values provided by the API
dataset_2 = getData(data.frame("col1" = GA_tree_focus@solution[,1], "col2" = GA_tree_focus@solution[,2], "col3" = GA_tree_focus@solution[,3], "col4" = GA_tree_focus@solution[,4]), token)
min(dataset_2$r)
cov(dataset_2)
pairs(dataset_2[-5])

#   The variance of the coordinates is very! low ==> Assumption: They are very close together and form a "plateau/valley" in the Model, The r values from the API
#   are also quite close to each other

#Get more detailed grid from the API for the identified valley:

dataset2 = getData(getGridData4D(0.7,1,0.8,1,0.2,0.5,0.5,0.8,2),token)
index <- splitData(dataset2, 0.8)

train2 = dataset2[index,]
test2 = dataset2[-index,]

# Train and tune a second Forest for the detailed range, skipping manual experimentation, just using the caret package

fitControl = trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 10)

newrf2 = train (r ~ col1 + col2 + col3 + col4, data = train2, method = "rf", trControl = fitControl)
#assign the best fitted model
best_rf2=newrf2$finalModel

#predict the values for the test set
test_predict_rf2 = getPredictionDataFrame4D(best_rf2, test2[,-5])
#predict the values for the whole initial dataset
predicted_rf2 = getPredictionDataFrame4D(best_rf2, getGridData4D(0.7,1,0.8,1,0.2,0.5,0.5,0.8,10))
min(test_predict_rf2$r)
min(dataset2$r)

#compute the mse for the trained model according to the holdout-test set
rf_error2 = test_predict_rf2$r - test2$r
rf_error2 = sqrt(mean(rf_error2^2))
rf_error2

#visually explore the predictions of the more detailed model
scatter3D(bty = "b2", x = predicted_rf2[,1], xlab = "col1", y = predicted_rf2[,2], ylab = "col2", z = predicted_rf2[,5], zlab = "r", main = "text = col3; color = col4", cex = 1, pch = 19, theta = 10, phi = 10, colvar = predicted_rf2[,4],ticktype = "detailed")
text3D(x= predicted_rf2[,1], y = predicted_rf2[,2], z = predicted_rf2[,5],  labels = round(predicted_rf2[,3],2),add = TRUE, colkey = FALSE, cex = 1)
plotrgl()

#Apply the GA to find the Minimum
GA_tree2 = ga(type = "real-valued", fitness = function (x) {1-fun_rf4D(best_rf2,x[1],x[2],x[3],x[4])}, lower = c(0.7,0.8,0.2,0.5), upper = c(1,1,0.5,0.8), maxiter = 1000, run = 50, popSize = 100)

#Investigate what values the Forest predicts for the best solutions of the GA
predict(best_rf2,data.frame("col1" = GA_tree2@solution[,1], "col2" = GA_tree2@solution[,2], "col3" = GA_tree2@solution[,3], "col4" = GA_tree2@solution[,4]))

#Validate the best solutions from the randomForest + GA by looking at the actual values provided by the API
dataset_3 = getData(data.frame("col1" = GA_tree2@solution[,1], "col2" = GA_tree2@solution[,2], "col3" = GA_tree2@solution[,3], "col4" = GA_tree2@solution[,4]), token)
min(dataset_3$r)
cov(dataset_2)
pairs(dataset_2[-5])


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

###########################################################################
##################### NN Test Tuning ######################################
###########################################################################

tuningGrid = expand.grid("layer1"=c(6:8),"layer2"=c(6:8),"layer3"=(6:8))
testNN2 = train(r~col1+col2+col3+col4,data = dataset, method = "neuralnet", preProcess = "scale", metric = "RMSE", tuneGrid = tuningGrid,rep = 1,threshold = 0.01)
testNN3 = train(r~col1+col2+col3+col4,data = dataset, method = "neuralnet", preProcess = "scale", metric = "RMSE", tuneGrid = tuningGrid,rep = 1,threshold = 0.01)
testNN4 = train(r~col1+col2+col3+col4,data = dataset, method = "neuralnet", preProcess = "scale", metric = "RMSE", tuneGrid = tuningGrid,rep = 1,threshold = 0.01)

NN = neuralnet(r ~ col1 + col2 + col3 + col4, data = train, hidden = c(500), linear.output= T, stepmax = 1e+6)
?neuralnet
NN$err.fct
