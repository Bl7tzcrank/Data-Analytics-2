#imports
install.packages("e1071")
install.packages("plot3D")
install.packages("plotly");
require('plotly');
require(plot3D)
require(e1071)
source("main.R")

#Create a data frame of the data
dataset = getData(getGridData(0,1,20,2),token)
scaledData = scalingData(dataset)
index = splitData(dataset, 0.80)
train = scalingData(dataset[index,])


test = scalingData(dataset[-index,])

#train = data.frame("x" = x, "y" = y)
#index <- 1:nrow(train)
#testindex <- sample(index, trunc(length(index)/3))
#testset <- na.omit(train[testindex,-3])
#trainset <- na.omit(train[-testindex,-3])

#Plot the dataset
points3D(x = dataset$col1, y = dataset$col2, z = dataset$r)

grid_start = 0;
grid_end = 1;
grid_interval = (grid_end - grid_start)/20;
grid_dimensions = 2;

plotdata <- dataset[,ncol(dataset)];

dim(plotdata) <- c(length(c(seq(grid_start,grid_end,grid_interval))),length(c(seq(grid_start,grid_end,grid_interval))));
rownames(plotdata) <-  c(seq(grid_start,grid_end,grid_interval));
colnames(plotdata) <-  c(seq(grid_start,grid_end,grid_interval));
plot_ly(x = rownames(plotdata), y = colnames(plotdata), z=plotdata, type="surface")
#Fit a model. The function syntax is very similar to lm function
model_svm <- svm(r ~ col1+col2 , scaledData)

#Use the predictions on the data
pred <- predict(model_svm, getGridData(0,05,80,2))

createSVMModel = function(testData){
  return(model_svm <- svm(r ~ col1+col2 , testData))
}

getPredictionDataFrame = function(model, data){
  pred = predict(model, data)
  return(data.frame("col1" = data$col1, "col2" = data$col2, "r" = pred))
}

dataset = getData(getGridData(0.05,0.225,0.0875,0.2625,20,2),token)
model_svm = createSVMModel(dataset)
predDataFrame = getPredictionDataFrame(model_svm, getGridData(0.05,0.225,0.0875,0.2625,80,2))
predDataFrame[which(predDataFrame[,3] == min(predDataFrame[,3])),]


#Plot the predictions and the plot to see our model fit
points3D(test$col1, test$col2, pred, col = "blue", pch=4, add = TRUE)

#For svm, we have to manually calculate the difference between actual values (train$y) with our predictions (pred)
error <- test$r - pred
svm_error <- sqrt(mean(error^2))


# perform a grid search
svm_tune <- tune(svm, r ~ col1+col2, data = train,
                 ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9))
)
print(svm_tune)

#The best model
best_mod <- svm_tune$best.model
best_mod_pred <- predict(best_mod, test) 

error_best_mod <- test$r - best_mod_pred 

# this value can be different on your computer
# because the tune method randomly shuffles the data
best_mod_RMSE <- sqrt(mean(error_best_mod^2))

plot(svm_tune)

points3D(test$col1, test$col2, test$r,pch=16)
points3D(test$col1, test$col2, best_mod_pred, col = "blue", pch=4, add = TRUE)
