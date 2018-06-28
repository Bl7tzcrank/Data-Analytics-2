#imports
install.packages("e1071")
install.packages("plot3D")
require(plot3D)
require(e1071)
source("main.R")

#Create a data frame of the data
dataset = getData(getRandomData(20,2),token)
index = splitData(dataset, 0.60)
train = scalingData(dataset[index,])
test = scalingData(dataset[-index,])

#train = data.frame("x" = x, "y" = y)
#index <- 1:nrow(train)
#testindex <- sample(index, trunc(length(index)/3))
#testset <- na.omit(train[testindex,-3])
#trainset <- na.omit(train[-testindex,-3])

#Plot the dataset
points3D(x = test$col1, y = test$col2, z = test$r)

#Fit a model. The function syntax is very similar to lm function
model_svm <- svm(r ~ col1+col2 , train)

#Use the predictions on the data
pred <- predict(model_svm, test)

#Plot the predictions and the plot to see our model fit
points3D(test$col1, test$col2, pred, col = "blue", pch=4, add = TRUE)

#For svm, we have to manually calculate the difference between actual values (train$y) with our predictions (pred)
error <- test$r - pred
svm_error <- sqrt(mean(error_2^2)) # 2.696281


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
best_mod_RMSE <- sqrt(mean(error_best_mod^2)) # 1.290738 

plot(svm_tune)

points3D(test$col1, test$col2, test$r,pch=16)
points3D(test$col1, test$col2, best_mod_pred, col = "blue", pch=4, add = TRUE)
