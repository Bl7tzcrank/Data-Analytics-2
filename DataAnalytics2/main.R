####################### Start of packages ##################################
install.packages('httr');
install.packages("e1071")
install.packages('xml2');
install.packages('XML');
install.packages('stringr');
install.packages('neuralnet');
install.packages("rpart");
install.packages("plotly");
install.packages("GA");
install.packages("nloptr")
require("nloptr")
require("GA");
require('plotly');
require("e1071")
require('httr');
require('xml2');
require('XML');
require('stringr');
require('neuralnet');
require('rpart');
source("support_vector_machines.R")
####################### End of packages ##################################

####################### Start of Variables ##################################
url = 'http://optim.uni-muenster.de:5000/';
operation = 'api-test2D/'
test = TRUE;
#token
token = '5d8096530da349e98ca4cc65b519daf7';

grid_start = 0;
grid_end = 1;
grid_interval = (grid_end - grid_start)/20;
grid_dimensions = 2;

####################### End of Variables ##################################


####################### Start of Functions ##################################
#Executes the GET request. Outputs an xml2-object.
getRequest<- function(data, token){
  #token <- readline(prompt="Token: ")
  getUrl <- paste(url,operation,token,'/',data, sep="");
  if(grepl('test',getUrl) || (!test)){
    r <- GET(getUrl);
    return (r);
  } else{
    print('Not in production mode yet');
    return (NULL);
  }
};

#Receives dataframe as input, turns it into a string, calls getRequest(), turns response object back into numeric and outputs a dataframe.
#It only accesses 50 points per GET request
#remark: run print(getData(data, token), digits=20) to see that the result is NOT rounded
getData<- function(data, token){
  
  for(x in 1:floor(((nrow(data)-1)/50)+1)){
    if(x == floor(((nrow(data)-1)/50)+1)){
      until = nrow(data);
    }else{
      until = x*50;
    }
    datachunk = data[(((x-1)*50)+1):until,]
    dataAsString <- '';
    for(row in 1:nrow(datachunk)){
      for(col in 1:ncol(datachunk)){
        dataAsString <- paste(dataAsString, as.character(datachunk[row,col]), sep='');
        if(col < ncol(datachunk)){
          dataAsString <- paste(dataAsString, ",", sep='');
        }
      }
      if(row < nrow(datachunk)){
        dataAsString <- paste(dataAsString, ";", sep='');
      }
    }
    request <- getRequest(dataAsString, token);
    html <- content(request, "text");
    substring <- sub(".*\\[", "", html);
    substring <- sub("\\].*", "", substring);
    substring <- str_extract_all(substring, "[0-9].[0-9]*e?-?[0-9]*", simplify = TRUE)
    if(x==1){
      result = cbind(datachunk,r=as.numeric(substring));
    }else{
      result = rbind(result,(cbind(datachunk,r=as.numeric(substring))))
    } 
  }
  return(result);
}

#getRandomData <- function(n, dimensions){
#  for (i in 1:dimensions){
#    if(i>1){
#        c <- cbind(c,runif(n, min=0, max=1));
#    }
#    else 
#    {
#      c <- (runif(n, min=0, max=1));
#    }
#  }
#  colnames(c) <- paste("col", 1:dimensions, sep = "")
#  return (c);
#}

#returns the grid for API requests
getGridData <- function(startx, endx, starty, endy, interval, dimensions){
  dx <- seq(startx,endx,(endx-startx)/interval);
  dy <- seq(starty, endy, (endy-starty)/interval);
  if(dimensions == 2){
    g <- expand.grid(dx,dy);
  }
  if(dimensions == 4){
    g <- expand.grid(d,d,d,d);
  }
  colnames(g) <- paste("col", 1:dimensions, sep = "")
  return(g);
}

#splits the data into trainings and test data. Returns index that can be used to access training and test data from the dataset
#percentage % will be used for training
splitData <- function(data, percentage){
  samplesize = percentage * nrow(data);
  set.seed(80);
  index = sample( seq_len ( nrow ( data ) ), size = samplesize );
  return (index);
}

#Scales the data accordingly
scalingData <- function(data){
  max = apply(data , 2 , max);
  min = apply(data, 2 , min);
  scaled = as.data.frame(scale(data, center = min, scale = max - min));
  return (scaled);
}

######################################
## NEURAL NETWORKS ###################
######################################

neuralNetwork <- function(data){
  set.seed(2);
  NN = neuralnet(r ~ col1 + col2, data, hidden = c(5,5,5), linear.output= T, stepmax = 1e+6);
  return(NN);
}

#plots predicted values vs test values and outputs rmse
predictNN <- function(NN, testData, data){
  predict_testNN = compute(NN, testData[,c(1:(ncol(data)-1))], rep = 1)
  predict_testNN = (predict_testNN$net.result * (max(data[,'r']) - min(data[,'r']))) + min(data[,'r'])
  plot(testData[,'r'], predict_testNN, col='blue', pch=16, ylab = "predicted r NN", xlab = "real r")
  abline(0,1)
  return((sum((testData[,'r'] - predict_testNN)^2) / nrow(testData)) ^ 0.5)
}

#Makes a prediction based on the Neural network provided and returns a data frame consisting of input data and calculated outputs
getPredictionDataFrame_NN = function(model, data){
  pred = compute(model, data, rep = 1)$net.result
  return(data.frame("col1" = data$col1, "col2" = data$col2, "r" = pred))
}

#############################################
## Support Vector Machine ###################
#############################################

createSVMModel = function(testData){
  return(model_svm <- svm(r ~ col1+col2 , testData))
}

getPredictionDataFrame = function(model, data){
  pred = predict(model, data)
  return(data.frame("col1" = data$col1, "col2" = data$col2, "r" = pred))
}

error <- dataset$r - pred
svm_error <- sqrt(mean(error^2))

# perform a grid search
svm_tune <- tune(model_svm, r ~ col1+col2, data = dataset,
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

#### Optimization Algorithms####

#Function to be optimized, receives currently two inputs returns the value from the neural network
#If you want to use a different neural network, please exchange the m with the corresponding variable
fun_NN = function(x1, x2){
  t = data.frame("x"= x1, "y"=x2)
  return(compute(m, t, rep = 1)$net.result)
}

###########################################################################
##################### Finding the minimum #################################
###########################################################################
dataset = getData(getGridData(0,1,0,1,20,2),token)
dataset <- scalingData(dataset)
NN <- neuralNetwork(dataset);
predicted <- predictNNWOTEST(NN, getGridData(0,1,0,1,80,2))
predicted[which(predicted[,3] == min(predicted[,3])),]


#Implementation of the Genetic Algorithm
GA <- ga(type = "real-valued", fitness = function (x) 1-fun_NN(x[1],x[2]), lower = c(0,0), upper = c(1,1))
summary(GA)
plot(GA)

#Implementation of the subplex algorithm (another optimization algorithm)
sp = c(runif(1,0,1),runif(1,0,1))
sbplx(sp,fn = function (x) fun_NN(x[1],x[2]),lower = c(0,0), upper = c(1,1))

#Visualization of the Neural network model - helps to get a better understanding of how the model looks like
#!!!!!!!!!!!!!!!!!THIS HAS TO ADJUSTED SUCH THAT THE PLOT ALWAYS FITS THE REQUESTED GRID!!!!!!!!
plotdata = getPredictionDataFrame_NN(m,getGridData(0,1,0,1,20,2))[,3]
dim(plotdata) <- c(length(c(seq(grid_start,grid_end,grid_interval))),length(c(seq(grid_start,grid_end,grid_interval))));
rownames(plotdata) <-  c(seq(grid_start,grid_end,grid_interval));
colnames(plotdata) <-  c(seq(grid_start,grid_end,grid_interval));
plot_ly(x = rownames(plotdata), y = colnames(plotdata), z=plotdata, type="surface")

####################### End of functions ##################################

####################### Start of Executable code ##################################
#execute: data creation
dataset = getData(getGridData(0,1,0,1,20,2),token)

model_svm = createSVMModel(dataset)
predDataFrame = getPredictionDataFrame(model_svm, getGridData(0,1,0,1,80,2))
predDataFrame[which(predDataFrame[,3] == min(predDataFrame[,3])),]
error <- dataset$r - predDataFrame$r
svm_error <- sqrt(mean(error^2))

NN <- neuralNetwork(train);
predicted <- predictNNWOTEST(NN, getGridData(0,1,0,1,20,2))
error <- train$r - predicted$r
nn_error <- sqrt(mean(error^2))

index <- splitData(dataset, 0.67);
train <- scalingData(dataset[index,]);
test <- scalingData(dataset[-index,]);
plotdata <- dataset[,ncol(dataset)];

#execute: 3D visual data exploration
dim(plotdata) <- c(length(c(seq(grid_start,grid_end,grid_interval))),length(c(seq(grid_start,grid_end,grid_interval))));
rownames(plotdata) <-  c(seq(grid_start,grid_end,grid_interval));
colnames(plotdata) <-  c(seq(grid_start,grid_end,grid_interval));
plot_ly(x = colnames(plotdata), y = rownames(plotdata), z=plotdata, type="surface")

#execute: approximations
NN <- neuralNetwork(train);
predicted <- predictNNWOTEST(NN, train)
plot(NN)
predictNN(NN, test, dataset)

predict_testNN = compute(NN, test[,-3], rep = 1)

error <- test$r - predict_testNN$net.result
nn_error <- sqrt(mean(error^2))



####################### End of executable code ##################################