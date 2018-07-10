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
install.packages("plot3D")
install.packages("plot3Drgl")
require("plot3D")
require("plot3Drgl")
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
operation = 'api-test4D/'
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

#getGridData2D <- function(startx, endx, starty, endy, interval){
#  dx <- seq(startx,endx,(endx-startx)/interval);
#  dy <- seq(starty, endy, (endy-starty)/interval);
#    g <- expand.grid(dx,dy);
#  
#  colnames(g) <- paste("col", 1:2, sep = "")
#  return(g);
#}

getGridData4D <- function(startx1, endx1, startx2, endx2, startx3, endx3, startx4, endx4, interval){
  dx1 <- seq(startx1,endx1,(endx1-startx1)/interval);
  dx2 <- seq(startx2, endx2, (endx2-startx2)/interval);
  dx3 <- seq(startx3,endx3,(endx3-startx3)/interval);
  dx4 <- seq(startx4,endx4,(endx4-startx4)/interval);
  g <- expand.grid(dx1,dx2,dx3,dx4);
  
  colnames(g) <- paste("col", 1:4, sep = "")
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

#neuralNetwork2D <- function(data){
#  set.seed(2);
#  NN = neuralnet(r ~ col1 + col2, data, hidden = c(5,5,5), linear.output= T, stepmax = 1e+6);
#  return(NN);
#}

neuralNetwork4D <- function(data){
  set.seed(2);
  NN = neuralnet(r ~ col1 + col2 + col3 + col4, data, hidden = c(4,4,4,4,4), linear.output= T, stepmax = 1e+6);
  return(NN);
}

#plots predicted values vs test values and outputs rmse
#predictNN <- function(NN, testData, data){
#  predict_testNN = compute(NN, testData[,c(1:(ncol(data)-1))], rep = 1)
#  predict_testNN = (predict_testNN$net.result * (max(data[,'r']) - min(data[,'r']))) + min(data[,'r'])
#  plot(testData[,'r'], predict_testNN, col='blue', pch=16, ylab = "predicted r NN", xlab = "real r")
#  abline(0,1)
#  return((sum((testData[,'r'] - predict_testNN)^2) / nrow(testData)) ^ 0.5)
#}


#Makes a prediction based on the Neural network provided and returns a data frame consisting of input data and calculated outputs
getPredictionDataFrame_NN = function(model, data){
  pred = compute(model, data, rep = 1)$net.result
  return(data.frame("col1" = data$col1, "col2" = data$col2, "r" = pred))
}


#predictNNWOTEST2D <-function(NN, data){
#  predict_testNN = compute(NN, data, rep = 1)
#  return (data.frame("col1" = data$col1, "col2" = data$col2, "r" = predict_testNN$net.result))
#}

predictNNWOTEST4D <-function(NN, data){
  predict_testNN = compute(NN, data, rep = 1)
  return (data.frame("col1" = data$col1, "col2" = data$col2, "col3" = data$col3, "col4" = data$col4, "r" = predict_testNN$net.result))
}

#############################################
## Support Vector Machine ###################
#############################################

createSVMModel = function(testData){
  return(model_svm <- svm(r ~ col1+col2 , testData))
}

#getPredictionDataFrame = function(model, data){
#  pred = predict(model, data)
#  return(data.frame("col1" = data$col1, "col2" = data$col2, "r" = pred))
#}

getPredictionDataFrame4D = function(model, data){
  pred = predict(model, data)
  return(data.frame("col1" = data$col1, "col2" = data$col2, "col3" = data$col3, "col4" = data$col4, "r" = pred))
}

#### Optimization Algorithms####

#Function to be optimized, receives currently four inputs returns the value from the neural network
#If you want to use a different neural network, please exchange NN with the corresponding variable
fun_NN_4D = function(x1, x2, x3, x4){
  t = data.frame("col1"= x1, "col2"=x2, "col3"= x3, "col4"=x4)
  return(compute(NN, t, rep = 1)$net.result)
}

fun_SVM_4D = function(x1, x2, x3, x4){
  t = data.frame("col1"= x1, "col2"=x2, "col3"= x3, "col4"=x4)
  return(1 - predict(svm_model, t))
}

###########################################################################
##################### Finding the minimum #################################
###########################################################################
findMin <- function(interval, limit, dimension){
  startx1 = 0
  endx1 = 1
  startx2 = 0
  endx2 = 1
  startx3 = 0
  endx3 = 1
  startx4 = 0
  endx4 = 1
  left = limit - (interval+1^dimension)
  k = 0.1
  while (left > 0){
    dataset = getData(getGridData4D(startx1, endx1, startx2, endx2, startx3, endx3, startx4, endx4, interval),token)
    NN <- neuralNetwork4D(dataset)
    predicted <- predictNNWOTEST4D(NN, getGridData4D(startx1, endx1, startx2, endx2, startx3, endx3, startx4, endx4, interval))
    min <- predicted[which(predicted[,5] == min(predicted[,5])),]
    if(min$col1-k > 0){
      startx1 = min$col1-k
    }else{
      startx1 = 0
    }
    if(min$col1+k < 1){
      endx1 = min$col1+k
    }else{
      endx1 = 1
    }
    if(min$col1-k > 0){
      startx2 = min$col1-k
    }else{
      startx2 = 0
    }
    if(min$col1+k < 1){
      endx2 = min$col1+k
    }else{
      endx2 = 1
    }
    if(min$col1-k > 0){
      startx3 = min$col1-k
    }else{
      startx3 = 0
    }
    if(min$col1+k < 1){
      endx3 = min$col1+k
    }else{
      endx3 = 1
    }
    if(min$col1-k > 0){
      startx4 = min$col1-k
    }else{
      startx4 = 0
    }
    if(min$col1+k < 1){
      endx4 = min$col1+k
    }else{
      endx4 = 1
    }
    k = k/10
    left = left - (interval+1^dimension)
    print(min)
  }
}

dataset = getData(getGridData4D(0,1,0,1,0,1,0,1,4),token)
scaledDataset = scalingData(dataset)
svm_tune <- tune(svm, r ~ col1+col2+col3+col4, data = scaledDataset, kernel = "radial", ranges = list(gamma = seq(0,1,0.25), epsilon = c(0,0.0001, 0.01, 0.1, 0.5, 1), cost = 2^(1:7)))
svm_tune$best.performance
svm_tune$best.parameters
svm_model = svm_tune$best.model
pred = getPredictionDataFrame4D(svm_model,getGridData4D(0,1,0,1,0,1,0,1,10))

#Identifies the lowest r value per point in the grid
merke = c()
for (i in seq(0,1,by = 0.1)) {
  for (k in seq(0,1, by = 0.1)) {
    p = pred[which(pred$col1 == i & pred$col2 == k),]
    tmp = p[order(p[,5], decreasing = FALSE),]
    merke = append(merke, which(pred$r == tmp[ order(tmp[,5], decreasing = FALSE), ][1,5]))
  } 
}

#Identifies the lowest r value per point in the grid
merke2 = c()
for (i in seq(0,1,by = 0.1)) {
  for (k in seq(0,1, by = 0.1)) {
    p = pred[which(pred$col3 == i & pred$col4 == k),]
    tmp = p[order(p[,5], decreasing = FALSE),]
    merke2 = append(merke2, which(pred$r == tmp[ order(tmp[,5], decreasing = FALSE), ][1,5]))
  } 
}

#Visualization of the data; first col1/col2 as grid, then col3/clo4 as grid
plot_ly(pred, x = pred[merke,1], y = pred[merke,2], z = pred[merke,5], text = pred[merke,3], color = pred[merke,4])

scatter3D(bty = "b2", x = pred[merke,1], xlab = "col1", y = pred[merke,2], ylab = "col2", z = pred[merke,5], zlab = "r", main = "text = col3; color = col4", cex = 1, pch = 19, theta = 10, phi = 10, colvar = pred[merke,4],ticktype = "detailed")
text3D(x= pred[merke,1], y = pred[merke,2], z = pred[merke,5],  labels = round(pred[merke,3],2),add = TRUE, colkey = FALSE, cex = 1)
plotrgl()

scatter3D(bty = "b2", x = pred[merke2,3], xlab = "col3", y = pred[merke2,4], ylab = "col4", z = pred[merke2,5], zlab = "r", main = "text = col1; color = col2", cex = 1, pch = 19, theta = 10, phi = 10, colvar = pred[merke2,2],ticktype = "detailed")
text3D(x= pred[merke2,3], y = pred[merke2,4], z = pred[merke2,5],  labels = round(pred[merke2,1],2),add = TRUE, colkey = FALSE, cex = 1)
plotrgl()

#Based on the information derived from the visual representation, the GA has to be adjusted

#Implementation of the Genetic Algorithm
GA <- ga(type = "real-valued", fitness = function (x) {- fun_NN_4D(x[1],x[2],x[3],x[4])}, lower = c(0,0,0,0), upper = c(1,1,1,1), maxiter = 1000, run = 50)
GA <- ga(type = "real-valued", fitness = function (x) {fun_SVM_4D(x[1],x[2],x[3],x[4])}, lower = c(0.7,0,0.4,0), upper = c(1,0.3,0.8,1), maxiter = 1000, run = 50)

#Real value retrieved from the API
getData(data.frame("col1" = GA@solution[,1], "col2" = GA@solution[,2], "col3" = GA@solution[,3], "col4" = GA@solution[,4]),token)

#Value predicted by the respective models
NN_value = fun_NN_4D(GA@solution[,1],GA@solution[,2],GA@solution[,3],GA@solution[,4])
SVM_value = 1 - fun_SVM_4D(GA@solution[,1],GA@solution[,2],GA@solution[,3],GA@solution[,4])

#Implementation of the subplex algorithm (another optimization algorithm)
sp = c(runif(1,0,0.5),runif(1,0,1),runif(1,0,1),runif(1,0,1))
sbplx(sp,fn = function (x) {1 - fun_SVM_4D(x[1],x[2],x[3],x[4])},lower = c(0,0,0,0), upper = c(1,1,1,1))

#Visualization of the Neural network model - helps to get a better understanding of how the model looks like
#!!!!!!!!!!!!!!!!!THIS HAS TO ADJUSTED SUCH THAT THE PLOT ALWAYS FITS THE REQUESTED GRID!!!!!!!!
#plotdata = getPredictionDataFrame_NN(m,getGridData(0,1,0,1,20,2))[,3]
#dim(plotdata) <- c(length(c(seq(grid_start,grid_end,grid_interval))),length(c(seq(grid_start,grid_end,grid_interval))));
#rownames(plotdata) <-  c(seq(grid_start,grid_end,grid_interval));
#colnames(plotdata) <-  c(seq(grid_start,grid_end,grid_interval));
#plot_ly(x = rownames(plotdata), y = colnames(plotdata), z=plotdata, type="surface")

####################### End of functions ##################################

####################### Start of Executable code ##################################
#execute: data creation
dataset = getData(getGridData2D(0,1,0,1,20),token)

model_svm = createSVMModel(dataset)
predDataFrame = getPredictionDataFrame(model_svm, getGridData2D(0,1,0,1,80))
predDataFrame[which(predDataFrame[,3] == min(predDataFrame[,3])),]
error <- dataset$r - predDataFrame$r
svm_error <- sqrt(mean(error^2))


NN <- neuralNetwork(train);
predicted <- predictNNWOTEST(NN, getGridData(0,1,0,1,20,2))
error <- train$r - predicted$r

NN <- neuralNetwork(dataset);
predicted <- predictNNWOTEST(NN, getGridData2D(0,1,0,1,80))
error <- dataset$r - predicted$r

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