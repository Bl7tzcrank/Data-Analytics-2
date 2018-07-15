#This R-Script is about the application of supervised learning and optimization algorithms on a 5 dimensional problem
#with the aim to identify local and if possible the absolute minimum.
#To run it, first execute lines in the packages section to install all required packages. 
#Next set all variables and load all functions by executing the code in the sections variables and functions.
#Afterwards jump to the main part and execute it line by line.
#The comments describe what each line of code is doing.

####################### Start of packages ##################################
install.packages('httr')
install.packages('e1071')
install.packages('xml2')
install.packages('XML')
install.packages('stringr')
install.packages('neuralnet')
install.packages('rpart')
install.packages('plotly')
install.packages('GA')
install.packages('nloptr')
install.packages('plot3D')
install.packages('plot3Drgl')
install.packages('sqldf')
install.packages('randomForest');
install.packages('xgboost');
install.packages("caret")
require('randomForest');
require('xgboost');
require("caret")
require('sqldf')
require('plot3D')
require('plot3Drgl')
require('nloptr')
require('GA')
require('plotly')
require("e1071")
require('httr')
require('xml2')
require('XML')
require('stringr')
require('neuralnet')
require('rpart')
source("support_vector_machines.R")
source("Tree.R")
####################### End of packages #####################################

####################### Start of Variables ##################################
#Parameters for the connection to the production/test API
url = 'http://optim.uni-muenster.de:5000/'
operation = 'api/'
test = FALSE;
token = '5d8096530da349e98ca4cc65b519daf7'
####################### End of Variables ####################################

####################### Start of Functions ##################################

#Description: Executes the GET request to the REST API
#Input: datapoint to request and access token for verification
#Output: xml2-object
getRequest<- function(data, token){
  getUrl <- paste(url,operation,token,'/',data, sep="");
  if(grepl('test',getUrl) || (!test)){
    r <- GET(getUrl);
    return (r);
  } else{
    print('Not in production mode yet');
    return (NULL);
  }
};

#Description: Receives dataframe as input, turns it into a string, calls getRequest(), turns response object back into numeric and outputs a dataframe.
#It only accesses 50 points per GET request
#Input: datapoint(s) to request in form of a dataframe and access token for verification
#Output: dataframe containing the input points and the corresponding results from the API
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

#Description: Creates a data grid for 4 dimensional space based on the start, end interval values for each dimension. 
#Used for grid creation for value prediction
#Input: Start and end points for each dimension as well as the required interval (e.g. start: 0, end: 1, interval: 2 => 0, 0.5, 1)
#Output: Data grid for the defined space
getGridData4D <- function(startx1, endx1, startx2, endx2, startx3, endx3, startx4, endx4, interval){
  dx1 <- seq(startx1,endx1,(endx1-startx1)/interval);
  dx2 <- seq(startx2,endx2,(endx2-startx2)/interval);
  dx3 <- seq(startx3,endx3,(endx3-startx3)/interval);
  dx4 <- seq(startx4,endx4,(endx4-startx4)/interval);
  g <- expand.grid(dx1,dx2,dx3,dx4);
  colnames(g) <- paste("col", 1:4, sep = "")
  return(g);
}

#Description: Identical function as getGridData4D, but it also checks whether a data value, which shall be requested has already been requested
#             Whereas the previous function is used for creating grids for data prediction, this is used for the actual data retrieval.
#Input: Start and end points for each dimension, required interval, variable defining whether the current dataset shall be extended or replaced
#Output: Data grid for the defined space
getGridData4D_for_Wrapper <- function(startx1, endx1, startx2, endx2, startx3, endx3, startx4, endx4, interval = 3, answer){
  dx1 <- seq(startx1,endx1,(endx1-startx1)/interval);
  dx2 <- seq(startx2,endx2,(endx2-startx2)/interval);
  dx3 <- seq(startx3,endx3,(endx3-startx3)/interval);
  dx4 <- seq(startx4,endx4,(endx4-startx4)/interval);
  
  g <- expand.grid(dx1,dx2,dx3,dx4);
  colnames(g) <- paste("col", 1:4, sep = "")
  
  if(answer == "e"){
    ds <- round(dataset[,-5],2)
    g <- sqldf('SELECT * FROM g EXCEPT SELECT * FROM ds')
  }
  return(g);
}

#Description: Function used to define a data grid and retrieve the values from the API. The user has the option to extent the existing dataset
#             or create a new dataset.
#Input: Start and end points for each dimension, the interval required and a token for verification at the API
#Output: Dataset including the defined grid and the corresponding values from the API
data.get.grid <- function (startx1, endx1, startx2, endx2, startx3, endx3, startx4, endx4, interval = 3, token){
  answer = readline("Do you want to create (c) a new dataset or extent the existing one (e)?")
  grid = getGridData4D_for_Wrapper(startx1, endx1, startx2, endx2, startx3, endx3, startx4, endx4, interval, answer)
  if (nrow(grid)!=0){
    result = getData(grid, token)
    print("The following combinations have been retrieved:")
    print(result)
    if (answer == "c") {
      return (result)
    }
    else{
      return (rbind(dataset, result))
    }  
  }else{
    print("No new data requested, because all values already exist.")
    return(dataset)
  }
}

#Description: Function used to define a data point and retrieve the value from the API. The user has the option to extent the existing dataset
#             or create a new dataset.
#Input: Value of the point for each dimension and a token for verification at the API
#Output: Dataset including the defined point and the corresponding value from the API
data.get.point <- function (col1,col2,col3,col4,token){
  answer = readline("Do you want to create (c) a new dataset or extent the existing one (e)? ")
  point = data.frame("col1" = col1, "col2" = col2, "col3" = col3, "col4" = col4)
  ds <- dataset[,-5]
  if(nrow(ds)!=0){
    g <- sqldf('SELECT * FROM point EXCEPT SELECT * FROM ds')
  }else{
    g <- point
  }
  if(nrow(g)!=0){
    result = getData(point, token)
    print("The following combinations have been retrieved:")
    print(result)
    if (answer == "c") {
      return (result)
    }
    else{
      return (rbind(dataset, result))
    }
  }else{
    print("No new data requested, because all value already exist.")
    return(dataset)
  }
}

#Description: Splits the data into trainings and test data. Returns index that can be used to access training and test data from the dataset
#             percentage % will be used for training
#Input: Dataframe containing current dataset, percentage of test data
#Output: Indexes of test data in the dataset
splitData <- function(data, percentage){
  samplesize = percentage * nrow(data);
  set.seed(80);
  index = sample( seq_len ( nrow ( data ) ), size = samplesize );
  return (index);
}

#Description: Scales data to the interval of 0 to 1
#Input: Dataframe containing current dataset
#Output: Dataframe containing scaled dataset
scalingData <- function(data){
  max = apply(data , 2 , max);
  min = apply(data, 2 , min);
  scaled = as.data.frame(scale(data, center = min, scale = max - min));
  return (scaled);
}

#Description: Creates predictions for all points defined 
#Input: Model used for the prediction, coordinates used for the prediction
#Output: Values predicted by the model
getPredictionDataFrame4D = function(model, data){
  pred = predict(model, data)
  return(data.frame("col1" = data$col1, "col2" = data$col2, "col3" = data$col3, "col4" = data$col4, "r" = pred))
}

#Description: Function to be optimized by the Genetic Algorihm
#Input: Values for the four dimensions and the model used for the prediction
#Output: Value predicted by the model
fun_SVM_4D = function(x1, x2, x3, x4,int_model){
  t = data.frame("col1"= x1, "col2"=x2, "col3"= x3, "col4"=x4)
  return(1 - predict(int_model, t))
}

#Description: Creates a subset, tunes the SVM, provides predicted values for the defined granularity, prepares a visual representation and
#             searches for valleys (local and global minimums)
#Input: Start values for each dimension, the interval to the end point (e.g. 0.1) and the required prediction interval (e.g. 20 steps)
#Output: The subset used for the model creation, the computed model, the coordinates of the identified valleys, additional information about 
#        the SVM calculation and the predicted lowest value for each col 1 / col 2 combination used for the visual representation
subset_model_visualization <- function(col1_start,col2_start,col3_start,col4_start,interval,pred_interval){
  subset_valley = subset.data.frame(dataset,dataset$col1>=col1_start&dataset$col1<=(col1_start+interval)&dataset$col2>=col2_start&dataset$col2<=(col2_start+interval)&dataset$col3>=col3_start&dataset$col3<=(col3_start+interval)&dataset$col4>=col4_start&dataset$col4<=(col4_start+interval))  
  print("Subset created")
  
  #Tune SVM
  print("Model tuning starts")
  svm_tune <- tune(svm, r ~ col1+col2+col3+col4, data = subset_valley, kernel = "radial", ranges = list(gamma = seq(0,1,0.25), epsilon = c(0,0.0001, 0.01, 0.1, 0.5, 1), cost = 2^(1:7)))
  svm_mse = svm_tune$best.performance
  model = svm_tune$best.model
  print("Model selected")
  
  #Create visualization
  print("Visualization is being created")
  pred = getPredictionDataFrame4D(model,getGridData4D(col1_start,col1_start+interval,col2_start,col2_start+interval,col3_start,col3_start+interval,col4_start,col4_start+interval,pred_interval))
  merke = c()
  for (i in seq(col1_start,col1_start+interval,by = interval/pred_interval)) {
    for (k in seq(col2_start,col2_start+interval, by = interval/pred_interval)) {
      p = pred[which(pred$col1 == i & pred$col2 == k),]
      tmp = p[order(p[,5], decreasing = FALSE),]
      merke = append(merke, which(pred$r == tmp[ order(tmp[,5], decreasing = FALSE), ][1,5] ))
    } 
  }
  scatter3D(bty = "b2", x = pred[merke,1], xlab = "col1", y = pred[merke,2], ylab = "col2", z = pred[merke,5], zlab = "r", main = "text = col3; color = col4", cex = 1, pch = 19, theta = 10, phi = 10, colvar = pred[merke,4],ticktype = "detailed")
  text3D(x= pred[merke,1], y = pred[merke,2], z = pred[merke,5],  labels = round(pred[merke,3],2),add = TRUE, colkey = FALSE, cex = 1)
  plotrgl()
  print("Visualization created")
  
  print("Search valleys")
  valleys = data.frame("col1"=c() , "col2"=c() , "col3"=c() , "col4"=c() ,"value"=c())
  t = ((col1_start+interval)-col1_start)/10
  for(i in seq(col1_start,col1_start+interval-t,t)){
    for(l in seq(col2_start,col2_start+interval-t,t)){
      sp = c(runif(1,i,i+t),runif(1,l,l+t),runif(1,col3_start,col3_start+interval),runif(1,col4_start,col4_start+interval))
      valley = sbplx(sp,fn = function (x) {1 - fun_SVM_4D(x[1],x[2],x[3],x[4],model)},lower = c(col1_start,col2_start,col3_start,col4_start), upper = c(col1_start+interval,col2_start+interval,col3_start+interval,col4_start+interval))
      valleys = rbind(valleys, data.frame("col1"= valley$par[1] , "col2"= valley$par[2] , "col3"= valley$par[3] , "col4"= valley$par[4] ,"value"= valley$value))
    }
  }
  valleys <- valleys[-which(duplicated(round(valleys,2))),]
  print("Valleys found")
  print("Done")
  return (list("subset" = subset_valley, 
               "model" = model,"valleys" = valleys, "best_tune"=svm_tune , 
               "pred" = pred[merke,]))
}

####################### End of functions ##################################

####################### Start of Executable code ##################################


####################### Start of TEST API CODE ##################################
#Retrieve initial grid
dataset.test = data.get.grid(0,1,0,1,0,1,0,1,3,token)

#Method comparison based on initial dataset
#SVM
#3*var(dataset[,5])
#epsilon
var(dataset[,5])

svm_tune <- tune(svm, r ~ col1+col2+col3+col4, data = dataset.test, kernel = "radial", ranges = list(gamma = seq(0,1,0.25), epsilon = c(0,0.0001, 0.01, 0.1, 0.5, 1), cost = 2^(1:7)))
svm_mse = svm_tune$best.performance

#Neural Networks
#Iterative Approach because of computational costs

#tuningGrid = expand.grid("layer1"=c(6:8),"layer2"=c(6:8),"layer3"=(1:3))
#tuningGrid = expand.grid("layer1"=c(6:8),"layer2"=c(6:8),"layer3"=(3:5))
tuningGrid = expand.grid("layer1"=c(6:8),"layer2"=c(6:8),"layer3"=(6:8))
#testNN2 = train(r~col1+col2+col3+col4,data = dataset, method = "neuralnet", preProcess = "scale", metric = "RMSE", tuneGrid = tuningGrid,rep = 1,threshold = 0.01)
#testNN3 = train(r~col1+col2+col3+col4,data = dataset, method = "neuralnet", preProcess = "scale", metric = "RMSE", tuneGrid = tuningGrid,rep = 1,threshold = 0.01)
testNN4 = train(r~col1+col2+col3+col4,data = dataset, method = "neuralnet", preProcess = "scale", metric = "RMSE", tuneGrid = tuningGrid,rep = 1,threshold = 0.01)
test_NN_results = rbind(testNN2$results, testNN3$results, testNN4$results)

#Random Forests
fitControl = trainControl(
  method = "repeatedcv", 
  number = 10,
  repeats = 10)

newrf = train (r ~ col1 + col2 + col3 + col4, data = dataset.test, method = "rf", trControl = fitControl)
best_rf=newrf$finalModel
best_rf

#xgboost - to be completed by Julian

index <- splitData(dataset, 0.8);
train <- (dataset[index,]);
test <- (dataset[-index,]);

bst <-xgboost(data = as.matrix(train[,-5]), label = as.matrix(train[,5]), 
              max_depth = 15, eta = 1, nthread = 1, nrounds = 30, booster = "gbtree",
              objective = "reg:linear")

cv <- xgb.cv(data = as.matrix(dataset),label =as.matrix(dataset[,5]) , nrounds = 20, nthread = 1, nfold = 10, metrics = "rmse",
             max_depth = 12, eta = 0.5, objective = "reg:linear", prediction = TRUE, callbacks = list(cb.cv.predict(save_models = TRUE)))

pred <- predict(bst, as.matrix(test[,-5]))

pred <- predict(bst, as.matrix(getGridData4D(0,1,0,1,0,1,0,1,10)))

a = cbind(getGridData4D(0,1,0,1,0,1,0,1,10),data.frame("r" = unlist(pred)))

cv <- xgb.cv(data = as.matrix(dataset),label = as.matrix(dataset.test[,5]) , nrounds = 20, nthread = 1, nfold = 10, metrics = "rmse",
             max_depth = 12, eta = 0.5, objective = "reg:linear", prediction = TRUE, callbacks = list(cb.cv.predict(save_models = TRUE)))



####################### End of TEST API CODE ##################################


####################### Start of PROD API CODE ##################################
#Retrieve initial grid and create initial model
dataset = data.get.grid(0,1,0,1,0,1,0,1,3,token)

model = subset_model_visualization(0,0,0,0,1,20)

#Another verification of the methods for production data
#SVM

#Neural network

#Random Forests

#xgboost

#The model identified 4 different valleys, for each a grid of interval 2 and range 0.1 was defined
#The values for the grids were retrieved and further analysis was conducted on each valley

#valley1
dataset = data.get.grid(0,0.1,0.75,0.85,0,0.1,0.69,0.79,2,token)
model_valley1 = subset_model_visualization(0,0.8,0,0.6,0.1,20)

#valley2
dataset = data.get.grid(0.29,0.39,0.21,0.31,0.36,0.46,0.03,0.13,2,token)
model_valley2 = subset_model_visualization(0.29,0.21,0.36,0.03,0.1,20)

#valley3
dataset = data.get.grid(0.54,0.64,0.65,0.75,0.3,0.4,0.65,0.75,2,token)
model_valley3 = subset_model_visualization(0.55,0.50,0.3,0.65,0.1,20)

#Valley4
dataset = data.get.grid(0.9,1,0.1,0.2,0.9,1,0.6,0.7,2,token)
model_valley4 = subset_model_visualization(0.8,0.0,0.8,0.6,0.1,20)


#Because of the small values of valley 1 and 4 we decided focus on those first and to retrieve the lowest point identified by the genetic algorithm
#Minima of valley 1
ga(type = "real-valued", fitness = function (x) {fun_SVM_4D(x[1],x[2],x[3],x[4],model_valley1$model)}, lower = c(0,0.75,0,0.69), upper = c(0.1,0.85,0.1,0.79), maxiter = 1000, run = 50)
dataset = data.get.point(0.0492024897, 0.8481126516, 0.08903008945, 0.7310247013,token)
#Minima of valley 4
ga(type = "real-valued", fitness = function (x) {fun_SVM_4D(x[1],x[2],x[3],x[4],model_valley1$model)}, lower = c(0.54,0.65,0.3,0.65), upper = c(0.64,0.75,0.4,0.75), maxiter = 1000, run = 50)
dataset = data.get.point(0.9180070354, 0.1055761687, 0.9563065526,  0.7, token)

#After rather disappointing results we furthermore retrieved data for the two best minimums of the entire model
#the values were extracted from an updated version of the initial model
model_1it = subset_model_visualization(0,0,0,0,1,20)
model_1it$valleys
dataset = data.get.point(0.04976414, 0.88142666, 0.1285623, 0.7764975,token)
dataset = data.get.point(0.87770071, 0.06131477, 0.9563864, 0.7592108,token)

#After poor results we focused on the analysis of the visual representation of valley 1
#For valley 1 we retrieved one value, which was outside the retrieved grid in order to get a better understanding of the model
dataset = data.get.point(0.05, 0.87, 0.09, 0.74,token)

#After having a look at the updated visualization we thought that further improvement of valley 1 is unrealistic
#Still we retrieved the value for the predicted minimum of an updated version of model_valley1 to verify our assumption
model_valley1_1it = subset_model_visualization(0,0.8,0,0.6,0.2,20)
ga(type = "real-valued", fitness = function (x) {fun_SVM_4D(x[1],x[2],x[3],x[4],model_valley1_1it$model)}, lower = c(0,0.8,0,0.6), upper = c(0.2,1,0.2,0.8), maxiter = 1000, run = 50)
dataset = data.get.point(0.0502497, 0.8660159, 0.09680179, 0.7618185,token)

#The results were not improving, therefore we decided to switch to valley 4
#In order to further explore valley 4 we decided to zoom in by retrieving data for a more granular defined grid
dataset = data.get.grid(0.86,0.96,0.05,0.15,0.9,1,0.7,0.8,2,token)

#Because of the retrieved data we concluded that no further improvement was possible and continued to valley 3
#Based on the global optimum for valley 3 and visual exploration we assumed that col1 and col2 were not optimal for finding the minimum of valley 3
#therefore we retrieved the point for the predicted local optimum
model_2it = subset_model_visualization(0,0,0,0,1,20)
model_2it$valleys
dataset = data.get.point(0.71638473, 0.53252414, 0.3979821, 0.7525453,token)
model_valley3_2it = subset_model_visualization(0.67,0.51,0.37,0.73,0.04,20)

#Because of promising results of the valley 3 optima in the next step we created a grid with interval 0.02 around the new minimum

dataset = data.get.grid(0.69,0.73,0.51,0.55,0.37,0.41,0.73,0.77,2,token)

#The results slightly improved
#Because of a visual exploration we decided to extent the landscape in one direction to check for further minimums
model_valley3_3it = subset_model_visualization(0.67,0.51,0.37,0.73,0.04,20)
dataset = data.get.grid(0.67,0.71,0.51,0.55,0.37,0.41,0.73,0.77,2,token)

#Based on new data a new optimum for valley 3 has been calculated and the value has been retrieved
model_valley3_4it = subset_model_visualization(0.67,0.51,0.37,0.73,0.04,20)
dataset = data.get.point(0.6789297, 0.5325574, 0.3808499, 0.7414746,token)

#Further zoom into valley3 with interval  0.005
dataset = data.get.grid(0.673,0.683,0.527,0.537,0.375,0.385,0.736,0.746,2,token)

#Because of no further improvement we turned to valley 2

#We retrieved a value for valley 2 predicted minimum based on the prediction based on the entire dataset
model_3it = subset_model_visualization(0,0,0,0,1,20)
model_3it$valleys
dataset = data.get.point(0.24269418, 0.1991316983, 0.4046623, 0.006489358,token)

#Because of no improvement we tested two new valleys, which appeared in the meantime due to retrieved data
dataset = data.get.grid(0.6,0.7,0.9,1,0.25,0.35,0.58,0.68,2,token)
dataset = data.get.point(0.29820547, 0.9110710325, 0.6924902, 0.642203880,token)

#Retrieved values were not promising and therefore no further time was spend on the valleys


#Because no valley seemed to be promising we tried to identify potential valleys by plotting the actual dataset
#and look for promising values, not adequately represented in the model
#The last points were retrieved with a combination of visual exploration and valley prediction
plot_ly(x = dataset[,1],y = dataset[,2],z = dataset[,5],color = dataset[,3], text = dataset[,4])
plot_ly(x = dataset[,3],y = dataset[,4],z = dataset[,5],color = dataset[,1], text = dataset[,2])

dataset = data.get.point(0.7, 0.4, 0.3, 0.5,token)
dataset = data.get.point(0.7, 0.4, 0.3, 0.2,token)
dataset = data.get.point(0.6790337, 0.5318569, 0.3818663, 0.7426143,token)
dataset = data.get.point(0.6544189, 0.50442716, 0.3784510, 0.7440944,token)
dataset = data.get.point(0.2397695, 0.21076565, 0.3999436, 0.0000000,token)
dataset = data.get.grid(0.25,0.35,0.85,0.95,0.7,0.8,0.5,0.6,1,token)
dataset = data.get.grid(0.25,0.35,0.2,0.4,0.5,0.6,0,0.1,1,token)
dataset = data.get.grid(0.25,0.35,0.8,0.9,0.5,0.7,0.25,0.35,1,token)
dataset = data.get.point(0.451733713, 0.45038490, 1.0000000, 0.735419410,token)
dataset = data.get.point(0.654678013, 0.50326664, 0.3785781, 0.743538371,token)
dataset = data.get.point(0.25, 0, 0.66, 0.1,token)
dataset = data.get.point(0.25, 0.1, 0.66, 0.1,token)
dataset = data.get.point(0.15, 0, 0.66, 0.1,token)
dataset = data.get.point(0.2, 0, 0.66, 0.1,token)
dataset = data.get.point(0.25, 0, 0.5, 0.05,token)

#Store final dataset
write.csv(dataset, file = "final.csv")

####################### End of PROD API CODE ##################################