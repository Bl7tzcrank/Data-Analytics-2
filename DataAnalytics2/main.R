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
install.packages("sqldf")
require("sqldf")
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
operation = 'api/'
test = FALSE;
#token
token = '5d8096530da349e98ca4cc65b519daf7';

dataset = data.frame("col1"=NULL, "col2"=NULL, "col3"=NULL, "col4"=NULL, "r"=NULL)

#grid_start = 0;
#grid_end = 1;
#grid_interval = (grid_end - grid_start)/20;
#grid_dimensions = 2;

#valley1
#dataset = data.get.grid(0,0.1,0.75,0.85,0,0.1,0.69,0.79,2,token)
#write.csv(dataset, file = "prod_2it1.csv")
#valley2
#dataset = data.get.grid(0.29,0.39,0.21,0.31,0.36,0.46,0.03,0.13,2,token)
#write.csv(dataset, file = "prod_2it2.csv")
#valley3
#dataset = data.get.grid(0.54,0.64,0.65,0.75,0.3,0.4,0.65,0.75,2,token)
#write.csv(dataset, file = "prod_2it3.csv")
#Valley4
#dataset = data.get.grid(0.9,1,0.1,0.2,0.9,1,0.6,0.7,2,token)
#write.csv(dataset, file = "prod_2it4.csv")
#Minima of valley 1
#dataset = data.get.point(0.0492024897, 0.8481126516, 0.08903008945, 0.7310247013,token)
#Minima of valley 4
#dataset = data.get.point(0.9180070354, 0.1055761687, 0.9563065526,  0.7, token)
#write.csv(dataset, file = "prod_2it5.csv")
#Top2 Global Minima
#dataset = data.get.point(0.04976414, 0.88142666, 0.1285623, 0.7764975,token)
#dataset = data.get.point(0.87770071, 0.06131477, 0.9563864, 0.7592108,token)
#write.csv(dataset, file = "prod_2it6.csv")
#Two valley 1 assumptions
#dataset = data.get.point(0.05, 0.87, 0.74, 0.09,token)
#write.csv(dataset, file = "prod_2it7.csv")
#dataset = data.get.point(0.05, 0.87, 0.09, 0.74,token)
#write.csv(dataset, file = "prod_2it8.csv")
#Minima of valley 4 - 2nd Zoom
#dataset = data.get.grid(0.86,0.96,0.05,0.15,0.9,1,0.7,0.8,2,token)
#write.csv(dataset, file = "prod_3it1.csv")
#Valley 1 verification - no further optimzation required
#dataset = data.get.point(0.0502497, 0.8660159, 0.09680179, 0.7618185,token)
#write.csv(dataset, file = "prod_3it2.csv")
#insight: no further analysis of valley 1 and 4 required
#Based on global optima and visual exploration we assume that col1 and col2 were not optimal for finding the minimum of valley 3
#therefore we retrieve the point for the predicted local optimum
#dataset = data.get.point(0.71638473, 0.53252414, 0.3979821, 0.7525453,token)
#write.csv(dataset, file = "prod_3it3.csv")
#results of the valley 3 optima were very good
#next step - create a grid - with interval 0.02
#dataset = data.get.grid(0.69,0.73,0.51,0.55,0.37,0.41,0.73,0.77,2,token)
#write.csv(dataset, file = "prod_3it4.csv")
#results further improved - thats cool
#Extend the landscape in on direction to check for further minimas
#dataset = data.get.grid(0.67,0.71,0.51,0.55,0.37,0.41,0.73,0.77,2,token)
#write.csv(dataset, file = "prod_3it5.csv")
#based on new data new optimum for valley 3 has been calculated
#next step retrieve the value
#dataset = data.get.point(0.6789297, 0.5325574, 0.3808499, 0.7414746,token)
#write.csv(dataset, file = "prod_3it6.csv")
#retrieve first value for valley 2 predicted optima based on entire model
#dataset = data.get.point(0.24269418, 0.1991316983, 0.4046623, 0.006489358,token)
#write.csv(dataset, file = "prod_3it7.csv")
#Test new valley (yellow one)
#dataset = data.get.grid(0.6,0.7,0.9,1,0.25,0.35,0.58,0.68,2,token)
#write.csv(dataset, file = "prod_4it1.csv")
#values are rather bad
#Test new valley (with only one point)
#dataset = data.get.point(0.29820547, 0.9110710325, 0.6924902, 0.642203880,token)
#write.csv(dataset, file = "prod_4it2.csv")
#dive deeper into existing valleys: valley 3 - 0.005
#dataset = data.get.grid(0.673,0.683,0.527,0.537,0.375,0.385,0.736,0.746,2,token)

#write.csv(dataset, file = "prod_4it3.csv")


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

getRandomData <- function(n, dimensions){
  for (i in 1:dimensions){
    if(i>1){
        c <- cbind(c,runif(n, min=0, max=1));
    }
    else 
    {
      c <- (runif(n, min=0, max=1));
    }
  }
  colnames(c) <- paste("col", 1:dimensions, sep = "")
  return (c);
}

getGridData4D <- function(startx1, endx1, startx2, endx2, startx3, endx3, startx4, endx4, interval){
  dx1 <- seq(startx1,endx1,(endx1-startx1)/interval);
  dx2 <- seq(startx2,endx2,(endx2-startx2)/interval);
  dx3 <- seq(startx3,endx3,(endx3-startx3)/interval);
  dx4 <- seq(startx4,endx4,(endx4-startx4)/interval);
  g <- expand.grid(dx1,dx2,dx3,dx4);
  colnames(g) <- paste("col", 1:4, sep = "")
  return(g);
}

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

neuralNetwork4D <- function(data){
  set.seed(2);
  NN = neuralnet(r ~ col1 + col2 + col3 + col4, data, hidden = rep(c(100),100), linear.output= T, stepmax = 1e+6);
  return(NN);
}

#Makes a prediction based on the Neural network provided and returns a data frame consisting of input data and calculated outputs
getPredictionDataFrame_NN = function(model, data){
  pred = compute(model, data, rep = 1)$net.result
  return(data.frame("col1" = data$col1, "col2" = data$col2, "r" = pred))
}

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

fun_SVM_4D = function(x1, x2, x3, x4,int_model){
  t = data.frame("col1"= x1, "col2"=x2, "col3"= x3, "col4"=x4)
  return(1 - predict(int_model, t))
}

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
  #merke = c()
  #for (i in seq(col1_start,col1_start+interval,by = interval/pred_interval)) {
  #  for (k in seq(col2_start,col2_start+interval, by = interval/pred_interval)) {
  #    p = pred[which(pred$col1 == i & pred$col2 == k),]
  #    tmp = p[order(p[,5], decreasing = FALSE),]
  #    merke = append(merke, which(pred$r == tmp[ order(tmp[,5], decreasing = FALSE), ][1,5] ))
  #  } 
  #}
  
  scatter3D(bty = "b2", x = pred[,3], xlab = "col1", y = pred[,4], ylab = "col2", z = pred[,5], zlab = "r", main = "text = col3; color = col4", cex = 1, pch = 19, theta = 10, phi = 10, colvar = pred[,1],ticktype = "detailed")
  text3D(x= pred[,3], y = pred[,4], z = pred[,5],  labels = round(pred[,2],2),add = TRUE, colkey = FALSE, cex = 1)
  plotrgl()
  #scatter3D(bty = "b2", x = pred[merke,1], xlab = "col1", y = pred[merke,2], ylab = "col2", z = pred[merke,5], zlab = "r", main = "text = col3; color = col4", cex = 1, pch = 19, theta = 10, phi = 10, colvar = pred[merke,4],ticktype = "detailed")
  #text3D(x= pred[merke,1], y = pred[merke,2], z = pred[merke,5],  labels = round(pred[merke,3],2),add = TRUE, colkey = FALSE, cex = 1)
  #plotrgl()
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
               "pred" = pred))
}
#Entire model
model = subset_model_visualization(0,0,0,0,1,20)

#Valley1
model = subset_model_visualization(0,0.8,0,0.6,0.2,20)

#Valley2
model = subset_model_visualization(0.29,0.21,0.36,0.03,0.1,20)

#Valley3
model = subset_model_visualization(0.54,0.65,0.3,0.65,0.1,20)

#Valley4
model = subset_model_visualization(0.8,0.0,0.8,0.6,0.2,20)

#valley 3 adjusted
model = subset_model_visualization(0.67,0.51,0.37,0.73,0.04,20)

model$best_tune$best.performance

model$valleys

plot(dataset[,c(1,2)])
plot_ly(x = dataset[,1],y = dataset[,2],z = dataset[,5],color = dataset[,5])

#Implementation of the Genetic Algorithm
GA <- ga(type = "real-valued", fitness = function (x) {fun_SVM_4D(x[1],x[2],x[3],x[4],model$model)}, lower = c(0,0,0,0), upper = c(1,1,1,1), maxiter = 1000, run = 50)
SVM_value = 1 - fun_SVM_4D(GA@solution[1],GA@solution[2],GA@solution[3],GA@solution[4],model$model)
GA@solution


#Based on the information derived from the visual representation, the GA has to be adjusted

#Implementation of the Genetic Algorithm
#GA <- ga(type = "real-valued", fitness = function (x) {- fun_NN_4D(x[1],x[2],x[3],x[4])}, lower = c(0,0,0,0), upper = c(1,1,1,1), maxiter = 1000, run = 50)
GA <- ga(type = "real-valued", fitness = function (x) {fun_SVM_4D(x[1],x[2],x[3],x[4])}, lower = c(0,0,0,0), upper = c(1,1,1,1), maxiter = 1000, run = 50)


#Value predicted by the respective models
NN_value = fun_NN_4D(GA@solution[,1],GA@solution[,2],GA@solution[,3],GA@solution[,4])
SVM_value = 1 - fun_SVM_4D(GA@solution[,1],GA@solution[,2],GA@solution[,3],GA@solution[,4])

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

merke_dataset = c()
for (i in seq(0,1,by = 0.1)) {
  for (k in seq(0,1, by = 0.1)) {
    p = a[which(a$col1 == i & a$col2 == k),]
    tmp = p[order(p[,5], decreasing = FALSE),]
    merke_dataset = append(merke_dataset, which(a$r == tmp[order(tmp[,5], decreasing = FALSE), ][1,5]))
  } 
}

scatter3D(bty = "b2", x = a[merke_dataset,1], xlab = "col1", y = a[merke_dataset,2], ylab = "col2", z = a[merke_dataset,5], zlab = "r", main = "text = col3; color = col4", cex = 1, pch = 19, theta = 10, phi = 10, colvar = a[merke_dataset,4],ticktype = "detailed")
text3D(x= a[merke_dataset,1], y = a[merke_dataset,2], z = a[merke_dataset,5],  labels = round(a[merke_dataset,3],2),add = TRUE, colkey = FALSE, cex = 1)
plotrgl()



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

NN <- neuralNetwork4D(dataset);
predicted <- predictNNWOTEST4D(NN, getGridData2D(0,1,0,1,80))
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