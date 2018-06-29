install.packages('httr');
install.packages('xml2');
install.packages('XML');
install.packages('stringr');
install.packages('neuralnet');
install.packages("rpart");
install.packages("plotly");
require('plotly');
require('httr');
require('xml2');
require('XML');
require('stringr');
require('neuralnet');
require('rpart');
source("support_vector_machines.R")

url = 'http://optim.uni-muenster.de:5000/';
operation = 'api-test2D/'
test = TRUE;
#token
token = '5d8096530da349e98ca4cc65b519daf7';

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
    substring <- str_extract_all(substring, "[0-9].[0-9]*", simplify = TRUE)
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

getGridData <- function(distance,dimensions){
  d <- seq(0,1,distance);
  if(dimensions == 2){
    g <- expand.grid(d,d);
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

scalingData <- function(data){
  max = apply(data , 2 , max);
  min = apply(data, 2 , min);
  scaled = as.data.frame(scale(data, center = min, scale = max - min));
  return (scaled);
}

neuralNetwork <- function(data){
  set.seed(2);
  NN = neuralnet(r ~ col1 + col2, data, hidden = c(3,3,3,3), linear.output= T);
  return(NN);
}

#plots predicted values vs test values and outputs rmse
predictNN <- function(NN, testData, data){
  predict_testNN = compute(NN, testData[,c(1:(ncol(data)-1))])
  predict_testNN = (predict_testNN$net.result * (max(data[,'r']) - min(data[,'r']))) + min(data[,'r'])
  
  plot(testData[,'r'], predict_testNN, col='blue', pch=16, ylab = "predicted r NN", xlab = "real r")
  
  abline(0,1)
  
  return((sum((testData[,'r'] - predict_testNN)^2) / nrow(testData)) ^ 0.5)
}

#execute:
dataset <- getData(getRandomData(1000,2),token);
dataset <- getData(getGridData(0.05,2),token);
index <- splitData(dataset, 0.90);
train <- scalingData(dataset[index,]);
test <- scalingData(dataset[-index,]);
NN <- neuralNetwork(train);
plot(NN)
predictNN(NN, test, dataset)

#visualization, currently hard coded
plotdata <- dataset[,3];
dim(plotdata) <- c(length(c(seq(0,1,0.05))),length(c(seq(0,1,0.05))));
rownames(plotdata) <-  c(seq(0,1,0.05));
colnames(plotdata) <-  c(seq(0,1,0.05));
plot_ly(x = rownames(plotdata), y = colnames(plotdata), z=plotdata, type="surface")