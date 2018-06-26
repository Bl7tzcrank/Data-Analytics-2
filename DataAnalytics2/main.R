install.packages('httr');
install.packages('xml2');
install.packages('XML');
install.packages('stringr');
require('httr');
require('xml2');
require('XML');
require('stringr');

url = 'http://optim.uni-muenster.de:5000/';
operation = 'api-test2D/'
#data: Provide one or more points
x <- c(0.4,0.06,0.07,0.09);
y <- c(0.1,0.2,0.2,0.01);
data <- cbind(x,y);
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
#remark: run print(getData(data, token), digits=20) to see that the result is NOT rounded
getData<- function(data, token){
  dataAsString <- '';
  for(row in 1:nrow(data)){
    for(col in 1:ncol(data)){
      dataAsString <- paste(dataAsString, as.character(data[row,col]), sep='');
      if(col < ncol(data)){
        dataAsString <- paste(dataAsString, ",", sep='');
      }
    }
    if(row < nrow(data)){
      dataAsString <- paste(dataAsString, ";", sep='');
    }
  }
  request <- getRequest(dataAsString, token);
  html <- content(request, "text");
  substring <- sub(".*\\[", "", html);
  substring <- sub("\\].*", "", substring);
  substring <- str_extract_all(substring, "[0-9].[0-9]*", simplify = TRUE)
  return (cbind(data,r=as.numeric(substring)));
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
  colnames(c) <- c(1:dimensions)
  return (c);
}

#splits the data into trainings and test data. Returns index that can be used to access training and test data from the dataset
#percentage % will be used for training
splitData <- function(data, percentage){
  samplesize = percentage * nrow(data);
  set.seed(80);
  index = sample( seq_len ( nrow ( data ) ), size = samplesize );
  return (index);
}

#execute:
dataset <- getData(getRandomData(50,2),token);
index <- splitData(dataset, 0.60)
train = dataset[index,]
test = dataset[-index,]
