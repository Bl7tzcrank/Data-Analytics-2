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
data = '0.4,0.1;0.06,0.2;0.07,0.2' #you are providing one or more points here
test = TRUE;
token = '5d8096530da349e98ca4cc65b519daf7';

#run getRequest() and insert the provided token to do the GET request. Outputs an xml2-object.
getRequest<- function(d, token){
  #token <- readline(prompt="Token: ")
  getUrl <- paste(url,operation,token,'/',d, sep="");
  if(grepl('test',getUrl) || (!test)){
    r <- GET(getUrl);
    return (r);
  } else{
    print('Not in production mode yet');
    return (NULL);
  }
};

#Turns the response object of the get-request into a data frame
getData<- function(d, token){
  request <- getRequest(d, token);
  #TODO: Figure out how to turn an xml2-object into a data frame
  html <- content(request, "text");
  substring <- sub(".*\\[", "", html);
  substring <- sub("\\].*", "", substring);
  substring <- str_extract_all(substring, "[0-9].[0-9]*", simplify = TRUE)
  return (as.numeric(substring));
  #remark: run print(getData(data, token), digits=20) to see that the result is NOT rounded
}

#execute: getData(data, token)
