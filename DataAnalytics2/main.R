install.packages('httr');
install.packages('xml2');
require('httr');
require('xml2');

url = 'http://optim.uni-muenster.de:5000/';
operation = 'api-test2D/'
data = '0.4,0.1;0.06,0.2'
test = TRUE;

#run getRequest() and insert the provided token to do the GET request. Outputs an xml2-object.
getRequest<- function(){
  token <- readline(prompt="Token: ")
  getUrl <- paste(url,operation,token,'/',data, sep="");
  if(grepl('test',getUrl) || (!test)){
    r <- GET(getUrl);
    return (r);
  } else{
    print('Not in production mode yet');
    return (NULL);
  }
};

#Turns the response object of the get-request into a data frame
getData<- function(){
  #TODO: Figure out how to turn an xml2-object into a data frame
  data <- content(r)
}