install.packages("rpart");
install.packages("xgboost");
require('rpart');
require('xgboost');

typeColNum <- grep("Class",names(train))
#this is only for classification trees: rpart_model <- rpart(class~.,data=train,method="class") 
## rpart_model <- rpart(col1~.,data = train, method="anova")
rpart_model1 <- rpart(comp~col1 + col2, data=train, method="anova")
plot(rpart_model1);text(rpart_model1)