library(kernlab)
library(e1071)
library(caret)


#fit model


file <- read.csv(file.choose(),sep = ",",header = TRUE)
View(file)
class(file)
str(file)

file_train<-as.data.frame(file)
class(file_train)

str(file_train)

file_test<-read.csv(file.choose(),sep=",",header=T)
View(file_test)
str(file_test)

fit1<-ksvm(Class~.,data=file_train,kernel="vanilladot",C=500)

fit1

#fit1<-ksvm(train_iris,test_iris,data=iris,kernel="vanilladot",C=1)


#Summarize the fit

predictions$predicted<-predict(fit1,file_test,type="response")
predictions$predicted

summary(predictions$predicted)
#Summarize Accuracy

table(predictions$predicted,file_train$Class)



confusionMatrix(data =predictions$predicted,reference = file_test$Class)

## Svm Tuning 

obj1 <- tune.svm(Class~., data = file_train, gamma = 2^(-1:1), cost = 2^(2:4))  
#obj <- tune.svm(Class~., data = file_train, cost = 2^(2:10),kernel="linear")

summary(obj)

## Alternate Tuning -  Sampling Method 

obj <- tune(svm, Class~., data = file_train, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:1)),
            tunecontrol = tune.control(sampling = "fix")
)
summary(obj)

plot(obj1)
