#applying Random Forest

library(ksvm)
#rfNews()
file <- read.csv(file.choose(),sep = ",",header = TRUE)
View(file)

file_test<-read.csv(file.choose(),sep=",",header=T)
View(file_test)
#View(file)
table(file$Class)/nrow(file)


file_sample <- sample(2, 
                       nrow(file),
                       replace = T,
                       prob = c(0.6,0.4))
file_sample
table(file_sample)

class(file$Class)
class(file)
table(file$Class)/nrow(file)

train_data <-df[file_sample==1,]
test_data <- df[file_sample==2,]
str(train_data)


str(test_data)

class(train_data$Class)
class(test_data)

table(file$Class)/nrow(file)

table(file_test$Class)/nrow(file_test)

rf <- randomForest(Class ~ .,data = file ,ntree=100,importance= TRUE)
dim(file_test)
rf
plot(rf)
summary(rf)
#View(test)
dim(file)
dim(file_test)
testMH <- read.csv(file.choose(),sep = ",",header = TRUE)
summary(predict(rf,testMH))


varImpPlot(rf,sort = T,main = "variable importance plot")
#summary(varNames)
#varNames <- varNames[!varNames %in% c("class")]
#str(varNames)
#dim(varNames)
#varNames[-6]

#varNames1 <- paste(varNames, collapse = "+")
#varNames1


#rf.form <- as.formula(paste("class", varNames1, sep = " ~ "))
#rf.form

#variable importance plot
varImpPlot(rf,sort = T)
rf_importance_plot

# varable importance table
rf_importance_table <- data.frame(importance(rf))
rf_importance_table

#file1 <- file[,-6]
#names(file)
#str(rf)

file_test1 <- file_test[]
names(file_test1)
library(caret)
library(e1071)

rf$predicted <- predict(rf,file_test)
dim(file_test)
dim(rf)
rf$predicted
summary(rf$predicted)
str(file$Class)
str(file_test1$predicted)
file$Class


confusionMatrix(data =rf$predicted,reference = file$Class)
                

