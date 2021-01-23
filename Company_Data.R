install.packages("randomForest")
install.packages("caret")
library(randomForest)
library(caret)
company<-read.csv("C:/Users/india/Desktop/data science/assignments/random forest/Company_Data.csv")
View(company)
str(company)
summary(company)
hist(company$Sales)
plot(company)
boxplot(company)
boxplot(company$Sales)
sales<-ifelse(company$Sales>8.5,"high","low")
company<-data.frame(sales,company[,-1])
View(company)
set.seed(123)
cut<-createDataPartition(sales,p=0.7,list=F)
train_company<-company[cut,]
test_company<-company[-cut,]
#random forest model
fit.company<-randomForest(sales~.,data=train_company, na.action=na.roughfix,importance=TRUE)
#Training accuracy 
mean(train_company$sales==predict(fit.company,train_company))
#Prediction of train data
pred_train<-predict(fit.company,train_company)
#Confusion Matrix
confusionMatrix(train_company$sales,pred_train)
#Predicting test data 
pred_test<-predict(fit.company,newdata=test_company)
mean(pred_test==test_company$sales) 
#Confusion Matrix 
confusionMatrix(test_company$sales, pred_test)
#importance
importance(fit.company)
varImpPlot(fit.company)
#plot
plot(fit.company,lwd=2)
legend("topright", colnames(fit.company$err.rate),col=1:4,cex=0.8,fill=1:4)


