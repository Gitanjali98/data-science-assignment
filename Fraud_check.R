install.packages("randomForest")
install.packages("caret")
library(randomForest)
library(caret)
fraud<-read.csv("C:/Users/india/Desktop/data science/assignments/random forest/Fraud_check.csv")
View(fraud)
str(fraud)
summary(fraud)
attach(fraud)
hist(fraud$Taxable.Income)
plot(fraud)
boxplot(fraud)
boxplot(fraud$Taxable.Income)
risksafe<-ifelse(fraud$Taxable.Income<=30000,"risk","safe")
fraud<-data.frame(risksafe,fraud)
View(fraud)
set.seed(123)
cut<-createDataPartition(risksafe,p=0.7,list=F)
train_fraud<-fraud[cut,]
test_fraud<-fraud[-cut,]
#random forest model
fit.fraud<-randomForest(risksafe~.,data=train_fraud,na.action=na.roughfix,importance=TRUE)
#Training accuracy 
mean(train_fraud$risksafe==predict(fit.fraud,train_fraud))
#Prediction of train data
pred_train<-predict(fit.fraud,train_fraud)
#Confusion Matrix
confusionMatrix(train_fraud$risksafe,pred_train)
#Predicting test data 
pred_test<-predict(fit.fraud,newdata=test_fraud)
mean(pred_test==test_fraud$risksafe) 
#Confusion Matrix 
confusionMatrix(test_fraud$risksafe, pred_test)
#importance
importance(fit.fraud)
varImpPlot(fit.fraud)
#plot
plot(fit.fraud,lwd=2)
legend("topright", colnames(fit.fraud$err.rate),col=1:4,cex=0.8,fill=1:4)


