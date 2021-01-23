install.packages("class")
library(class)
zoo<-read.csv("C:/Users/india/Desktop/data science/assignments/knn/Zoo.csv")
View(zoo)
str(zoo)
zoo<-zoo[-1]
View(zoo)
table(zoo$type)
round(prop.table(table(zoo$type))*100,1)
summary(zoo[-17])
#normalization not needed
#test and train
zoo_train<-zoo[1:51,]
zoo_test<-zoo[52:101,]
zoo_train_label<-zoo[1:51,17]
zoo_train_label
zoo_test_label<-zoo[52:101,17]
zoo_test_label
#knn model
test_zoo_pred <- knn(train=zoo_train,test=zoo_test,cl=zoo_train_label,k=4)
table(test_zoo_pred,zoo_test_label)
mean(test_zoo_pred==zoo_test_label)

test_acc <- NULL
train_acc <- NULL
for (i in seq(3,200,2))
{
  train_zoo_pred <- knn(train=zoo_train,test=zoo_train,cl=zoo_train_label,k=i)
  train_acc <- c(train_acc,mean(train_zoo_pred==zoo_train_label))
  test_zoo_pred <- knn(train = zoo_train, test=zoo_test, cl=zoo_train_label,k=i)
  test_acc <- c(test_acc,mean(test_zoo_pred==zoo_test_label))
}
#plots
install.packages("ggplot2")
library(ggplot2)
par(mfrow=c(1,2)) 
plot(seq(3,200,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test_acc,type="l",main="Test_accuracy",col="red")
acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,20,2)))
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

test_zoo_pred <- knn(train = zoo_train, test=zoo_test, cl=zoo_train_label,k=25)
#crosstable
install.packages("gmodels")
library(gmodels)
CrossTable(x=zoo_test_label,y=test_zoo_pred,prop.chisq = FALSE)
