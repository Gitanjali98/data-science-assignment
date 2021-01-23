install.packages("class")
library(ggplot2)
library(class)
glass<-read.csv("C:/Users/india/Desktop/data science/assignments/knn/glass.csv")
View(glass)
str(glass)
table(glass$Type)
round(prop.table(table(glass$Type))*100,1)
summary(glass[c("Na","Mg","Al","Si","K","Ca","Ba","Fe")])
plot(glass)
boxplot(glass)
plot(glass$Type)
plot(glass$Type,type = "h")
#normalize
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
norm_glass<-as.data.frame(lapply(glass[1:9], norm))
summary(norm_glass[c("Na","Mg","Al","Si","K","Ca","Ba","Fe")])
#test and train
glass_n_train<-norm_glass[1:114,]
glass_n_test<-norm_glass[115:214,]
glass_train_label<-glass[1:114,10]
glass_train_label
glass_test_label<-glass[115:214,10]
glass_test_label
#knn model
test_acc <- NULL
train_acc <- NULL
for (i in seq(3,200,2))
{
train_glass_pred <- knn(train=glass_n_train,test=glass_n_train,cl=glass_train_label,k=i)
train_acc <- c(train_acc,mean(train_glass_pred==glass_train_label))
test_glass_pred <- knn(train = glass_n_train, test=glass_n_test, cl=glass_train_label,k=i)
test_acc <- c(test_acc,mean(test_glass_pred==glass_test_label))
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

test_glass_pred <- knn(train = glass_n_train, test=glass_n_test, cl=glass_train_label,k=20)
#crosstable
install.packages("gmodels")
library(gmodels)
CrossTable(x=glass_test_label,y=test_glass_pred,prop.chisq = FALSE)

