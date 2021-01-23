install.packages("kernlab")
install.packages("caret")
library(kernlab)
library(caret)
library(ggplot2)
forestfires<-read.csv("C:/Users/india/Desktop/data science/assignments/svm/forestfires.csv")
View(forestfires)
forestfires$month=as.integer(factor(forestfires$month,levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),labels = c(1,2,3,4,5,6,7,8,9,10,11,12)))
forestfires$day=as.integer(factor(forestfires$day,levels = c("sun","mon","tue","wed","thu","fri","sat"),labels = c(1,2,3,4,5,6,7)))
forestfires$size_category=as.integer(factor(forestfires$size_category,levels = c("large","small"),labels = c(1,0)))
View(forestfires)
str(forestfires)
plot(forestfires$size_category)
boxplot(forestfires)
boxplot(forestfires$size_category)
table(forestfires$size_category)
summary(forestfires)
#normalization
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
forest_n<-as.data.frame(lapply(forestfires,normalize))
View(forest_n)
str(forest_n)
summary(forest_n)
ggplot(data=forest_n)+geom_bar(mapping = aes(x=forest_n$size_category))
ggplot(data=forest_n)+geom_histogram(mapping = aes(x=forest_n$size_category))
#train and test data
train_forest<-forest_n[1:417,]
test_forest<-forest_n[418:517,]
#SVM model
arr<-c("rbfdot","polydot","tanhdot","vanilladot","laplacedot","besseldot","anovadot","splinedot","stringdot")
accuracy<-list()
pred_inf<-list()
table_inf<-list()
for(i in arr){
  model<-ksvm(size_category~.,data=train_forest,arr=i)
  pred<-predict(model,test_forest)
  pred_inf[[i]]<-pred
  accuracy[[i]]<-(cor(pred,test_forest$size_category))
  table_inf<-table(pred,test_forest$size_category)
  
}
accuracy
pred_inf
table_inf
#vanilladot has the highest accuracy hence it is used as the kernel

