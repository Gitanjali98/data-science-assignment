install.packages("neuralnet")
install.packages("nnet")
install.packages("NeuralNetTools")
library(NeuralNetTools)
library(neuralnet)
library(nnet)
library(ggplot2)
forest<-read.csv("C:/Users/india/Desktop/data science/assignments/neural network/forestfires.csv")
View(forest)
str(forest)
table(forest$size_category)
ggplot(data = forest)+geom_bar(aes(x=size_category))
#making the entire dataset numeric
forest$month=as.integer(factor(forest$month,levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),labels = c(1,2,3,4,5,6,7,8,9,10,11,12)))
forest$day=as.integer(factor(forest$day,levels = c("sun","mon","tue","wed","thu","fri","sat"),labels = c(1,2,3,4,5,6,7)))
forest$size_category=as.integer(factor(forest$size_category,levels = c("large","small"),labels = c(1,0)))
View(forest)
str(forest)
summary(forest)
#normalization
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
forest_n<-as.data.frame(lapply(forest,normalize))
View(forest_n)
str(forest_n)
summary(forest_n)
#train and test
train_forest<-forest_n[1:417,]
test_forest<-forest_n[418:517,]
#neural netwrok model
colnames(forest_n)
forest_nn<-neuralnet(formula=size_category~+month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain+area+dayfri+daymon+daysat+daysun+daythu+daytue+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+monthsep,data=train_forest)
str(forest_nn)
plot(forest_nn,rep = "best")
set.seed(123)
results_forest<-compute(forest_nn,test_forest)
str(results_forest)
pred<-results_forest$net.result
cor(pred,test_forest$size_category)
#find no of hidden layers needed
acc <- c()
for(i in seq(1,10,1)){
  set.seed(100)
  model_bag <- neuralnet(size_category~.,hidden = i,data =train_forest)
  pred_bag <- compute(model_bag,test_forest)
  acc <- c(acc,cor(pred_bag$net.result,test_forest$size_category))
}
acc
plot(seq(1,10,1),acc)
#adding hidden layer
forest1_nn<-neuralnet(formula=size_category~.,data = train_forest,hidden = 1)
str(forest1_nn)
plot.(forest1_nn,rep = "best")
results_forest1<-compute(forest1_nn,test_forest)
str(results_forest1)
pred1<-results_forest1$net.result
cor(pred1,test_forest$size_category)






