install.packages("randomForest")
install.packages("caret")
library(randomForest)
library(caret)
library(ggplot2)
data(iris)
View(iris)
str(iris)
summary(iris)
plot(iris)
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)
boxplot(iris[-5])
#Splitting data into training and testing. 
#splitting the data based on species 
iris_setosa<-iris[iris$Species=="setosa",] 
iris_versicolor <- iris[iris$Species=="versicolor",] 
iris_virginica <- iris[iris$Species=="virginica",] 
boxplot(iris_setosa[-5])
boxplot(iris_versicolor[-5])
boxplot(iris_virginica[-5])
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])
#random forest model
fit.forest <- randomForest(Species~.,data=iris_train, na.action=na.roughfix,importance=TRUE)
#Training accuracy 
mean(iris_train$Species==predict(fit.forest,iris_train))
#Prediction of train data
pred_train <- predict(fit.forest,iris_train)
#Confusion Matrix
confusionMatrix(iris_train$Species, pred_train)
#Predicting test data 
pred_test <- predict(fit.forest,newdata=iris_test)
mean(pred_test==iris_test$Species) 
#Confusion Matrix 
confusionMatrix(iris_test$Species, pred_test)
#importance
importance(fit.forest)
varImpPlot(fit.forest)
#plot
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)


