del <- read.csv("C:/Users/india/Desktop/data science/assignments/simple linear regression/delivery_time.csv")
del
summary(del)
plot(del$Delivery.Time,del$Sorting.Time)
attach(del)
install.packages("lattice")
library(lattice)
dotplot(del$Delivery.Time,main="Dot plot of Delivery time")
dotplot(del$Sorting.Time,main="Dot plot of Sorting time")

boxplot(del$Delivery.Time,col="red",main="Box plot of Delivery time")
boxplot(del$Sorting.Time,col="blue",main="Box plot of Sort time")

hist(del$Delivery.Time)
hist(del$Sorting.Time)

qqnorm(del$Delivery.Time)
qqline(del$Delivery.Time)

qqnorm(del$Sorting.Time)
qqline(del$Sorting.Time)

hist(del$Delivery.Time,prob=TRUE)
lines(density(del$Delivery.Time))
lines(density(del$Delivery.Time,adjust = 2),lty="dotted")

hist(del$Sorting.Time,prob=TRUE)
lines(density(del$Sorting.Time))
lines(density(del$Sorting.Time,adjust = 2),lty="dotted")

plot(del$Delivery.Time,del$Sorting.Time,main = "Scatter Plot",col="red",xlab = "Delivery time",ylab = "Sorting time",pch=20)

cor(Delivery.Time,Sorting.Time)
reg<-lm(Sorting.Time~Delivery.Time)
summary(reg)
confint(reg,level = 0.95)
pred<-predict(reg,interval = "predict")
pred<-as.data.frame(pred)
pred
cor(pred$fit,del$Sorting.Time)
plot(sqrt(Delivery.Time),Sorting.Time)
reg_sqrt<-lm(Sorting.Time~sqrt(Delivery.Time),data=del)
summary(reg_sqrt)
confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval = "predict")
plot(log(Delivery.Time),Sorting.Time)
reg_log<-lm(Sorting.Time~log(Delivery.Time))
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval = "predict")

#best model is reg_log
