sal <- read.csv("C:/Users/india/Desktop/data science/assignments/simple linear regression/Salary_Data.csv")
sal
summary(sal)
plot(sal$YearsExperience,sal$Salary)
attach(sal)
install.packages("lattice")
library(lattice)
dotplot(sal$YearsExperience,main="Dot plot of Years of Experince")
dotplot(sal$Salary,main="Dot plot of Salary")

boxplot(sal$YearsExperience,col="red",main="Box plot of YoE")
boxplot(sal$Salary,col="blue",main="Box plot of Salary")

hist(sal$YearsExperience)
hist(sal$Salary)

qqnorm(sal$YearsExperience)
qqline(sal$YearsExperience)

qqnorm(sal$Salary)
qqline(sal$Salary)

hist(sal$YearsExperience,prob=TRUE)
lines(density(sal$YearsExperience))
lines(density(sal$YearsExperience,adjust = 2),lty="dotted")

hist(sal$Salary,prob=TRUE)
lines(density(sal$Salary))
lines(density(sal$Salary,adjust = 2),lty="dotted")

plot(sal$YearsExperience,sal$Salary,main = "Scatter Plot",col="red",xlab = "YoE",ylab = "Salary",pch=20)

cor(sal$YearsExperience,sal$Salary)
reg<-lm(sal$Salary~sal$YearsExperience)
summary(reg)
confint(reg,level = 0.95)
pred<-predict(reg,interval = "predict")
pred<-as.data.frame(pred)
pred
cor(pred$fit,sal$Salary)
plot(sqrt(sal$YearsExperience),sal$Salary)
reg_sqrt<-lm(sal$Salary~sqrt(sal$YearsExperience),data=del)
summary(reg_sqrt)
confint(reg_sqrt,level=0.95)
predict(reg_sqrt,interval = "predict")
plot(log(sal$YearsExperience),sal$Salary)
reg_log<-lm(sal$Salary~log(sal$YearsExperience))
summary(reg_log)
confint(reg_log,level=0.95)
predict(reg_log,interval = "predict")
 #best model is reg