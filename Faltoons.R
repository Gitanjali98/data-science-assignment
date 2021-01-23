install.packages("")
library(readxl)
faltoons<-read.csv("C:/Users/india/Desktop/data science/assignments/hypothesis testing/Faltoons.csv")  
View(faltoons) 
attach(faltoons)
colnames(faltoons)
nrow(faltoons)
faltoons1=as.data.frame(cbind(c(Weekdays,Weekend),rep(c("Weekdays","Weekend"),c(400,400))))
View(faltoons1)
colnames(faltoons1)=c("Weekdays","Weekends")
faltoons1
chisq.test(faltoons1$Weekdays,faltoons1$Weekends)
#p-value:8.543e-05 < 0.05    Therefore Null hypothesis is REJECTED