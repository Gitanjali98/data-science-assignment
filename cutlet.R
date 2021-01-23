install.packages("")
library(readxl)
cutlet<- read.csv("C:/Users/india/Desktop/data science/assignments/hypothesis testing/Cutlets.csv")
cutlet
attach(cutlet)
#normality test
shapiro.test(Unit.A)
shapiro.test(Unit.B)
#variance test
var.test(Unit.A,Unit.B)
#2 sample t test
t.test(Unit.A,Unit.B,alternative = "two.sided",conf.level = 0.95,correct=TRUE)
t.test(Unit.A,Unit.B,alternative = "greater",var.equal = T)
boxplot((cutlet$Unit.A),(cutlet$Unit.B))
#inference: p-value>0.05.There is no significant difference in diameterof the cutlet between unit A and unit B

