install.packages("")
library(readxl)
cust<-read.csv("C:/Users/india/Desktop/data science/assignments/hypothesis testing/Costomer+OrderForm.csv")  
View(cust) 
attach(cust)
colnames(cust)
nrow(cust)
cust1=as.data.frame(cbind(c(Phillippines,Indonesia,Malta,India),rep(c("Phillippines","Indonesia","Malta","India"),c(300,300,300,300))))
View(cust1)
colnames(cust1)=c("Type","Place")
cust1
chisq.test(cust1$Type,cust1$Place)
plot(cust1$Place,cust1$Type,type="h")
#p-valu:0.2771>0.05  Therefore Null hypothesis is accepted