install.packages("Amelia")
library(Amelia)
bank <- read.csv("C:/Users/india/Desktop/data science/assignments/logistic regression/bank-full.csv")
View(bank)
missmap(bank)
str(bank)
df <- as.data.frame(bank)
df1 <- as.character(df[,])
bank1 <- strsplit(df1,split =";")
bank1
bank2 <- matrix(unlist(bank1),nrow=45211,byrow = T)
View(bank2)
colnames(bank2) <- c("age","job","marital","education","default","balance","housing","loan","contact","day","month","duration","campaign","pdays","previous","poutcome","y")
View(bank2)
bank3 <- as.data.frame(bank2)
str(bank3)
summary(bank3)
attach(bank3)
View(bank3)
sum(is.na(bank3))
nrow(bank3[is.na(bank3)])
bank3[is.na(bank3)]
bank3 <- na.omit(bank3) 
dim(bank3)

bank3=subset(bank3,select = c())
pairs(bank3)
bank3
cor(bank3)

mod_lm <- glm(bank3$y~.,data = bank3)
pred1 <- predict(mod_lm,bank3)
pred1

prd <- predict(model,type=c("response"),test,family="binomial")
View(prd)
confusion <- table(prd >0.5,bank3$y)
confusion

accuracy <- sum(diag(confusion))/sum(confusion)
accuracy

install.packages("ROCR") 
library(ROCR)
rocrpred<-prediction(prd,y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained
