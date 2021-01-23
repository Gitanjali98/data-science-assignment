install.packages("tseries")
install.packages("forecast")
library(readr)
library(ggplot2)
library(tseries)
library(forecast)
setwd("C:/Users/india/Desktop/data science/assignments/forecast")
coca<-read.csv("C:/Users/india/Desktop/data science/assignments/forecast/CocaCola_Sales.csv")
View(coca)
str(coca)
plot(coca)
boxplot(coca)
hist(coca$Sales)
summary(coca)
#dummy variables
Q1 <-  ifelse(grepl("Q1",coca$Quarter),'1','0') 
Q2 <-  ifelse(grepl("Q2",coca$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",coca$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",coca$Quarter),'1','0')
coca1<-cbind(coca,Q1,Q2,Q3,Q4)
View(coca1)
colnames(coca1)
coca1["t"]<-1:42 
coca1["log_Sales"]<-log(coca1["Sales"]) 
coca1["t_square"]<-coca1["t"]*coca1["t"] 
View(coca1)
attach(coca1)
#test and train
train<-coca1[1:32,]
test<-coca1[33:42,]
#MODELS
#Linear Model
linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear

#Exponential
expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
View(expo_model)
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

#Quadratic
Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 

#Additive Seasonality 
sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 

#Additive Seasonality with Linear
Add_sea_Linear_model<-lm(Sales~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear 

#Additive Seasonality with Quadratic 
Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

#Multiplicative Seasonality 
multi_sea_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea 

#Multiplicative Seasonality Linear trend 
multi_add_sea_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

#selection of best model
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
write.csv(coca1,file="coca1.csv",col.names = F,row.names = F)

#Additive Seasonality with Quadratic has the least RMSE value
model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(model)

#arima model
cococola3<-as.ts(coca$Sales)
cococola_ts<-ts(data =cococola3,start = c(1986,1),end = c(1996,2),frequency = 4)
class(cococola_ts)
cycle(cococola_ts)
summary(cococola_ts)
decomp <- decompose(cococola_ts,"multiplicative")
plot(decomp)
boxplot(cococola_ts~cycle(cococola_ts))
# auto arima model
arimamodel <- auto.arima(cococola_ts,ic="aic",trace = T)
plot.ts(arimamodel$residuals)
#verifying p,d,q
acf(arimamodel$residuals)
acf(diff(arimamodel$residuals))
pacf(arimamodel$residuals)
#forecasting
forecastmodel <- forecast(arimamodel,level = c(95),h=10*4)
plot(forecastmodel)
#prediction
pred_new<-data.frame(predict(model,newdata=test,interval = 'predict'))
View(pred_new)
