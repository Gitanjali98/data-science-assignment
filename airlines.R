install.packages("tseries")
install.packages("forecast")
library(readr)
library(ggplot2)
library(tseries)
library(forecast)
setwd("C:/Users/india/Desktop/data science/assignments/forecast")
airline<-read.csv("C:/Users/india/Desktop/data science/assignments/forecast/airlines.csv")
View(airline)
str(airline)
plot(airline)
boxplot(airline)
hist(airline$Passengers)
summary(airline)
#dummy variables
X<- data.frame(outer(rep(month.abb,length=96), month.abb,"==") + 0 )
View(X)
colnames(X)<-month.abb 
View(X)
airline1<-cbind(airline,X)
View(airline1)
colnames(airline1)
airline1["t"]<- 1:96
View(airline1)
airline1["log_passengers"]<-log(airline1["Passengers"])
airline1["t_square"]<-airline1["t"]*airline1["t"]
View(airline1)
attach(airline1)
#test and train
train<-airline1[1:76,]
test<-airline1[77:96,]
#MODELS
#Linear Model
linear_model<-lm(Passengers~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear

#Exponential
expo_model<-lm(log_passengers~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
View(expo_model)
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

#Quadratic
Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 

#Additive Seasonality 
sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 

#Additive Seasonality with Linear
Add_sea_Linear_model<-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Passengers-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear 

#Additive Seasonality with Quadratic 
Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

#Multiplicative Seasonality 
multi_sea_model<-lm(log_passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea 

#Multiplicative Seasonality Linear trend 
multi_add_sea_model<-lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

#selection of best model
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
write.csv(airline1,file="airlines1.csv",col.names = F,row.names = F)

#Multiplicative Seasonality Linear trend has the least RMSE value
model<-lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=airline1)
summary(model)

#arima model
airlines_ts<-as.ts(airline$Passengers)
airlines_ts<-ts(airlines_ts,start = c(1995,1),end = c(2002,12),frequency = 12)
class(airlines_ts)
start(airlines_ts)
end(airlines_ts)
sum(is.na(airlines_ts))
summary(airlines_ts)
decompdata<-decompose(airlines_ts,"multiplicative")
plot(decompdata)
cycle(airlines_ts)
boxplot(airlines_ts~cycle(airlines_ts))
newmodel<-auto.arima(airlines_ts,ic = "aic",trace = T)
plot.ts(newmodel$residuals)
#verifying p,d,q values
acf(newmodel$residuals)                       
pacf(newmodel$residuals)                      
acf(diff(newmodel$residuals))                  
#forecasting
forecasting <- forecast(newmodel,level = c(95),h=10*12)
plot(forecasting) 
#prediction
pred_new<-data.frame(predict(model,newdata=test,interval = 'predict'))
View(pred_new)
