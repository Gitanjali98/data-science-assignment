library(readr)
library(ggplot2)
setwd("C:/Users/india/Desktop/data science/assignments/forecast")
plastic<-read.csv("C:/Users/india/Desktop/data science/assignments/forecast/PlasticSales.csv")
View(plastic)
str(plastic)
plot(plastic)
boxplot(plastic)
hist(plastic$Sales)
summary(plastic)
#dummy variables
X<- data.frame(outer(rep(month.abb,length=60), month.abb,"==") + 0 )
View(X)
colnames(X)<-month.abb 
View(X)
plastic1<-cbind(plastic,X)
View(plastic1)
colnames(plastic1)
plastic1["t"]<- 1:60
View(plastic1)
plastic1["log_sales"]<-log(plastic1["Sales"])
plastic1["t_square"]<-plastic1["t"]*plastic1["t"]
View(plastic1)
attach(plastic1)
#test and train
train<-plastic1[1:40,]
test<-plastic1[41:60,]
#MODELS
#Linear Model
linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear

#Exponential
expo_model<-lm(log_sales~t,data=train)
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
sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add 

#Additive Seasonality with Linear
Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear 

#Additive Seasonality with Quadratic 
Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

#Multiplicative Seasonality 
multi_sea_model<-lm(log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea 

#Multiplicative Seasonality Linear trend 
multi_add_sea_model<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

#selection of best model
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
write.csv(plastic1,file="plastic1.csv",col.names = F,row.names = F)

#Multiplicative Seasonality Linear trend has the least RMSE value
model<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=plastic1)
summary(model)

#arima model
plastic_ts<-as.ts(plastic$Sales)
plastic_ts<-ts(plastic_ts,start = c(1949,1),end = c(1953,12),frequency = 12)
class(plastic_ts)
start(plastic_ts)
end(plastic_ts)
sum(is.na(plastic_ts))
summary(plastic_ts)
decompdata<-decompose(plastic_ts,"multiplicative")
plot(decompdata)
cycle(plastic_ts)
boxplot(plastic_ts~cycle(plastic_ts))
newmodel<-auto.arima(plastic_ts,ic = "aic",trace = T)
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
