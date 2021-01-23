startup<-read.csv("C:/Users/india/Desktop/data science/assignments/multi linear regression/50_Startups.csv")
startup
attach(startup)
install.packages("lattice")
library(lattice)

qqnorm(R.D.Spend)
qqline(R.D.Spend)
qqnorm(Administration)
qqline(Administration)
qqnorm(Marketing.Spend)
qqline(Marketing.Spend)
qqnorm(Profit)
qqline(Profit)

summary(startup)

plot(R.D.Spend,Profit)
plot(Administration,Profit)
plot(Marketing.Spend,Profit)

startup=subset(startup,select = c(R.D.Spend,Administration,Marketing.Spend,Profit))
pairs(startup)
startup
cor(startup)

model_startup<-lm(Profit~R.D.Spend+Administration+Marketing.Spend)
summary(model_startup)

model_startupA<-lm(Profit~Administration)
summary(model_startupA)

model_startupM<-lm(Profit~Marketing.Spend)
summary(model_startupM)

model_startupAM<-lm(Profit~Administration+Marketing.Spend)
summary(model_startupAM)

panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(startup,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")

install.packages("corpcor")
library(corpcor)
cor(startup)
cor2pcor(cor(startup))
plot(model_startup)

model_startup1<-lm(Profit~R.D.Spend+Marketing.Spend)
summary(model_startup1)
plot(model_startup1)

r2degree <- lm(Profit ~ R.D.Spend+I(R.D.Spend*R.D.Spend)+Marketing.Spend+I(Marketing.Spend*Marketing.Spend))
summary(r2degree)

#model_startup1 is accurate as r^2 is greater than all

